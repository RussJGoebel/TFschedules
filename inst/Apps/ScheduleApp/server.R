#library(tidyverse)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(rhandsontable)
library(stringr)
library(TFschedules)
library(magrittr)
library(DT)
library(purrr)

server <- function(input, output) {

  GRS_table <- TFschedules::htmltxt_to_table_schedule("GRS.txt",by_day = TRUE)
  CAS_table <- TFschedules::htmltxt_to_table_schedule("CAS.txt",by_day = TRUE)

  # Schedule Input -------------------------------

  trim_leading_0 <- function(x){
    ifelse(str_starts(x,"0"),str_sub(x,2),x)
  }

  valid_times <- format(seq(lubridate::parse_date_time("12:00am", '%I:%M %p'),by = "5 min", length.out = 288), '%I:%M %p')
  valid_times <- trim_leading_0(valid_times) # remove leading zeroes
  valid_times <- factor(valid_times, levels = valid_times) # in an rtable, using factors generates dropdown options

  single_row <- data.frame(Mon = rep(FALSE,1),
                           Tue = rep(FALSE,1),
                           Wed = rep(FALSE,1),
                           Thu = rep(FALSE,1),
                           Fri = rep(FALSE,1),
                           Start = valid_times[101:101],
                           Stop = valid_times[101:101],
                           stringsAsFactors = FALSE)





  RV <- reactiveValues(df = single_row)

  observe({
    if (!is.null(input$hot)) {
      DF <- rhandsontable::hot_to_r(input$hot)
    } else {
      if (is.null(RV$df))
        DF <- DF
      else
        DF <- RV$df
    }
    RV$df <- DF




    RV$pivot <- pivot_longer(DF, cols = Mon:Fri, names_to = "Day")
  })


  output$hot <- rhandsontable::renderRHandsontable({

      if(any(lubridate::parse_date_time(RV$df$Stop , '%I:%M %p') < lubridate::parse_date_time(RV$df$Start , '%I:%M %p'))){

        showNotification("Stop times should be no earlier than Start times.", type = "warning")


      }

    rhandsontable::rhandsontable(RV$df, width = 600,height = 150, search = F) %>%rhandsontable::hot_table(stretchH = "all")
  })


  observeEvent(input$button1,
               { RV$df = rbind(RV$df,single_row);
               rownames(RV$df) <- 1:dim(RV$df)[1]}
  )

  observeEvent(input$button2,
               { if(dim(RV$df)[1] > 1){RV$df = RV$df[-c(dim(RV$df)[1]),] } }
  )

  # Tables



  GRS_RV <- reactive({
    gdata <- inner_join(RV$pivot, GRS_table, by = "Day")

    gdata <- gdata %>%mutate(
      nonoverlapping_meetings = ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE),
      overlapping_meetings = !ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE)
    ) %>% group_by(`Class/Call`,`Title/Instructor`,Type,Start = Start.y,Stop = Stop.y) %>%
      summarize(Day = str_c(unique(Day),collapse = ","),
                Bld = Bld,
                Room = Room,
                `Compatible with Schedule` = ifelse(all(nonoverlapping_meetings),"Yes","No"),.groups = "drop")%>%
      select(`Class/Call`,`Title/Instructor`,Type,Day,Start,Stop,Bld,Room,`Compatible with Schedule`)


    return(gdata)
  }
  )




  output$GRS <- renderDataTable({
    res <- GRS_RV()
    compatible <- res$`Compatible with Schedule`
    col_to_format <- rep(TRUE,dim(res)[2])
    colors <- ifelse(compatible,"white","black")

    res$Start <- factor(trim_leading_0(res$Start), levels = valid_times)
    res$Stop <- factor(trim_leading_0(res$Stop), levels = valid_times)


    return(datatable(res) %>% formatStyle(columns = col_to_format,target = "row", backgroundColor = styleEqual(levels = c("Yes","No"),
                                                                                                               values = c("White","lightgray"))))
  })


  CAS_RV <- reactive({
    cdata <- inner_join(RV$pivot, CAS_table, by = "Day")

    # Identify schedule conflicts for individual days, and consolidate days by Class/Call
    cdata <- cdata %>%mutate(
      nonoverlapping_meetings = ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE),
      overlapping_meetings = !ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE)
    ) %>% group_by(`Class/Call`,`Title/Instructor`,Type,Start = Start.y,Stop = Stop.y) %>%
      summarize(Day = str_c(unique(Day),collapse = ","),
                Bld = Bld,
                Room = Room,
                `Compatible with Schedule` = ifelse(all(nonoverlapping_meetings),"Yes","No"),.groups = "drop")%>%
      select(`Class/Call`,`Title/Instructor`,Type,Day,Start,Stop,Bld,Room,`Compatible with Schedule`)

    return(cdata)
  }
  )



  output$CAS <-  renderDataTable({
    res <- CAS_RV()
    compatible <- res$`Compatible with Schedule`
    col_to_format <- rep(TRUE,dim(res)[2])
    colors <- ifelse(compatible,"white","black")

    res$Start <- factor(trim_leading_0(res$Start), levels = valid_times)
    res$Stop <- factor(trim_leading_0(res$Stop), levels = valid_times)

    return(datatable(res,
                     options = list(columnDefs=list(list(orderData=5,targets=5)))) %>%
             formatStyle(columns = col_to_format,target = "row", backgroundColor = styleEqual(levels = c("Yes","No"),
                                                                       values = c("White","lightgray"))))
  })


  summary_CAS <- reactive({

    full_table <- rbind(CAS_table,GRS_table)

    jdata <- inner_join(RV$pivot,full_table, by = "Day")

    # Obtain unique types, such as lecture, discussion, lab or independent
    types <- unique(jdata$Type)
    types <- c(types[-1],types[1])

    # Identify schedule conflicts for individual days, and consolidate days by Class/Call
    jdata <- jdata %>%mutate(
      nonoverlapping_meetings = ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE),
      overlapping_meetings = !ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE)
    ) %>% group_by(`Class/Call`,`Type`) %>%
      summarize(Day = str_c(unique(Day),collapse = ","),
                nonoverlapping_meetings = all(nonoverlapping_meetings),
                overlapping_meetings = any(overlapping_meetings),
                .groups = "drop")

    # Consolidate conflicts by type and class
    jdata <- jdata %>%
      mutate(`Class/Call` = str_replace(`Class/Call`,"( [A-Z][0-9])",TFschedules::trim_off_number)) %>% group_by(`Class/Call`,`Type`) %>%
      summarize(`Compatible Sections` = sum(nonoverlapping_meetings),
                `Total Sections` = sum(nonoverlapping_meetings)+sum(`overlapping_meetings`),
                .groups = "drop") %>% arrange(`Compatible Sections`) %>%
      pivot_wider(names_from = Type,values_from = `Compatible Sections`:`Total Sections`) %>% replace(is.na(.),0)



    `Total Sections` <-  jdata %>% ungroup() %>% transmute(total = purrr::reduce(select(.,starts_with("Total")),`+`))
    `Compatible Sections` <- jdata %>% ungroup() %>% transmute(total = purrr::reduce(select(.,starts_with("Compatible")),`+`))

    jdata$`Incompatible Sections` <- `Total Sections`$total-`Compatible Sections`$total #incompatible sections



    output_function <- function(a,b){
      str_c(a,b,sep = "/")
    }

    jdata$`Compatible Sections` <- tibble(`Compatible Sections`$total,`Total Sections`$total)%>% reduce(output_function)
    jdata <- jdata %>% arrange(`Incompatible Sections`)
    jdata <- jdata %>% select(!`Incompatible Sections`)

    for(i in seq_along(types)){

      compatible_type <- str_c("Compatible",types[i],"Sections",sep = " ", collapse = T)

      jdata[,compatible_type] <- jdata %>% ungroup() %>% transmute("{compatible_type}" := reduce(select(.,contains(types[i])),output_function))

      remove1 <- str_c("Compatible Sections",types[i],sep = "_", collapse = T)
      remove2 <- str_c("Total Sections",types[i],sep = "_", collapse = T)

      jdata <- jdata %>% select(!all_of(remove1)) %>% select(!all_of(remove2))

    }

    jdata

  })

  output$Summary <- DT::renderDataTable({d = summary_CAS();
  l <- unique(unlist(d[,-1]))
  s <- str_split(l,"/",simplify = T)
  r <- apply(s,1,function(x){as.numeric(x[2])-as.numeric(x[1])})
  r <- order(r)
  l <- l[r]

  d <- d %>% mutate_at(-1,function(x){factor(x, levels = l)})

  return(datatable(d))

  })


}


