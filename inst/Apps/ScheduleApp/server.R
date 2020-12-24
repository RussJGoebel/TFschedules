library(tidyverse)
library(TFschedules)
library(DT)

server <- function(input, output) {

  GRS_table <- TFschedules::htmltxt_to_table_schedule("GRS.txt",by_day = TRUE)
  CAS_table <- TFschedules::htmltxt_to_table_schedule("CAS.txt",by_day = TRUE)

  # Schedule Input -------------------------------

  valid_times <- format(seq(lubridate::parse_date_time("12:00am", '%I:%M %p'),by = "5 min", length.out = 288), '%I:%M %p')
  valid_times <- ifelse(str_starts(valid_times,"0"),str_sub(valid_times,2),valid_times) # remove leading zeroes
  valid_times <- factor(valid_times, levels = valid_times) # in an rhandsontable, using factors generates dropdown options

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

    rhandsontable::rhandsontable(RV$df, width = 600, height = 300, search = F)
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
    table = GRS_table %>% group_by(`Class/Call`) %>%
      summarize(Day = str_c(unique(Day), collapse = ","), Start = Start, Stop = Stop, Bld = Bld, Room = Room, Notes = Notes)

    return(table)
  }
  )




  output$GRS <- renderDataTable({
    res <- GRS_RV()
    return(datatable(res))
  })


  CAS_RV <- reactive({
    table = CAS_table %>% group_by(`Class/Call`) %>%
      summarize(Day = str_c(unique(Day), collapse = ","), Start = Start, Stop = Stop, Bld = Bld, Room = Room, Notes = Notes)


    return(table)
  }
  )



  output$CAS <-  renderDataTable({
    res <- CAS_RV()
    return(datatable(res))
  })


  summary_CAS <- reactive({

    full_table <- rbind(CAS_table,GRS_table)

    jdata <- inner_join(RV$pivot,full_table, by = "Day")

    types <- unique(jdata$Type)
    types <- c(types[-1],types[1])

    # Identify schedule conflicts
    jdata <- jdata %>%mutate(
      nonoverlapping_meetings = ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE),
      overlapping_meetings = !ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE)
    ) %>% group_by(`Class/Call`,`Type`) %>%
      summarize(Day = str_c(unique(Day),collapse = ","),
                nonoverlapping_meetings = all(nonoverlapping_meetings),
                overlapping_meetings = any(overlapping_meetings))

    # Consolidate conflicts by type and class
    jdata <- jdata %>%
      mutate(`Class/Call` = str_replace(`Class/Call`,"( [A-Z][0-9])",TFschedules::trim_off_number)) %>% group_by(`Class/Call`,`Type`) %>%
      summarize(`Compatible Sections` = sum(nonoverlapping_meetings),
                `Total Sections` = sum(nonoverlapping_meetings)+sum(`overlapping_meetings`)) %>% arrange(`Compatible Sections`) %>%
      pivot_wider(names_from = Type,values_from = `Compatible Sections`:`Total Sections`) %>% replace(is.na(.),0)



    `Total Sections` <-  jdata %>% ungroup() %>% transmute(total = reduce(select(.,starts_with("Total")),`+`))
    `Compatible Sections` <- jdata %>% ungroup() %>% transmute(total = reduce(select(.,starts_with("Compatible")),`+`))

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

      jdata <- jdata %>% select(!remove1) %>% select(!remove2)

    }


    jdata

  })

  output$Summary <- DT::renderDataTable({d = summary_CAS(); DT::datatable(d)})


}


