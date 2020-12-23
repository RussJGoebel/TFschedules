library(tidyverse)


server <- function(input, output) {

  GRS_table <- TFschedules::htmltxt_to_table_schedule("../../Data/GRS.txt",by_day = TRUE)
  CAS_table <- TFschedules::htmltxt_to_table_schedule("../../Data/CAS.txt",by_day = TRUE)

  # Schedule Input -------------------------------

  valid_times <- format(seq(lubridate::parse_date_time("12:00am", '%I:%M %p'),by = "5 min", length.out = 288), '%I:%M %p')
  valid_times <- as.factor(valid_times)
  single_row <- data.frame(Mon = rep(FALSE,1),
                           Tue = rep(FALSE,1),
                           Wed = rep(FALSE,1),
                           Thu = rep(FALSE,1),
                           Fri = rep(FALSE,1),
                           Start = factor(valid_times[101:101], levels = valid_times),
                           Stop = factor(valid_times[101:101], levels = valid_times),
                           stringsAsFactors = FALSE)



  RV <- reactiveValues(df = single_row)

  observe({
    if (!is.null(input$hot)) {
      DF = rhandsontable::hot_to_r(input$hot)
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
    rhandsontable::rhandsontable(RV$df, width = 600, height = 300)
  })


  observeEvent(input$button,
               { RV$df = rbind(RV$df,single_row);
               rownames(RV$df) <- 1:dim(RV$df)[1]}
  )

  observeEvent(input$button2,
               { RV$df = RV$df[-c(dim(RV$df)[1]),] }
  )

  # Tables



  GRS_RV <- reactive({
    table = inner_join(RV$pivot,GRS_table, by = "Day") %>% group_by(`Class/Call`) %>% filter(
      all(ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE))) %>%
      group_by(`Class/Call`,`Title/Instructor`,Type,Start = Start.y,Stop = Stop.y) %>%
      summarize(Day = str_c(unique(Day), collapse = ",")) %>% group_by(`Title/Instructor`)

    return(table)
  }
  )




  output$GRS <- renderTable({
    res <- GRS_RV()
    return(res)
  })


  CAS_RV <- reactive({
    table = inner_join(RV$pivot,CAS_table, by = "Day") %>% group_by(`Class/Call`) %>% filter(
      all(ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE))) %>%
      group_by(`Class/Call`,`Title/Instructor`,Type,Start = Start.y,Stop = Stop.y) %>%
      summarize(Day = str_c(unique(Day), collapse = ",")) %>% group_by(`Title/Instructor`)

    return(table)
  }
  )



  output$CAS <- renderTable(CAS_RV())

  summary_CAS <- reactive({

    jdata <- inner_join(RV$pivot,CAS_table, by = "Day")

    jdata %>%mutate(
      nonoverlapping_meetings = ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE),
      overlapping_meetings = !ifelse(value,check_not_overlap(Start.x,Stop.x,Start.y,Stop.y),TRUE)
    ) %>% group_by(`Class/Call`) %>%
      summarize(Day = str_c(unique(Day),collapse = ","),
                nonoverlapping_meetings = all(nonoverlapping_meetings),
                overlapping_meetings = any(overlapping_meetings))%>%
      mutate(`Class/Call` = str_replace(`Class/Call`,"( [A-Z][0-9])",TFschedules::trim_off_number)) %>% group_by(`Class/Call`) %>%
      summarize(`Schedule Conflicts` = sum(overlapping_meetings),
                `Total Sections` = sum(nonoverlapping_meetings)+sum(`overlapping_meetings`)) %>% arrange(`Schedule Conflicts`)


  })

  output$Summary <- renderTable(summary_CAS())


}
