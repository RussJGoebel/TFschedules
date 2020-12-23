#' Read an html table stored in filename following a specific format to a tibble, with cleaning.
#'
#' Some classes have extra times for a given table row. These appear with an extra Day not separated by a comma.
#' For example, "Mon,Wed,FriThu" has an extra meeting time for Thursday. These entries also have two Start and Stop times.
#' This function separates this into two rows, e.g. one for "Mon,Wed,Fri" and one for "Thu".
#'
#'
#'
#'
#' @param filename A text file containing an html table following a specific format.
#' @param by_day If TRUE, every day is a unique row.
#'
#' @return
#'
#'
#' @import stringr
#' @import rvest
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
#' @examples
htmltxt_to_table_schedule <- function(filename,by_day = FALSE){

  # Read HTML table (copy pasted into a text document)
  file_rawHTML <- paste(readLines(filename), collapse = "\n")
  file_minHTML <- rvest::minimal_html("Title",file_rawHTML)
  file_table <- rvest::html_table(file_minHTML, fill = TRUE)[[1]]

  extraday_pattern <- c("(Mon,|Tue,|Wed,|Thu,|Fri,)+(Mon|Tue|Wed|Thu|Fri)(Mon|Tue|Wed|Thu|Fri)")
  regularschedule_pattern <- c("(Mon,|Tue,|Wed,|Thu,|Fri,)+(Mon|Tue|Wed|Thu|Fri)")

  file_table <- file_table %>% tidyr::uncount(stringr::str_detect(Day,extraday_pattern)+1) %>%
    mutate_at(vars(Start),
              ~case_when(`Class/Call` == lag(`Class/Call`) ~ word(Start,2),
                         `Class/Call` == lead(`Class/Call`)  ~ word(Start,1),
                         TRUE~.))%>%
    mutate_at(vars(Stop),
              ~case_when(`Class/Call` == lag(`Class/Call`) ~ word(Stop,2),
                         `Class/Call` == lead(`Class/Call`) ~ word(Stop,1),
                         TRUE~.)) %>%
    mutate_at(vars(Day),
              ~case_when(`Class/Call` == lag(`Class/Call`) ~ str_remove(Day,regularschedule_pattern),
                         `Class/Call` == lead(`Class/Call`) ~ str_extract(Day,regularschedule_pattern),
                         TRUE ~ .))
  if(by_day){
    file_table <- separate_rows(file_table,Day)
  }

  ### ------------------------------------------------------------------------------------------------

  # To compare starttimes and endtimes, we converte from the form e.g "2:30pm" to military time.

  file_table <- file_table %>% mutate(Start = format(lubridate::parse_date_time(Start, '%I:%M %p'), '%I:%M %p'),
                                      Stop  =  format(lubridate::parse_date_time(Stop, '%I:%M %p'), '%I:%M %p'))

  return(file_table)

}

# Function to determine if times overlap given start times and stoptimes
#' Check not overlap
#'
#' Checks for a schedule conflict given starttimes and endtimes.
#'
#' @param starttime1 A string or time of the form "H:M pm", e.g. "9:00 pm"
#' @param stoptime1 A string or time of the form "H:M pm", e.g. "9:00 pm"
#' @param starttime2 A string or time of the form "H:M pm", e.g. "9:00 pm"
#' @param stoptime2 A string or time of the form "H:M pm", e.g. "9:00 pm"
#'
#' @return
#' @export
#'
#' @examples
check_not_overlap <- function(starttime1,stoptime1,starttime2,stoptime2){

  x_start <- lubridate::parse_date_time(starttime1, '%I:%M %p')
  y_start <- lubridate::parse_date_time(starttime2, '%I:%M %p')
  x_stop <- lubridate::parse_date_time(stoptime1, '%I:%M %p')
  y_stop <- lubridate::parse_date_time(stoptime2, '%I:%M %p')


   (x_stop < y_start) | (x_start > y_stop )
}


#' Trim off last number
#' Remove the last number of a course title. Course titles have a form like "CAS MA415 A1", and we would like to remove any numbers.
#'
#' @param A1 A string that contains numbers to be removed.
#'
#' @return
#' @export
#'
#' @examples
trim_off_number <- function(A1){
  stringr::str_remove(A1,"[0-9]+")
}








