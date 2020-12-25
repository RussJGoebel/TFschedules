#' Runs the TF schedule app
#'
#' @return
#' @export
#'
#' @examples
#'
#' #TFschedules::view_schedule()
#'
view_schedule <- function(){

    appDir <- system.file("Apps","ScheduleApp", package = "TFschedules")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
