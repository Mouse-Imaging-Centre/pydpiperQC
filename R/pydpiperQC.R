#'@importFrom magrittr %>%

#' @export
launch <- function(annotation = NULL,
                   consensus = NULL,
                   wd=getwd()) {

  .GlobalEnv$.annotation <- annotation
  .GlobalEnv$.consensus <- consensus
  .GlobalEnv$.wd <- wd

  on.exit(rm(list=c(.annotation, .consensus, .wd), envir=.GlobalEnv))

  shiny::runApp(
    appDir = system.file('appDir', package = 'pydpiperQC'),
    launch.browser = TRUE
  )
}
