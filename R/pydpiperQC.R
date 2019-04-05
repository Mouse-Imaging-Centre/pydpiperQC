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

#' @export
launch_example <- function(data_dir) {
  launch(
    annotation = file.path(data_dir, "tidy_analysis.csv"),
    consensus = file.path(data_dir, "example_nlin/example-nlin-3.mnc")
  )
}
