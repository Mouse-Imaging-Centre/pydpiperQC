#'@importFrom magrittr %>%

#' @export
launch_app <- function() {
  shiny::runApp(
    appDir = system.file('appDir', package = 'pydpiperQC'),
    launch.browser = TRUE
  )
}
