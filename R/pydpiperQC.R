#'@importFrom magrittr %>%
#'@importFrom readr read_csv

#' @export
launch <- function(annotation = NULL,
                   consensus = NULL,
                   wd=getwd()) {

  .GlobalEnv$.annotation <- annotation
  if (!is.null(annotation))
    .GlobalEnv$.annotation_dirname <- annotation %>% dirname()
  .GlobalEnv$.consensus <- consensus
  .GlobalEnv$.wd <- wd

  on.exit(rm(list=c(".annotation", ".consensus", ".wd"), envir=.GlobalEnv))

  shiny::runApp(
    appDir = system.file('appDir', package = 'pydpiperQC'),
    launch.browser = TRUE
  )
}

#' @export
launch_example <- function(data_dir=NULL) {

  if (is.null(data_dir)){
    data_dir <- tempdir()
    download_example_data(data_dir)
  }
  launch(
    annotation = file.path(data_dir, "tidy_analysis.csv"),
    consensus = file.path(data_dir, "test_nlin/test-nlin-3.mnc"),
  )
}

# changed from "wget" to stop freakouts on mac
# this function largely borrowed from RMINC:::getRMINCTestData
download_example_data <- function(data_path, method = "libcurl") {

  downloadPath <- file.path(data_path, "pydpiperQC_data.tgz")

  dir.create(data_path, showWarnings = FALSE, recursive = TRUE)
  download.file("http://repo.mouseimaging.ca/repo/PydpiperQC_test-files/pydpiperQC_data.tgz",
                destfile = downloadPath,
                method = method)

  untar(downloadPath, exdir = data_path, compressed = "gzip")
}
