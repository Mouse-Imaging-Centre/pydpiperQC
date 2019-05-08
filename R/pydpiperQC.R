#'@importFrom magrittr %>%
NULL

#' Launch the pydpiperQC application in an external browser
#'
#' @param annotation A path to an annotation file.
#' @param consensus A path to a consensus MINC file.
#' @param wd A path to the working directory.
#'
#' @seealso \code{\link{launch_example}}
#'
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

#' Downloads example data and uses it to showcase the application by calling \code{\link{launch}}
#'
#' @param data_dir A path to the example data if it has been previously downloaded.
#' @param hpf Do you have access to HPF? If so, skip downloading and directly use the example data located at "/hpf/largeprojects/MICe/nwang/2019-04-05_pydpiperQC_data"
#'
#' @examples
#' launch_example(hpf=TRUE) #if you have a connection to HPF
#'
#' @seealso \code{\link{launch}}
#'
#' @export
launch_example <- function(data_dir = NULL, hpf = FALSE) {

  if (is.null(data_dir)){
    if (hpf==TRUE)
      data_dir <- "/hpf/largeprojects/MICe/nwang/2019-04-05_pydpiperQC_data"
    else {
      data_dir <- tempdir()
      download_example_data(data_dir)
    }
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
