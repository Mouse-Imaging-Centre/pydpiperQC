% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pydpiperQC.R
\name{launch_example}
\alias{launch_example}
\title{Downloads example data and uses it to showcase the application by calling \code{\link{launch}}}
\usage{
launch_example(data_dir = NULL, hpf = FALSE)
}
\arguments{
\item{data_dir}{A path to the example data if it has been previously downloaded.}

\item{hpf}{Do you have access to HPF? If so, skip downloading and directly use the example data located at "/hpf/largeprojects/MICe/nwang/2019-04-05_pydpiperQC_data"}
}
\description{
Downloads example data and uses it to showcase the application by calling \code{\link{launch}}
}
\examples{
launch_example(hpf=TRUE) #if you have a connection to HPF

}
\seealso{
\code{\link{launch}}
}
