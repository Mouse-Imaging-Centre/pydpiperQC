library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

library(fs)
library(shinyFiles)
library(shiny)
library(RMINC)
library(MRIcrotome)
library(grid)

#By default, Shiny limits fule uploads to 5MB per file. Let's allow 1GB.
options(shiny.maxRequestSize=1024*1024^2)
