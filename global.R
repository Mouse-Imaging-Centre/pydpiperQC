library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

library(shiny)
library(RMINC)
library(MRIcrotome)
library(grid)

consensus <- "/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2/Sibille_stress2_nlin/Sibille_stress2-nlin-3.mnc" %>%
  mincGetVolume() %>%
  mincArray()
