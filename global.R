library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)

library(shiny)
library(RMINC)
library(MRIcrotome)
library(grid)

consensus <- "/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2/Sibille_stress2_nlin/Sibille_stress2-nlin-3.mnc" %>%
  mincGetVolume() %>%
  mincArray()

df <- read_csv("/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2/analysis.csv") %>%
  mutate(nlin_file = file.path("/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2", nlin_file)) %>%
  filter(fwhm == 0.2) %>%
  .[1:10,]
