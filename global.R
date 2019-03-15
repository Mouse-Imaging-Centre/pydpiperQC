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

consensus_histogram <- consensus %>%
  as.vector() %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(value), breaks = seq(0, max(consensus),40))

df <- read_csv("/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2/analysis.csv") %>%
  mutate(nlin_file = file.path("/hpf/largeprojects/MICe/nwang/collaborator_40um/Sibille_stress2", nlin_file)) %>%
  filter(fwhm == 0.2) %>%
  .[1:10,]
