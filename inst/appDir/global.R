library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(RMINC)
library(MRIcrotome)
library(grid)

#By default, Shiny limits fule uploads to 5MB per file. Let's allow 1GB.
options(shiny.maxRequestSize=1024*1024^2)

absolutize_path <- function(path) {
  if (!is.null(.GlobalEnv$.annotation_dirname))
    file.path(.GlobalEnv$.annotation_dirname, path)
  else
    file.path(.GlobalEnv$.wd, path)
}

js <- '
var w_presses = 0;
var s_presses = 0;
var comparate_note_enter_presses = 0;

$(document).on("keypress", function (e) {
  console.log(e.which);
  console.log(document.activeElement);

  if (document.activeElement.id === "comparate_note") {
    if(e.which === 13){
    comparate_note_enter_presses++;
    Shiny.onInputChange("comparate_note_enter_press", comparate_note_enter_presses);
    document.activeElement.blur();
  }
  }

  if (document.activeElement.id === "comparate_note") {return}

  if(e.which === 119){
    w_presses++;
    Shiny.onInputChange("w_press", w_presses);
  }

  if(e.which === 115){
    s_presses++;
    Shiny.onInputChange("s_press", s_presses);
  }

  if(e.which === 49){
    Shiny.onInputChange("key_rating", 1);
  }

  if(e.which === 50){
    Shiny.onInputChange("key_rating", 2);
  }

  if(e.which === 51){
    Shiny.onInputChange("key_rating", 3);
  }

  if(e.which === 52){
    Shiny.onInputChange("key_rating", 4);
  }

  if(e.which === 53){
    Shiny.onInputChange("key_rating", 5);
  }

});
'
