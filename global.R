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

#By default, Shiny limits fule uploads to 5MB per file. Let's allow 1GB.
options(shiny.maxRequestSize=1024*1024^2)

js <- 'var fwd_toggle_state = false;
var rev_toggle_state = false;

$(document).on("keypress", function (e) {
  console.log(e.which);

  if(e.keyCode === 39){
    fwd_toggle_state = !fwd_toggle_state;
    Shiny.onInputChange("fwd_trigger", fwd_toggle_state);
  }

  if(e.keyCode === 37){
    rev_toggle_state = !rev_toggle_state;
    Shiny.onInputChange("rev_trigger", rev_toggle_state);
  }

  if(e.which === 48){
    Shiny.onInputChange("key_rating", "unknown");
  }

  if(e.which === 49){
    console.log("yes->triggered");
    Shiny.onInputChange("key_rating", "terrible");
  }

  if(e.which === 50){
    Shiny.onInputChange("key_rating", "bad");
  }

  if(e.which === 51){
    Shiny.onInputChange("key_rating", "medium");
  }

  if(e.which === 52){
    Shiny.onInputChange("key_rating", "good");
  }

  if(e.which === 53){
    Shiny.onInputChange("key_rating", "great");
  }

});'
