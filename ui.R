ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),

  column(width=2.5,
         h4("Consensus"),
         sliderInput(inputId = "consensus_range",
                     label = "Intensity Range",
                     min = 0, max = round(max(consensus)),
                     value = c(0, round(max(consensus)))),
         checkboxInput(inputId = "show_consensus_histogram",
                       label = "Show Consensus Histogram",
                       value = FALSE),
         plotOutput("consensus_histogram", height="150px")
  ),

  column(width=7,
    imageOutput(outputId = "overlay")
  ),

  column(width=2.5,
         h4("Comparate"),
         selectInput(inputId = "comparate_file",
                     label = NULL,
                     choices = setNames(df$nlin_file,
                                        basename(df$nlin_file)),
                     selected = df$nlin_file[1]),

         #sliderInput
         uiOutput("comparate_range_slider"),
         uiOutput("comparate_contour_slider"),
         checkboxInput(inputId = "show_comparate_histogram",
                       label = "Show Comparate Histogram",
                       value = FALSE),
         plotOutput("comparate_histogram", height="150px")
  )
)
