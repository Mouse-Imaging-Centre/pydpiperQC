ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),

  column(width=2,
         fluidRow(
           fileInput("input_csv", "Upload previous annotation")
         ),

         fluidRow(
           h3("Consensus"),
           sliderInput(inputId = "consensus_range",
                       label = "Intensity Range",
                       min = 0, max = round(max(consensus)),
                       value = c(0, round(max(consensus)))),
           checkboxInput(inputId = "show_consensus_histogram",
                         label = "Show Consensus Histogram",
                         value = FALSE),
           plotOutput("consensus_histogram", height="150px")
         )

  ),

  column(width=8,
    imageOutput(outputId = "overlay",
                height = "800px")
  ),

  column(width=2,
         h3("Comparate"),
         uiOutput("comparate_file_dropdown"),
         uiOutput("comparate_range_slider"),
         uiOutput("comparate_contour_slider"),
         checkboxInput(inputId = "show_comparate_histogram",
                       label = "Show Comparate Histogram",
                       value = FALSE),
         plotOutput("comparate_histogram", height="150px")
  )
  # ,fluidRow(tableOutput("df"))
)
