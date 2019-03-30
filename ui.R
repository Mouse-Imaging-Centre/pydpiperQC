ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),

  column(width=2,
         fluidRow(
           h3("Annotation"),
           fileInput("input_csv", NULL)
         ),

         fluidRow(
           h3("Consensus"),
           fileInput("consensus_file", "Upload average"),
           sliderInput(inputId = "consensus_range",
                       label = "Intensity Range",
                       min = 0, max = round(max(consensus)),
                       value = c(0, round(max(consensus)))),
           checkboxInput(inputId = "show_consensus_histogram",
                         label = "Show Consensus Histogram",
                         value = FALSE),
           plotOutput("consensus_histogram", height="150px")
         )

         ,fluidRow(textOutput("vars"))
  ),

  column(width=8,
    imageOutput(outputId = "plot",
                height = "800px")
  ),

  column(width=2,
         h3("Comparate"),
         uiOutput("comparate_file_dropdown"),
         uiOutput("comparate_rating_voter"),
         uiOutput("comparate_note_entry"),
         uiOutput("comparate_range_slider"),
         uiOutput("comparate_contour_slider"),
         checkboxInput(inputId = "show_comparate_histogram",
                       label = "Show Comparate Histogram",
                       value = FALSE),
         plotOutput("comparate_histogram", height="150px")
  )
  ,fluidRow(tableOutput("values"))
)
