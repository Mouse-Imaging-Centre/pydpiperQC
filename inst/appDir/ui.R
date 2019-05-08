ui <- fluidPage(
  title = "Pydpiper Quality Check",
  tags$script(js),

  fluidRow(
    column(
      width=2,

      h3("Annotation"),
      fileInput("input_csv", label = NULL),
      downloadButton("download_annotation", label = "Download"),


      h3("Consensus"),
      fileInput("consensus_file", label = NULL),
      uiOutput("consensus_range_slider"),
      radioButtons("mode", label = NULL,
                   choices = list(Contours = "contours", Alpha = "alpha"),
                   selected = "contours",
                   inline = TRUE),
      uiOutput("consensus_contour_alpha_slider"),
      plotOutput("consensus_histogram", height="150px"),
      br(),
      uiOutput("consensus_slice_range_slider"),
      plotOutput("slice_indicator", height="100px")

      # ,textOutput("vars")
    ),

    column(
      width=8,
      imageOutput(outputId = "plot",
                  height = "800px")
    ),

    column(
      width=2,
      h3("Comparate"),
      uiOutput("col_name_dropdown"),
      uiOutput("comparate_file_dropdown"),
      br(),
      uiOutput("comparate_rating_voter"),
      uiOutput("comparate_note_entry"),
      br(),
      uiOutput("comparate_range_slider"),
      plotOutput("comparate_histogram", height="150px")
    )
  ),
  # ,textOutput("vars")
  fluidRow(
    tableOutput("values")
  )
)
