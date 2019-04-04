ui <- fluidPage(
  tags$script(js),
  titlePanel("Pydpiper Quality Check"),

  fluidRow(
    column(width=2,

           h3("Annotation"),
           fileInput("input_csv", NULL),

           h3("Consensus"),
           fileInput("consensus_file", "Upload average"),
           uiOutput("consensus_range_slider"),
           uiOutput("consensus_contour_slider"),
           checkboxInput(inputId = "show_consensus_histogram",
                         label = "Show Consensus Histogram",
                         value = FALSE),
           plotOutput("consensus_histogram", height="150px"),
           checkboxInput(inputId = "show_slice_indicator",
                         label = "Show Slice Indicator",
                         value = FALSE),
           plotOutput("slice_indicator", height="100px")

           ,textOutput("vars")
    ),

    column(width=8,
           imageOutput(outputId = "plot",
                       height = "800px")
    ),

    column(width=2,
           h3("Comparate"),
           textInput("col_name",
                     label=NULL,
                     value = "nlin_file"),
           uiOutput("comparate_file_dropdown"),
           uiOutput("comparate_rating_voter"),
           uiOutput("comparate_note_entry"),
           uiOutput("comparate_range_slider"),
           checkboxInput(inputId = "show_comparate_histogram",
                         label = "Show Comparate Histogram",
                         value = FALSE),
           plotOutput("comparate_histogram", height="150px")
    )
  ),

  fluidRow(
    checkboxInput("display_table", "Display Annotations"),
    tableOutput("values")
  )
)
