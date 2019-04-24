ui <- navbarPage(
  title = "Pydpiper Quality Check",
  id = "active_tab",


  tabPanel(
    title = "Files",
    value = "Files",
    tags$script(js),

    column(
      width=4,
      h3("Annotation"),
      fileInput("input_csv", label = NULL),
      downloadButton("download_annotation", label = "Download"),

      h3("Consensus"),
      fileInput("consensus_file", label = NULL)
    ),

    column(
      width=8,
      tableOutput("values")
    )
  ),

  tabPanel(
    title = "Contours",
    value = "Contours",
    column(
      width=2,
      h3("Consensus"),
      uiOutput("consensus_range_slider"),
      uiOutput("consensus_contour_slider"),
      checkboxInput(inputId = "show_consensus_histogram",
                    label = "Show Consensus Histogram",
                    value = TRUE),
      plotOutput("consensus_histogram", height="150px"),
      uiOutput("consensus_slice_range_slider"),
      checkboxInput(inputId = "show_slice_indicator",
                    label = "Show Slice Indicator",
                    value = TRUE),
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
      uiOutput("comparate_rating_voter"),
      uiOutput("comparate_note_entry"),
      uiOutput("comparate_range_slider"),
      checkboxInput(inputId = "show_comparate_histogram",
                    label = "Show Comparate Histogram",
                    value = TRUE),
      plotOutput("comparate_histogram", height="150px")
    )
    # ,textOutput("vars")
  )

)
