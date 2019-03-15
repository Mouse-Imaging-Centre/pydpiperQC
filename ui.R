ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),
  sidebarLayout(

    sidebarPanel(width=5,

      wellPanel(
        h3("Consensus"),
        sliderInput(inputId = "consensus_range",
                    label = "Intensity Range",
                    min = 0, max = round(max(consensus)),
                    value = c(0, round(max(consensus)))),
        checkboxInput(inputId = "show_consensus_histogram",
                      label = "Show Consensus Histogram",
                      value = FALSE),
        plotOutput("consensus_histogram", height="200px")
        ),

      wellPanel(
        h3("Comparate"),
        selectInput(inputId = "comparate_file",
                    label = "Comparate File",
                    choices = setNames(df$nlin_file,
                                       basename(df$nlin_file)),
                    selected = df$nlin_file[1]),

        #sliderInput
        uiOutput("comparate_range_slider"),
        uiOutput("comparate_contour_slider")
        )
    ),

    mainPanel(width=7,
      imageOutput(outputId = "overlay")
    )
  )
)
