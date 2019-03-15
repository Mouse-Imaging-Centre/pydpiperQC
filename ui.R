ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),
  sidebarLayout(

    sidebarPanel(

      wellPanel(
        h3("Consensus"),
        sliderInput(inputId = "consensus_range",
                    label = "Intensity Range",
                    min = 0, max = round(max(consensus)),
                    value = c(0, round(max(consensus)))),
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

    mainPanel(
      imageOutput(outputId = "overlay")
    )
  )
)
