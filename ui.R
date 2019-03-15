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
        hr()),

      wellPanel(
        h3("Comparate"),
        selectInput(inputId = "comparate_file",
                    label = "Comparate File",
                    choices = df$nlin_file,
                    selected = df$nlin_file[1]),

        #sliderInput
        uiOutput("comparate_range_slider"),
        hr())
    ),

    mainPanel(
      imageOutput(outputId = "overlay")
    )
  )
)
