ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),
  sidebarLayout(

    sidebarPanel(

      sliderInput(inputId = "consensus_levels",
                  label = "Consensus Levels",
                  min = 0, max = max(consensus),
                  value = c(0, max(consensus))),

      selectInput(inputId = "minc_file",
                  label = "Minc File",
                  choices = df$nlin_file,
                  selected = df$nlin_file[1])
    ),

    mainPanel(
      imageOutput(outputId = "overlay")
    )
  )
)
