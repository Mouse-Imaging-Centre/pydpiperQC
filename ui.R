ui <- fluidPage(
  titlePanel("Pydpiper Quality Check"),
  sidebarLayout(

    sidebarPanel(
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
