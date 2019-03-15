server <- function(input, output) {

  #TODO make this default properly
  minc_overlay <- reactive({
    input$minc_file %>% mincGetVolume() %>% mincArray()
  })

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=5, begin=100, end=300) %>%
      anatomy(consensus, low=low, high=high) %>%
      contours(minc_overlay(), levels=c(1000,1400), col="red") %>%
      draw()
  })
}
