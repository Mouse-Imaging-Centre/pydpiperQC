server <- function(input, output) {

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=5, begin=100, end=300) %>%
      anatomy(consensus, low=low, high=high) %>%
      contours(consensus, levels=c(1000,1400), col="red") %>%
      draw()
  })
}
