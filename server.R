server <- function(input, output) {

  #TODO make this default properly
  minc_overlay <- reactive({
    input$minc_file %>% mincGetVolume() %>% mincArray()
  })

  output$overlay_levels <- renderUI({
    sliderInput(inputId = "contour_levels",
                label = "Contour Levels",
                min = 0, max = round(max(minc_overlay())),
                value = c(0, round(max(minc_overlay()))))
  })

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=5, begin=100, end=300) %>%
      anatomy(consensus,
              low = input$consensus_levels[1],
              high = input$consensus_levels[2]) %>%
      contours(minc_overlay(),
               levels=c(input$contour_levels[1], input$contour_levels[2]),
               col="red") %>%
      draw()
  })
}
