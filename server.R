server <- function(input, output) {

  output$consensus_histogram <-renderPlot({
    consensus %>%
    as.vector() %>%
    as_tibble() %>%
    ggplot() +
    geom_histogram(aes(value), breaks = seq(0, max(consensus),40)) +
    theme(axis.text.y = element_blank()) +
    xlab(NULL) +
    ylab(NULL)
  })

  comparate <- reactive({
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_range_slider <- renderUI({
    sliderInput(inputId = "comparate_range",
                label = "Intensity Range",
                min = 0, max = round(max(comparate())),
                value = c(0, round(max(comparate()))))
  })

  output$comparate_contour_slider <- renderUI({
    sliderInput(inputId = "comparate_contour_level",
                label = "Contour Level",
                min = 0, max = round(max(comparate())),
                value = max(comparate())/2)
  })

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(consensus,
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      contours(comparate(),
               levels = input$comparate_contour_level,
               col="red") %>%
      addtitle("Consensus") %>%
      ####
      sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(consensus,
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      contours(comparate(),
               levels = input$comparate_contour_level,
               col="red") %>%
      addtitle("Overlay") %>%
      ####
      sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
      contours(comparate(),
               levels = input$comparate_contour_level,
               col="red") %>%
      addtitle("Comparate") %>%
      draw()
  })
}
