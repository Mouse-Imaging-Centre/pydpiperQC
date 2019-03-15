server <- function(input, output) {

  #TODO make this default properly
  comparate <- reactive({
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_range_slider <- renderUI({
    sliderInput(inputId = "comparate_range",
                label = "Intensity Range",
                min = 0, max = round(max(comparate())),
                value = c(0, round(max(comparate()))))
  })

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(consensus,
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      addtitle("Consensus") %>%
      ####
      sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
      addtitle("Comparate") %>%
      # ####
      # sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      # anatomy(consensus,
      #         low = input$nlin3_range[1],
      #         high = input$nlin3_range[2]) %>%
      # addtitle("Overlay") %>%
      draw()
  })
}
