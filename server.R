server <- function(input, output) {

  input_csv <- reactive({
    req(input$input_csv)
    read_csv(input$input_csv$datapath)
  })

  df <- reactive({
    tibble(
      nlin_file = input$comparate_file,
      comparate_range1 = input$comparate_range[1],
      comparate_range2 = input$comparate_range[2],
      contour_level = input$comparate_contour_level
    )
  })
  output$df <-renderTable({df()})

  output$consensus_histogram <-renderPlot({
    if (input$show_consensus_histogram) {
      consensus %>%
        as.vector() %>%
        as_tibble() %>%
        ggplot() +
        geom_histogram(aes(value), breaks = seq(0, max(consensus),40)) +
        theme(axis.text.y = element_blank()) +
        xlab(NULL) +
        ylab(NULL)}
  })

  comparate <- reactive({
    req(input$comparate_file)
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_file_dropdown <- renderUI({
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(input_csv()$nlin_file,
                                   basename(input_csv()$nlin_file)),
                selected = input_csv()$nlin_file[1])
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

  output$comparate_histogram <- renderPlot({
    if (input$show_comparate_histogram) {
      comparate() %>%
        as.vector() %>%
        as_tibble() %>%
        ggplot() +
        geom_histogram(aes(value), breaks = seq(0, max(comparate()),40)) +
        theme(axis.text.y = element_blank()) +
        xlab(NULL) +
        ylab(NULL)}
  })

  comparate_contours <- reactive({
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$overlay <- renderPlot({
    sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(consensus,
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      contours(comparate(),
               levels = input$comparate_contour_level,
               col="red") %>%
      # anatomySliceIndicator(consensus,
      #                       low = input$consensus_range[1],
      #                       high = input$consensus_range[2]) %>%
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
