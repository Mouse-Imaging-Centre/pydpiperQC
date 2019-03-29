server <- function(input, output) {

  values <- reactiveValues()

  observeEvent(input$input_csv, {
    values$df <- read_csv(input$input_csv$datapath)

    #if first time, csv will not have these columns initialized.
    if ( !("notes" %in% colnames(values$df)))
      values$df$notes <- NA
    if ( !("rating" %in% colnames(values$df)))
      values$df$rating<- NA
  })

  observeEvent(input$comparate_rating, {
    req(values$df)
    values$df[df_row(), "rating"] <- input$comparate_rating
  })

  output$df <-renderTable({
    req(values$df)
    values$df %>% select(nlin_file, rating)
  })

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

  output$comparate_file_dropdown <- renderUI({
    req(values$df$nlin_file)
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(values$df$nlin_file,
                                   basename(values$df$nlin_file)),
                selected = values$df$nlin_file[1])
  })

  df_row <- reactive({
    which(values$df$nlin_file == input$comparate_file)
  })

  comparate <- reactive({
    req(df_row())
    values$df$nlin_file[as.integer(df_row())] %>% mincGetVolume() %>% mincArray()
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

  output$plot <- renderPlot({
    req(input$consensus_range,
        input$comparate_range,
        input$comparate_contour_level)

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
