server <- function(input, output) {

  values <- reactiveValues()

  observeEvent(input$input_csv, {
    df <- read_csv(input$input_csv$datapath)

    values$nlin_file <- df$nlin_file
    length <- length(df$nlin_file)

    maybe_initialize <- function(col) {
      if (is.null(values[[col]])) values[[col]] <- rep(" ", length)
    }
    walk(c("rating", "note"), maybe_initialize)
  })

  observeEvent(input$comparate_rating, {
    req(values$rating)
    values$rating[df_row()] <- input$comparate_rating
  })

  observeEvent(input$comparate_note, {
    req(values$note)
    values$note[df_row()] <- input$comparate_note
  })

  output$vars <- renderPrint({
    paste("df_row()", df_row())
  })

  output$values <-renderTable({
    req(values)
    values %>% reactiveValuesToList() %>% as_tibble()
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
    req(values$nlin_file)
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(values$nlin_file,
                                   basename(values$nlin_file)),
                selected = values$nlin_file[1])
  })

  df_row <- reactive({
    which(values$nlin_file == input$comparate_file)
  })

  comparate <- reactive({
    req(df_row())
    #TODO
    values$nlin_file[df_row()] %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_rating_voter <- renderUI({
    numericInput(inputId = "comparate_rating",
                 label = "Rating",
                 value = values$rating[df_row()],
                 min = 0, max = 5, step = 1)
  })

  output$comparate_note_entry <- renderUI({
    textInput(inputId = "comparate_note",
              label = "Note",
              value = values$note[df_row()])
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
