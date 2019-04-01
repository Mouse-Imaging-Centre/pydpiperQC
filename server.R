server <- function(input, output) {

  consensus <- reactive({
    req(input$consensus_file)
    input$consensus_file$datapath %>% mincGetVolume() %>% mincArray()
  })

  values <- reactiveValues()

  observeEvent(input$input_csv, {
    df <- read_csv(input$input_csv$datapath)

    values$comparate_file <- df[,input$col_name,drop=TRUE]
    length <- nrow(df)

    maybe_initialize <- function(col, init) {
      if (is.null(values[[col]])) values[[col]] <- rep(init, length)
    }

    walk(c("note"),
         maybe_initialize %>% partial(init=" "))

    walk(c("max_intensity",
           "rating",
           "lower_intensity_range",
           "upper_intensity_range"),
         maybe_initialize %>% partial(init=0))
  })

  observeEvent(input$comparate_file, {
    req(values$max_intensity, values$upper_intensity_range)

    max_intensity <- comparate() %>% max() %>% round()

    values$max_intensity[df_row()] <- max_intensity

    if (values$upper_intensity_range[df_row()]==0)
      values$upper_intensity_range[df_row()] <- max_intensity
  })

  observeEvent(input$comparate_rating, {
    req(values$rating)
    values$rating[df_row()] <- input$comparate_rating
  })

  observeEvent(input$comparate_note, {
    req(values$note)
    values$note[df_row()] <- input$comparate_note
  })

  observeEvent(input$comparate_range, {
    req(values$lower_intensity_range,
        values$upper_intensity_range)
    values$lower_intensity_range[df_row()] <- input$comparate_range[1]
    values$upper_intensity_range[df_row()] <- input$comparate_range[2]
  })

  output$vars <- renderPrint({
    ""
  })

  output$values <-renderTable({
    values %>%
      reactiveValuesToList() %>%
      as_tibble() %>%
      mutate_if(is.character, basename)
  },
  digits = 0
  )

  output$consensus_histogram <-renderPlot({
    if (input$show_consensus_histogram) {
      consensus() %>%
        as.vector() %>%
        as_tibble() %>%
        ggplot() +
        geom_histogram(aes(value), breaks = seq(0, max(consensus()),40)) +
        theme(axis.text.y = element_blank()) +
        xlab(NULL) +
        ylab(NULL)}
  })

  output$comparate_file_dropdown <- renderUI({
    req(values$comparate_file)
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(values$comparate_file,
                                   basename(values$comparate_file)),
                selected = values$comparate_file[1])
  })

  df_row <- reactive({
    which(values$comparate_file == input$comparate_file)
  })

  comparate <- reactive({
    req(df_row())
    input$comparate_file %>% mincGetVolume() %>% mincArray()
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
    req(df_row())
    sliderInput(inputId = "comparate_range",
                label = "Intensity Range",
                min = 0,
                max = values$max_intensity[df_row()],
                value = c( values$lower_intensity_range[df_row()],
                           values$upper_intensity_range[df_row()]))
  })

  output$comparate_contour_slider <- renderUI({
    sliderInput(inputId = "comparate_contour_level",
                label = "Contour Level",
                min = 0, max = values$max_intensity[df_row()],
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
      anatomy(consensus(),
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
      anatomy(consensus(),
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
