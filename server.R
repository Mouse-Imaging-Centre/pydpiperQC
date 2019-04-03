server <- function(input, output) {

  consensus <- reactive({
    req(input$consensus_file)
    input$consensus_file$datapath %>% mincGetVolume() %>% mincArray()
  })
  max_consensus <- reactive({
    consensus() %>% max() %>% round()
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

    # walk(c("rating"),
    #      maybe_initialize %>% partial(init=0))

    walk (c("rating", "max_intensity",
            "lower_intensity_range",
            "upper_intensity_range"),
          maybe_initialize %>% partial(init=NA))
  })

  observeEvent(input$comparate_file, {
    max_intensity <- comparate() %>% max() %>% round()
    values$max_intensity[df_row()] <- max_intensity

    if (values$upper_intensity_range[df_row()] %>% is.na())
      values$upper_intensity_range[df_row()] <- max_intensity
  })

  observeEvent(input$comparate_rating, {
    values$rating[df_row()] <- input$comparate_rating
  })

  observeEvent(input$comparate_note, {
    req(values$note)
    values$note[df_row()] <- input$comparate_note
  })

  observeEvent(input$comparate_range, {
    values$lower_intensity_range[df_row()] <- input$comparate_range[1]
    values$upper_intensity_range[df_row()] <- input$comparate_range[2]
  })

  output$vars <- renderPrint({
    plot_conductor() %>% str()
  })

  output$values <- renderTable({
    if (input$display_table) {values %>%
        reactiveValuesToList() %>%
        as_tibble() %>%
        mutate_if(is.character, basename)}
  },
  digits = 0, na=""
  )
  output$consensus_range_slider <- renderUI({
    req(max_consensus())
    sliderInput(inputId = "consensus_range",
                label = "Intensity Range",
                min = 0,
                max = max_consensus(),
                value = c( 0, max_consensus()))
  })
  output$consensus_contour_slider <- renderUI({
    req(max_consensus())
    sliderInput(inputId = "consensus_contour_level",
                label = "Contour Level",
                min = 0, max = max_consensus(),
                value = max_consensus()/2)
  })

  quick_hist <- function(x) {
    x %>%
      hist(breaks=40, plot=FALSE) %>%
      .[c("mids", "counts")] %>%
      as_tibble() %>%
      ggplot() +
      geom_col(aes(x=mids, y=counts))+
      theme(
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
  }
  output$consensus_histogram <- renderPlot({
    if (input$show_consensus_histogram) {
      consensus() %>% quick_hist()
    }
  })

  output$slice_indicator <- renderPlot({
    if (input$show_slice_indicator) {grid.newpage()
      #HACK!
      plot_conductor()$ssl[[3]]$sliceIndicator %>% grid.draw()}
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
    req(df_row())
    numericInput(inputId = "comparate_rating",
                 label = "Rating",
                 value = isolate(values$rating[df_row()]),
                 min = 1, max = 5, step = 1)
  })

  output$comparate_note_entry <- renderUI({
    req(df_row())
    textInput(inputId = "comparate_note",
              label = "Note",
              value = isolate(values$note[df_row()]))
  })

  output$comparate_range_slider <- renderUI({
    req(df_row())
    sliderInput(
      inputId = "comparate_range",
      label = "Intensity Range",
      min = 0,
      max = values$max_intensity[df_row()],
      value = isolate(
        c(values$lower_intensity_range[df_row()],
          values$upper_intensity_range[df_row()])
      )
    )
  })

  output$comparate_histogram <- renderPlot({
    if (input$show_comparate_histogram) {
      comparate() %>% quick_hist()
      }
  })

  plot_conductor <- reactive({
    req(input$consensus_range,
        input$comparate_range,
        input$consensus_contour_level
    )

    sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(consensus(),
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      contours(consensus(),
               levels = input$consensus_contour_level,
               col="red") %>%
      addtitle("Consensus") %>%
      ####
      sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
      contours(consensus(),
               levels = input$consensus_contour_level,
               col="red") %>%
      addtitle("Overlay") %>%
      ####
      sliceSeries(nrow=4, ncol=1, begin=100, end=300) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
      contours(consensus(),
               levels = input$consensus_contour_level,
               col="red") %>%
      addtitle("Comparate") %>%
      anatomySliceIndicator(consensus(),
                            low = input$consensus_range[1],
                            high = input$consensus_range[2])
  })

  output$plot <- renderPlot({
    req(plot_conductor())
    plot_conductor() %>% draw()
  })
}
