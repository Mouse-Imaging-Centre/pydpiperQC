server <- function(input, output, session) {

  output$download_annotation <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_pydpiperQC", ".csv")
    },
    content = function(file) {
      values %>%
        reactiveValuesToList() %>%
        as_tibble() %>%
        left_join(metavalues$input_csv,
                  by=c("comparate_file" = input$col_name)) %>%
        write_csv(file)
    }
  )

  consensus <- reactive({
    req(input$consensus_file)
    input$consensus_file$datapath %>% mincGetVolume() %>% mincArray()
  })
  max_consensus <- reactive({
    consensus() %>% max() %>% round()
  })
  dim_consensus <- reactive({
    consensus() %>% dim()
  })

  values <- reactiveValues()
  metavalues <- reactiveValues()

  observeEvent(input$input_csv, {
    metavalues$input_csv <- read_csv(input$input_csv$datapath)
    metavalues$nrow <- nrow(metavalues$input_csv)

    maybe_initialize <- function(col, init) {
      if (is.null(values[[col]])) values[[col]] <- rep(init, metavalues$nrow)
    }
    walk(c("note"),
         maybe_initialize %>% partial(init=" "))
    walk (c("rating",
            "max_intensity",
            "min_intensity",
            "lower_intensity_range",
            "upper_intensity_range"),
          maybe_initialize %>% partial(init=NA))

    metavalues$col_names <- colnames(metavalues$input_csv)
    if ("nlin_file" %in% metavalues$col_names){metavalues$default_column <- "nlin_file"}
    else {metavalues$default_column <- 1}
  })

  observeEvent(input$col_name, {
    req(metavalues$default_column)
    values$comparate_file <-
      metavalues$input_csv[, input$col_name, drop=TRUE]
  })

  observeEvent(input$comparate_file, {
    metavalues$last_df_row <-
      which(values$comparate_file == input$comparate_file)
    if (!is.null(metavalues$df_row))
      metavalues$last_df_row <- metavalues$df_row
    metavalues$df_row <-
      which(values$comparate_file == input$comparate_file)

    max_intensity <- comparate() %>% max() %>% round()
    min_intensity <- comparate() %>% min() %>% round()

    values$max_intensity[metavalues$df_row] <- max_intensity
    values$min_intensity[metavalues$df_row] <- min_intensity

    if (values$upper_intensity_range[metavalues$df_row] %>% is.na())
      if (values$upper_intensity_range[metavalues$last_df_row] %>% is.na())
        values$upper_intensity_range[metavalues$df_row] <- max_intensity
      else
        values$upper_intensity_range[metavalues$df_row] <-
      values$upper_intensity_range[metavalues$last_df_row]

    if (values$lower_intensity_range[metavalues$df_row] %>% is.na())
      if (values$lower_intensity_range[metavalues$last_df_row] %>% is.na())
        values$lower_intensity_range[metavalues$df_row] <- min_intensity
    else
      values$lower_intensity_range[metavalues$df_row] <-
      values$lower_intensity_range[metavalues$last_df_row]
  })

  observeEvent(input$comparate_rating, {
    values$rating[metavalues$df_row] <- input$comparate_rating
  })

  observeEvent(input$comparate_note_enter_press, {
    req(values$note)
    values$note[metavalues$df_row] <- input$comparate_note
  })

  observeEvent(input$comparate_range, {
    values$lower_intensity_range[metavalues$df_row] <- input$comparate_range[1]
    values$upper_intensity_range[metavalues$df_row] <- input$comparate_range[2]
  })

  observeEvent(input$w_press, {
    if(metavalues$df_row > 1){
      updateSelectInput(
        session, "comparate_file",
        choices = setNames(values$comparate_file,
                           basename(values$comparate_file)),
        selected = values$comparate_file[metavalues$df_row - 1]
      )
    }
  })

  observeEvent(input$s_press, {
    if(metavalues$df_row < metavalues$nrow){
      updateSelectInput(
        session, "comparate_file",
        choices = setNames(values$comparate_file,
                           basename(values$comparate_file)),
        selected = values$comparate_file[metavalues$df_row + 1]
      )
    }
  })

  observeEvent(input$key_rating, {
    updateNumericInput(
      session, "comparate_rating",
      label = "Rating",
      value = input$key_rating,
      min = 1, max = 5, step = 1)
  })

  output$vars <- renderPrint({
  })

  output$values <- renderTable({
    if (input$display_table) {
      values %>%
        reactiveValuesToList() %>%
        as_tibble() %>%
        mutate_if(is.character, basename)
      }
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

  output$consensus_slice_range_slider <- renderUI({
    req(dim_consensus())
    sliderInput(inputId = "consensus_slice_range",
                label = "Slices",
                min = 0, max = dim_consensus()[2],
                value = c(0.1, 0.9) * dim_consensus()[2])
  })
  output$slice_indicator <- renderPlot({
    if (input$show_slice_indicator) {grid.newpage()
      #HACK!
      plot_conductor()$ssl[[3]]$sliceIndicator %>% grid.draw()}
  })

  output$col_name_dropdown <- renderUI({
    req(metavalues$col_names, metavalues$default_column)
    selectInput(inputId = "col_name",
                label = NULL,
                choices = metavalues$col_names,
                selected = metavalues$default_column)
  })

  output$comparate_file_dropdown <- renderUI({
    req(values$comparate_file)
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(values$comparate_file,
                                   basename(values$comparate_file)),
                selected = values$comparate_file[1])
  })

  comparate <- reactive({
    req(metavalues$df_row)
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_rating_voter <- renderUI({
    req(metavalues$df_row)
    numericInput(inputId = "comparate_rating",
                 label = "Rating",
                 value = isolate(values$rating[metavalues$df_row]),
                 min = 1, max = 5, step = 1)
  })

  output$comparate_note_entry <- renderUI({
    req(metavalues$df_row)
    textInput(inputId = "comparate_note",
              label = "Note",
              value = isolate(values$note[metavalues$df_row]))
  })

  output$comparate_range_slider <- renderUI({
    req(metavalues$df_row)
    sliderInput(
      inputId = "comparate_range",
      label = "Intensity Range",
      min = values$min_intensity[metavalues$df_row],
      max = values$max_intensity[metavalues$df_row],
      value = isolate(
        c(values$lower_intensity_range[metavalues$df_row],
          values$upper_intensity_range[metavalues$df_row])
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

    sliceSeries(nrow=4, ncol=1,
                begin=input$consensus_slice_range[1],
                end=input$consensus_slice_range[2]) %>%
      anatomy(consensus(),
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
      contours(consensus(),
               levels = input$consensus_contour_level,
               col="red") %>%
      addtitle("Consensus") %>%
      ####
      sliceSeries(nrow=4, ncol=1,
                  begin=input$consensus_slice_range[1],
                  end=input$consensus_slice_range[2]) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
      contours(consensus(),
               levels = input$consensus_contour_level,
               col="red") %>%
      addtitle("Overlay") %>%
      ####
      sliceSeries(nrow=4, ncol=1,
                  begin=input$consensus_slice_range[1],
                  end=input$consensus_slice_range[2]) %>%
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