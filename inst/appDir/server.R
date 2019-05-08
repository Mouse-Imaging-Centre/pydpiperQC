server <- function(input, output, session) {

  output$download_annotation <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_pydpiperQC", ".csv")
    },
    content = function(file) {
      df %>%
        reactiveValuesToList() %>%
        tibble::as_tibble() %>%
        left_join(rv$input_csv,
                  by=c("comparate_file" = input$col_name)) %>%
        write_csv(file)
    }
  )


  df <- reactiveValues()
  rv <- reactiveValues()

  rv$annotation <- .GlobalEnv$.annotation
  rv$consensus_file <- .GlobalEnv$.consensus

  observeEvent({
    input$consensus_file
    rv$consensus_file
  }, {
    if (is.null(input$consensus_file$datapath))
      rv$consensus <- rv$consensus_file %>%
        mincGetVolume() %>% mincArray()
    else
      rv$consensus <- input$consensus_file$datapath %>%
        mincGetVolume() %>% mincArray()
    rv$max_consensus <- rv$consensus %>% max() %>% round()
    rv$dim_consensus <- rv$consensus %>% dim()
  })

  observeEvent({
    input$input_csv
    rv$annotation
  }, {
    if(is.null(input$input_csv$datapath))
      rv$input_csv <- read_csv(rv$annotation)
    else
      rv$input_csv <- read_csv(input$input_csv$datapath)
    rv$nrow <- nrow(rv$input_csv)

    maybe_initialize <- function(col, init) {
      if (is.null(df[[col]])) df[[col]] <- rep(init, rv$nrow)
    }
    walk(c("note"),
         maybe_initialize %>% partial(init=" "))
    walk (c("rating",
            "max_intensity",
            "min_intensity",
            "lower_intensity_range",
            "upper_intensity_range"),
          maybe_initialize %>% partial(init=NA))

    rv$col_names <- colnames(rv$input_csv)
    if ("nlin_file" %in% rv$col_names){rv$default_column <- "nlin_file"}
    else {rv$default_column <- 1}
  })

  observeEvent(input$col_name, {
    req(rv$default_column)
    df$comparate_file <-
      rv$input_csv[, input$col_name, drop=TRUE] %>%
      absolutize_path
  })

  observeEvent(input$comparate_file, {
    rv$last_df_row <-
      which(df$comparate_file == input$comparate_file)
    if (!is.null(rv$df_row))
      rv$last_df_row <- rv$df_row
    rv$df_row <-
      which(df$comparate_file == input$comparate_file)

    max_intensity <- comparate() %>% max() %>% round()
    min_intensity <- comparate() %>% min() %>% round()

    df$max_intensity[rv$df_row] <- max_intensity
    df$min_intensity[rv$df_row] <- min_intensity

    if (df$upper_intensity_range[rv$df_row] %>% is.na())
      if (df$upper_intensity_range[rv$last_df_row] %>% is.na())
        df$upper_intensity_range[rv$df_row] <- max_intensity
      else
        df$upper_intensity_range[rv$df_row] <-
      df$upper_intensity_range[rv$last_df_row]

    if (df$lower_intensity_range[rv$df_row] %>% is.na())
      if (df$lower_intensity_range[rv$last_df_row] %>% is.na())
        df$lower_intensity_range[rv$df_row] <- min_intensity
    else
      df$lower_intensity_range[rv$df_row] <-
      df$lower_intensity_range[rv$last_df_row]
  })

  observeEvent(input$comparate_rating, {
    df$rating[rv$df_row] <- input$comparate_rating
  })

  observeEvent(input$comparate_note_enter_press, {
    req(df$note)
    df$note[rv$df_row] <- input$comparate_note
  })

  observeEvent(input$comparate_range, {
    df$lower_intensity_range[rv$df_row] <- input$comparate_range[1]
    df$upper_intensity_range[rv$df_row] <- input$comparate_range[2]
  })

  observeEvent(input$w_press, {
    if(rv$df_row > 1){
      updateSelectInput(
        session, "comparate_file",
        choices = setNames(df$comparate_file,
                           basename(df$comparate_file)),
        selected = df$comparate_file[rv$df_row - 1]
      )
    }
  })

  observeEvent(input$s_press, {
    if(rv$df_row < rv$nrow){
      updateSelectInput(
        session, "comparate_file",
        choices = setNames(df$comparate_file,
                           basename(df$comparate_file)),
        selected = df$comparate_file[rv$df_row + 1]
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

  output$df <- renderTable({
    df %>%
      reactiveValuesToList() %>%
      tibble::as_tibble() %>%
      mutate_if(is.character, basename)
  },
  digits = 0, na=""
  )

  output$consensus_range_slider <- renderUI({
    req(rv$max_consensus)
    sliderInput(inputId = "consensus_range",
                label = "Intensity Range",
                min = 0,
                max = rv$max_consensus,
                value = c( 0, rv$max_consensus))
  })
  output$consensus_contour_alpha_slider <- renderUI({
    req(rv$max_consensus)
    if (input$mode == "contours")
      sliderInput(inputId = "consensus_contour_alpha",
                  label = NULL,
                  min = 0, max = rv$max_consensus,
                  value = rv$max_consensus/2)
    else if (input$mode == "alpha")
      sliderInput(inputId = "consensus_contour_alpha",
                  label = NULL,
                  min = 0, max = 1,
                  value = 0.5)
  })

  quick_hist <- function(x) {
    x %>%
      hist(breaks=40, plot=FALSE) %>%
      .[c("mids", "counts")] %>%
      tibble::as_tibble() %>%
      ggplot() +
      geom_col(aes(x=mids, y=counts))+
      theme(
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
  }
  output$consensus_histogram <- renderPlot({
     rv$consensus %>% quick_hist()
  })

  output$consensus_slice_range_slider <- renderUI({
    req(rv$dim_consensus)
    sliderInput(inputId = "consensus_slice_range",
                label = "Slices",
                min = 0, max = rv$dim_consensus[2],
                value = c(0.1, 0.9) * rv$dim_consensus[2])
  })
  output$slice_indicator <- renderPlot({
    grid.newpage()
    #HACK!
    plot_conductor()$ssl[[3]]$sliceIndicator %>% grid.draw()
  })

  output$col_name_dropdown <- renderUI({
    req(rv$col_names, rv$default_column)
    selectInput(inputId = "col_name",
                label = NULL,
                choices = rv$col_names,
                selected = rv$default_column)
  })

  output$comparate_file_dropdown <- renderUI({
    req(df$comparate_file)
    selectInput(inputId = "comparate_file",
                label = NULL,
                choices = setNames(df$comparate_file,
                                   basename(df$comparate_file)),
                selected = df$comparate_file[1])
  })

  comparate <- reactive({
    req(rv$df_row)
    input$comparate_file %>% mincGetVolume() %>% mincArray()
  })

  output$comparate_rating_voter <- renderUI({
    req(rv$df_row)
    numericInput(inputId = "comparate_rating",
                 label = "Rating",
                 value = isolate(df$rating[rv$df_row]),
                 min = 1, max = 5, step = 1)
  })

  output$comparate_note_entry <- renderUI({
    req(rv$df_row)
    textInput(inputId = "comparate_note",
              label = "Note",
              value = isolate(df$note[rv$df_row]))
  })

  output$comparate_range_slider <- renderUI({
    req(rv$df_row)
    sliderInput(
      inputId = "comparate_range",
      label = "Intensity Range",
      min = df$min_intensity[rv$df_row],
      max = df$max_intensity[rv$df_row],
      value = isolate(
        c(df$lower_intensity_range[rv$df_row],
          df$upper_intensity_range[rv$df_row])
      )
    )
  })

  output$comparate_histogram <- renderPlot({
      comparate() %>% quick_hist()
  })

  plot_conductor <- reactive({
    req(input$consensus_range,
        input$comparate_range,
        input$consensus_contour_alpha
    )

    sliceSeries(nrow=4, ncol=1,
                begin=input$consensus_slice_range[1],
                end=input$consensus_slice_range[2]) %>%
      anatomy(rv$consensus,
              low = input$consensus_range[1],
              high = input$consensus_range[2]) %>%
              {if (input$mode == "contours")
                contours(.,
                         rv$consensus,
                         levels = input$consensus_contour_alpha,
                         col="red")
                else .} %>%
      addtitle("Consensus") %>%
      ####
      sliceSeries(nrow=4, ncol=1,
                  begin=input$consensus_slice_range[1],
                  end=input$consensus_slice_range[2]) %>% {
                    if (input$mode == "contours")
                      anatomy(.,
                              comparate(),
                              low = input$comparate_range[1],
                              high = input$comparate_range[2]) %>%
                      contours(rv$consensus,
                               levels = input$consensus_contour_alpha,
                               col="red")
                    else
                      anatomy(.,
                              rv$consensus,
                              low = input$consensus_range[1],
                              high = input$consensus_range[2],
                              alpha = 1 - input$consensus_contour_alpha) %>%
                      overlay(comparate(),
                              low = input$comparate_range[1],
                              high = input$comparate_range[2],
                              alpha = input$consensus_contour_alpha,
                              col = gray.colors(255, start = 0))} %>%

      addtitle("Overlay") %>%
      ####
      sliceSeries(nrow=4, ncol=1,
                  begin=input$consensus_slice_range[1],
                  end=input$consensus_slice_range[2]) %>%
      anatomy(comparate(),
              low = input$comparate_range[1],
              high = input$comparate_range[2]) %>%
              {if (input$mode == "contours")
                contours(.,
                         rv$consensus,
                         levels = input$consensus_contour_alpha,
                         col="red")
                else .} %>%
      addtitle("Comparate") %>%
      anatomySliceIndicator(rv$consensus,
                            low = input$consensus_range[1],
                            high = input$consensus_range[2])
  })

  output$plot <- renderPlot({
    req(plot_conductor())
    plot_conductor() %>% draw()
  })
}
