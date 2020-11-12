detailViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("detail_ip_name"), "Downloader name", character(0)),
    fluidRow(
      valueBoxOutput(ns("detail_size")),
      valueBoxOutput(ns("detail_count")),
      valueBoxOutput(ns("detail_uniques"))
    ),
    plotOutput(ns("detail"),
      brush = brushOpts(ns("detail_brush"), resetOnNew = TRUE)
    ),
    DTOutput(ns("detail_table"))
  )
}

detailView <- function(input, output, session, whales, whale_downloads) {

  # When whales() changes, update the selectInput with their names
  observeEvent(try(silent=TRUE, whales()), {
    choices <- tryCatch(
      whales()$ip_name,
      error = function(err) { character(0) }
    )
    
    if (is.null(input$detail_ip_name)) {
      selected <- character()
    } else if (!input$detail_ip_name %in% choices) {
      selected <- character()
    } else {
      selected <- input$detail_ip_name
    }

    updateSelectInput(session, "detail_ip_name",
      choices = choices, 
      selected = selected,
    )
    freezeReactiveValue(input, "detail_ip_name")
  })
  
  detail_downloads <- reactive({
    req(input$detail_ip_name, nzchar(input$detail_ip_name))
    whale_downloads() %>%
      filter(ip_name == input$detail_ip_name) %>%
      arrange(time) %>%
      mutate(package = factor(package, levels = rev(unique(package)), ordered = TRUE))
  })
  
  output$detail_size <- renderValueBox({
    detail_downloads() %>%
      pull(size) %>%
      as.numeric() %>%  # Cast from integer to numeric to avoid overflow warning
      sum() %>% 
      humanReadable() %>%
      valueBox("bandwidth consumed")
  })
  
  output$detail_count <- renderValueBox({
    detail_downloads() %>%
      nrow() %>%
      format(big.mark = ",") %>%
      valueBox("files downloaded")
  })
  
  output$detail_uniques <- renderValueBox({
    detail_downloads() %>%
      pull(package) %>%
      unique() %>%
      length() %>%
      format(big.mark = ",") %>%
      valueBox("unique packages")
  })
  
  # Show every single download from the selected downloader
  output$detail <- renderPlot({
    
    validate(need(input$detail_ip_name, "Select a downloader from the list above"))    
    pkg <- levels(detail_downloads()$package)
    
    detail_downloads() %>% {
      ggplot(., aes(time, package)) +
        geom_point() +
        scale_x_time(breaks = seq(hms::hms(0,0,0), by = 60*60*3, length.out = 9),
          limits = c(hms::hms(0,0,0), hms::hms(0,0,24))) +
        scale_y_discrete(breaks = pkg[seq(from = 1, to = length(pkg), length.out = 50) %>% as.integer() %>% c(1, length(pkg)) %>% unique()]) +
        ylab(glue("package ({length(pkg)} unique)"))
    }
  })
  
  # Show the downloads that are brushed on output$detail
  output$detail_table <- renderDT({
    req(input$detail_brush)
    detail_downloads() %>%
      brushedPoints(input$detail_brush) %>%
      mutate(
        time = as.character(time),
        size = humanReadable(size)
      ) %>%
      select(-ip_id, -ip_name, -country)
  })
}