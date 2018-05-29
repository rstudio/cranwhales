library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(glue)
library(lubridate)
library(gdata)  # for gdata::humanReadable

source("random-names.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "CRAN whales"
  ),
  dashboardSidebar(
    dateInput("date", "Date", value = Sys.Date() - 2),
    numericInput("count", "Show top N downloaders:", 6)
  ),
  dashboardBody(
    fluidRow(
      tabBox(width = 12,
        tabPanel("All traffic",
          fluidRow(
            valueBoxOutput("total_size", width = 4),
            valueBoxOutput("total_count", width = 4),
            valueBoxOutput("total_downloaders", width = 4)
          ),
          plotOutput("all_hour")
        ),
        tabPanel("Biggest whales",
          plotOutput("downloaders", height = 500)
        ),
        tabPanel("Whales by hour",
          plotOutput("downloaders_hour", height = 500)
        ),
        tabPanel("Detail view",
          selectInput("detail_ip_name", "Downloader name", character(0)),
          fluidRow(
            valueBoxOutput("detail_size"),
            valueBoxOutput("detail_count"),
            valueBoxOutput("detail_uniques")
          ),
          plotOutput("detail", brush = brushOpts("detail_brush", resetOnNew = TRUE)),
          DTOutput("detail_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Downloads data from cran-logs.rstudio.com, and parses it.
  # Successful downloads are stored in the data_cache dir.
  data <- eventReactive(input$date, ignoreNULL = FALSE, {
    date <- input$date
    validate(need(is.Date(date), "Invalid date"))
    
    year <- lubridate::year(date)
    
    url <- glue("http://cran-logs.rstudio.com/{year}/{date}.csv.gz")
    path <- file.path("data_cache", paste0(date, ".csv.gz"))
    
    withProgress(value = NULL, {
      
      # Download to a temporary file path, then rename to the real
      # path when the download is complete. We do this so other
      # processes/sessions don't use partially downloaded files.
      if (!file.exists(path)) {
        tmppath <- paste0(path, "-", Sys.getpid())
        setProgress(message = "Downloading data...")
        download.file(url, tmppath)
        if (!file.exists(path)) {
          file.rename(tmppath, path)
        } else {
          file.remove(tmppath)
        }
      }
      
      setProgress(message = "Parsing data...")
      readr::read_csv(path, col_types = cols(
        date = col_skip(),
        time = col_time(),
        size = col_integer(),
        r_version = col_skip(),
        r_arch = col_skip(),
        r_os = col_skip(),
        package = col_character(),
        version = col_skip(),
        country = col_character(),
        ip_id = col_integer()
      ), progress = FALSE)
      
    })
  })
  
  output$total_size <- renderValueBox({
    valueBox(
      humanReadable(sum(as.numeric(data()$size))),
      "bandwidth consumed"
    )
  })
  
  output$total_count <- renderValueBox({
    valueBox(
      format(nrow(data()), big.mark = ","),
      "files downloaded"
    )
  })
  
  output$total_uniques <- renderValueBox({
    valueBox(
      format(length(unique(data()$package)), big.mark = ","),
      "unique packages"
    )
  })
  
  output$total_downloaders <- renderValueBox({
    valueBox(
      format(length(unique(data()$ip_id)), big.mark = ","),
      "unique downloaders"
    )
  })
  
  output$all_hour <- renderPlot({
    whale_ip <- suspicious_downloaders()$ip_id
    
    data() %>%
      mutate(
        time = hms::trunc_hms(time, 60*60),
        whale = ip_id %in% whale_ip
      ) %>%
      count(time, whale) %>%
      ggplot(aes(time, n, fill = whale)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#666666", "#88FF99"),
        labels = c("no", "yes")) +
      ylab("Downloads") +
      xlab("Hour") +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Returns a data frame of just the top `input$count` downloaders of the day,
  # with the columns: 
  # ip_id - an arbitrary integer that's used in place of the real IP address
  # ip_name - the same as ip_id but using easier-to-remember labels like
  #     "quant_weasel" or "nutritious_lovebird".
  # n - the number of downloads performed by this IP on this day
  suspicious_downloaders <- reactive({
    validate(
      need(is.numeric(input$count), "Invalid top downloader count"),
      need(input$count > 0, "Too few downloaders"),
      need(input$count <= 25, "Too many downloaders; 25 or fewer please")
    )
    data() %>%
      count(ip_id, country) %>%
      arrange(desc(n)) %>%
      head(input$count) %>%
      mutate(ip_name = factor(ip_id, levels = ip_id,
        labels = glue("{random_name(1000, isolate(input$date))[seq_along(ip_id)]} [{country}]"))) %>%
      select(-country)
  })
  
  # When suspicious_downloaders() changes, update the selectInput with their
  # names
  observeEvent(try(silent=TRUE, suspicious_downloaders()), {
    tryCatch({
      updateSelectInput(session, "detail_ip_name",
        choices = suspicious_downloaders()$ip_name,
        selected = if (input$detail_ip_name %in% suspicious_downloaders()$ip_name)
          input$detail_ip_name
        else
          character(0))
      freezeReactiveValue(input, "detail_ip_name")
    }, error = function(e) {})
  })
  
  # data(), filtered down to the downloads that are by the top `input$count`
  # downloaders
  suspicious_downloads <- reactive({
    data() %>%
      inner_join(suspicious_downloaders(), "ip_id") %>%
      select(-n)
  })
  
  output$downloaders <- renderPlot({
    suspicious_downloaders() %>%
      ggplot(aes(ip_name, n)) +
      geom_bar(stat = "identity") +
      ylab("Downloads on this day")
  })
  
  output$downloaders_hour <- renderPlot({
    suspicious_downloads() %>%
      mutate(time = hms::trunc_hms(time, 60*60)) %>%
      ggplot(aes(time)) + geom_bar() +
      facet_wrap(~ip_name)
  })
  
  detail_downloads <- reactive({
    req(input$detail_ip_name, nzchar(input$detail_ip_name))
    suspicious_downloads() %>%
      filter(ip_name == input$detail_ip_name) %>%
      arrange(time) %>%
      mutate(package = factor(package, levels = rev(unique(package)), ordered = TRUE))
  })
  
  output$detail_size <- renderValueBox({
    valueBox(
      humanReadable(sum(as.numeric(detail_downloads()$size))),
      "bandwidth consumed"
    )
  })
  
  output$detail_count <- renderValueBox({
    valueBox(
      format(nrow(detail_downloads()), big.mark = ","),
      "files downloaded"
    )
  })
  
  output$detail_uniques <- renderValueBox({
    valueBox(
      format(length(levels(detail_downloads()$package)), big.mark = ","),
      "unique packages"
    )
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

shinyApp(ui, server)