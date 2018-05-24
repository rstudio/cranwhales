library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(lubridate)

source("random-names.R")


# most_suspicious <- function(df, n = 6) {
#   keep <- n
#   most_downloads <- df %>%
#     count(ip_id) %>%
#     arrange(desc(n)) %>%
#     head(keep)
#   df %>% filter(ip_id %in% most_downloads$ip_id)
# }



ui <- dashboardPage(
  dashboardHeader(
    title = "CRAN mirror abusers"
  ),
  dashboardSidebar(
    dateInput("date", "Date", value = "2018-05-22"),
    actionButton("go", "Go")
  ),
  dashboardBody(
    numericInput("count", "Show top N downloaders:", 6),
    fluidRow(
      tabBox(width = 12,
        title = "Top downloaders of the day",
        tabPanel("Total downloads",
          plotOutput("downloaders")
        ),
        tabPanel("Downloads per hour",
          plotOutput("downloaders_time")
        ),
        tabPanel("Detail view",
          selectInput("detail_ip_name", "Downloader name", character(0)),
          plotOutput("detail")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- eventReactive(input$go, ignoreNULL = FALSE, {
    date <- input$date
    validate(need(grepl("^\\d{4}-\\d{2}-\\d{2}", date), "Invalid date"))
    
    year <- substr(date, 1, 4)
    
    url <- glue("http://cran-logs.rstudio.com/{year}/{date}.csv.gz")
    path <- file.path("data_cache", paste0(date, ".csv.gz"))
    
    withProgress(value = NULL, {
      
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
        date = col_date(format = ""),
        time = col_time(format = ""),
        size = col_integer(),
        r_version = col_character(),
        r_arch = col_character(),
        r_os = col_character(),
        package = col_character(),
        version = col_character(),
        country = col_character(),
        ip_id = col_integer()
      ))
      
    })
  })
  
  suspicious_downloaders <- reactive({
    data() %>%
      count(ip_id, country) %>%
      arrange(desc(n)) %>%
      head(input$count) %>%
      mutate(ip_name = factor(ip_id, levels = ip_id,
        labels = glue("{random_name(1000, isolate(input$date))[seq_along(ip_id)]} [{country}]")))
  })
  
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
  
  suspicious_downloads <- reactive({
    data() %>%
      inner_join(suspicious_downloaders(), "ip_id") %>%
      select(-n)
  })
  
  output$downloaders <- renderTable({
    suspicious_downloaders() %>% head()
  })
  
  output$downloaders <- renderPlot({
    suspicious_downloaders() %>%
      ggplot(aes(ip_name, n)) +
      geom_bar(stat = "identity") +
      ylab("Downloads on this day")
  })
  
  output$downloaders_time <- renderPlot({
    suspicious_downloads() %>%
      mutate(time = hms::trunc_hms(time, 60*60)) %>%
      ggplot(aes(time)) + geom_bar() +
      facet_wrap(~ip_name)
  })
  
  output$detail <- renderPlot({
    req(input$detail_ip_name, nzchar(input$detail_ip_name))
    suspicious_downloads() %>%
      filter(ip_name == input$detail_ip_name) %>%
      arrange(time) %>%
      mutate(package = factor(package, levels = rev(unique(package)), ordered = TRUE)) %>%
      ggplot(aes(time, package)) +
      geom_point() +
      scale_x_time(breaks = seq(hms::hms(0,0,0), by = 60*60*3, length.out = 9),
        limits = c(hms::hms(0,0,0), hms::hms(0,0,24)))
  })
}

shinyApp(ui, server)