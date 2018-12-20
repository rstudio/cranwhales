library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(glue)
library(lubridate)
library(gdata)  # for gdata::humanReadable
library(feather)

shinyOptions(cache = diskCache("cache/shiny"))

source("random-names.R")
source("modules/detail.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "CRAN whales"
  ),
  dashboardSidebar(
    dateInput("date", "Date", value = Sys.Date() - 3),
    numericInput("count", "Show top N downloaders:", 6)
  ),
  dashboardBody(
    fluidRow(
      tabBox(id = "tab", width = 12,
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
          detailViewUI("details")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ### Reactive expressions ============================================
  
  feather_data <- function(filename) {
    req(input$date)
    
    filepath <- file.path("cache/feather", input$date, filename)
    validate(need(file.exists(filepath), "Sorry, data is not available for the requested date"))
    
    read_feather(filepath)
  }

  # Returns a data frame of just the top `input$count` downloaders of the day,
  # with the columns: 
  # ip_id - an arbitrary integer that's used in place of the real IP address
  # ip_name - the same as ip_id but using easier-to-remember labels like
  #     "quant_weasel" or "nutritious_lovebird".
  # n - the number of downloads performed by this IP on this day
  whales <- reactive({
    validate(
      need(is.numeric(input$count), "Invalid top downloader count"),
      need(input$count > 0, "Too few downloaders"),
      need(input$count <= 12, "Too many downloaders; 12 or fewer please")
    )

    feather_data("whale_info.feather") %>%
      head(input$count)
  })
  
  # data(), filtered down to the downloads that are by the top `input$count`
  # downloaders
  whale_downloads <- reactive({
    feather_data("whale_downloads.feather") %>%
      filter(ip_id %in% whales()$ip_id)
  })

  hourly_summary <- reactive({
    feather_data("hourly_summary.feather") %>%
      mutate(is_whale = !is.na(whale_index) & whale_index <= input$count) %>%
      group_by(hour, is_whale) %>%
      summarise(size = sum(size), n = sum(n)) %>%
      arrange(hour, is_whale)
  })

  daily_summary <- reactive({
    feather_data("daily_summary.feather")
  })
  
  ### Outputs =========================================================
  
  #### "All traffic" tab ----------------------------------------
  
  output$total_size <- renderValueBox({
    daily_summary()$total_size %>%
      humanReadable() %>%
      valueBox("bandwidth consumed")
  })
  
  output$total_count <- renderValueBox({
    daily_summary()$total_count %>%
      format(big.mark = ",") %>%
      valueBox("files downloaded")
  })
  
  output$total_downloaders <- renderValueBox({
    daily_summary()$unique_downloaders %>%
      format(big.mark = ",") %>%
      valueBox("unique downloaders")
  })
  
  output$all_hour <- renderCachedPlot({
    whale_ip <- whales()$ip_id
    
    hourly_summary() %>%
      ggplot(aes(hour, n, fill = is_whale)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#666666", "#88FF99"),
        labels = c("no", "yes")) +
      ylab("Downloads") +
      xlab("Hour") +
      scale_y_continuous(labels = scales::comma)
  }, cacheKeyExpr = { whales() })
  
  #### "Biggest whales" tab -------------------------------------
  
  output$downloaders <- renderCachedPlot({
    whales() %>%
      ggplot(aes(ip_name, n)) +
      geom_bar(stat = "identity") +
      ylab("Downloads on this day")
  }, cacheKeyExpr = { whales() })
  
  #### "Whales by hour" tab -------------------------------------
  
  output$downloaders_hour <- renderCachedPlot({
    whale_downloads() %>%
      mutate(time = hms::trunc_hms(time, 60*60)) %>%
      count(time, ip_name) %>%
      ggplot(aes(time, n)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ip_name) +
      ylab("Downloads") +
      xlab("Hour")
  }, cacheKeyExpr = { whales() })
  
  #### "Detail view" tab ----------------------------------------

  callModule(detailView, "details", whales, whale_downloads)
}

shinyApp(ui, server)
