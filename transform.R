suppressPackageStartupMessages({
  library(readr)
  library(lubridate)
  library(dplyr)
  library(glue)
  library(feather)
  library(hms)
})

MAX_WHALES <- 12

source("random-names.R")

read_logfile <- function(path) {
  df <- read_csv(path, col_types = "Dti---c-ci", progress = FALSE)
  df %>% filter(!is.na(package))
}

find_whales <- function(df, date = as.Date(df$date[1]), count = MAX_WHALES) {
  df %>%
    count(ip_id, country) %>% 
    arrange(desc(n)) %>%
    head(count) %>%
    mutate(ip_name = factor(ip_id, levels = ip_id,
      labels = glue("{random_name(length(ip_id), date)} [{country}]"))) %>%
    select(-country)
}

whale_downloads <- function(downloads_df, whales_df) {
  downloads_df %>%
    inner_join(whales_df, by = c("ip_id" = "ip_id")) %>%
    select(-n)
}

whale_info <- function(whale_downloads_df) {
  whale_downloads_df %>%
    group_by(ip_id, ip_name) %>%
    summarise(size = sum(as.numeric(size)), n = n()) %>%
    arrange(desc(n))
}

hourly_summary <- function(df, whales_df) {
  df %>%
    mutate(hour = trunc_hms(time, 60 * 60)) %>%
    mutate(whale_index = match(ip_id, whales_df$ip_id)) %>%
    group_by(hour, whale_index) %>%
    summarise(size = sum(as.numeric(size)), n = n()) %>%
    arrange(hour, whale_index)
}

daily_summary <- function(df) {
  unique_downloaders <- df$ip_id %>% unique() %>% length()
  total_size <- df$size %>% as.numeric() %>% sum()
  total_count <- df %>% nrow()
  
  data.frame(
    unique_downloaders,
    total_size,
    total_count
  )
}

process_date <- function(date, output_dir) {
  day_dir <- file.path(output_dir, date)
  dir.create(day_dir, recursive = TRUE)
  
  df <- read_logfile(glue("data_cache/{date}.csv.gz"))
  whales_df <- find_whales(df)
  whale_downloads_df <- whale_downloads(df, whales_df)
  whale_info_df <- whale_info(whale_downloads_df)
  hourly_summary_df <- hourly_summary(df, whales_df)
  daily_summary_df <- daily_summary(df)
  
  files <- list(
    whale_downloads.feather = whale_downloads_df,
    whale_info.feather = whale_info_df,
    hourly_summary.feather = hourly_summary_df,
    daily_summary.feather = daily_summary_df
  )
  
  mapply(names(files), files, FUN = function(name, df) {
    write_feather(df, file.path(day_dir, name))
  })
}

dates <- commandArgs(TRUE)
if (length(dates) == 0) {
  dates <- as.character(Sys.Date() - 1)
}

for (date in dates) {
  message(date)
  process_date(date, "cache/feather")
}
