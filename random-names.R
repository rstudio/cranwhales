random_name <- local({
  adjectives <- readLines("adjectives.txt")
  animals <- readLines("animals.txt")
  
  function(n = 1, date = Sys.Date()) {
    withr::with_seed(as.integer(date) * n,
      paste0(sample(adjectives, n, replace = TRUE), "_", sample(animals, n, replace = TRUE))
    )
  }
})
