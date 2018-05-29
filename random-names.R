random_name <- local({
  adjectives <- readLines("adjectives.txt")
  animals <- readLines("animals.txt")
  
  function(n = 1, date = Sys.Date()) {
    withr::with_seed(as.integer(date), {
      x <- sample(length(adjectives) * length(animals), n * 2, replace = TRUE)
      adj <- x[seq_along(x) %% 2 == 1] %% length(adjectives) + 1
      ani <- x[seq_along(x) %% 2 == 0] %% length(animals) + 1
      paste0(adjectives[adj], "_", animals[ani])
    })
  }
})
