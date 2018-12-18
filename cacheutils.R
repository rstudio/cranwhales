#' @examples
#' library(promises)
#' library(future)
#' plan(multicore)
#' library(shiny)
#' f <- memoise_plus(function() { future({Sys.sleep(2); 10}) }, cache = memoryCache())
#' f() %...>% print()

# Like memoise, but async-aware.
memoise_plus <- function(func, cache = getShinyOption("cache")) {
  function(...) {
    key <- digest::digest(list(...))
    cache_entry <- cache$get(key, NULL)
    if (!is.null(cache_entry)) {
      if (cache_entry$async) {
        if (cache_entry$success) {
          if (cache_entry$visible) {
            return(promise_resolve(cache_entry$value))
          } else {
            return(promise_resolve(invisible(cache_entry$value)))
          }
        } else {
          return(promise_reject(cache_entry$reason))
        }
      } else {
        if (cache_entry$success) {
          if (cache_entry$visible) {
            return(cache_entry$value)
          } else {
            return(invisible(cache_entry$value))
          }
        } else {
          stop(cache_entry$reason)
        }
      }
    }

    res <- tryCatch(
      {
        # If all goes well, return a list with `success`,
        # `value`, and `visible`
        c(list(success = TRUE), withVisible(func(...)))
      },
      error = function(reason) {
        list(success = FALSE, reason = reason)
      }
    )
    
    if (res$success && is.promising(res$value)) {
      res$value %>% then(
        function(value, .visible) {
          cache$set(key, list(
            success = TRUE,
            async = TRUE,
            value = value,
            visible = .visible
          ))
        },
        function(reason) {
          cache$set(key, list(
            success = FALSE,
            async = TRUE,
            reason = reason
          ))
        }
      )
      return(res$value)
    } else if (res$success) {
      cache$set(key, c(res, list(async = FALSE)))
      if (res$visible) {
        return(res$value)
      } else {
        return(invisible(res$value))
      }
    } else {
      stop(res$reason)
    }
    
    warning("Should never get here")
  }
}

# Pipe onto the end of your `renderXXX` call to cache the output. The
# `cacheKeyExpr` and `cache` options work like they do with the
# `renderCachedPlot` function.
cacheOutput <- function(renderer, cacheKeyExpr, cache = getShinyOption("cache")) {
  force(renderer)
  force(cache)
  cacheKeyQuo <- rlang::enquo(cacheKeyExpr)
  
  current_args <- NULL
  
  f <- function(key) {
    isolate({
      do.call(renderer, current_args)
    })
  }
  mf <- memoise_plus(f, cache = cache)
  function(...) {
    key <- list(
      name = shiny::getCurrentOutputInfo()$name,
      rlang::eval_tidy(cacheKeyQuo)
    )
    current_args <<- list(...)
    on.exit(current_args <<- NULL)
    
    mf(key)
  }
}
