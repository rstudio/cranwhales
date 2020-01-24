infoBoxOutput <- function(id, width = 4) {
  uiOutput(id, class = paste0("col-sm-", width))
}

renderInfoBox <- renderUI

infoBox <- function(value, subtitle) {
  tagList(
    div(class = "card valuebox h-100 bg-light text-dark",
      div(class = "card-body",
        h3(class = "card-title mb-1", value),
        p(class = "card-text", subtitle)
      )
    )
  )
}
