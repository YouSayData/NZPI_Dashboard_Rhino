box::use(
  shiny[
    div,
    moduleServer,
    NS,
    req,
    uiOutput,
    renderUI,
    tags,
  ],
  dplyr[
    filter,
    arrange,
    desc,
    pull,
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("plot"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderUI({
      
      tmp <- data() |>
        filter(!is.na(nest_img))
      
      req(nrow(tmp) > 0)
      
      img_link <- tmp |>
        arrange(desc(Date)) |>
        pull(nest_img)
      
      tags$img(src = img_link[1],
               width='100%')
    })
  })
}