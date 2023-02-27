box::use(
  echarts4r[
    echarts4rOutput,
    renderEcharts4r,
    e_charts,
    e_bar,
    e_tooltip,
  ],
  shiny[
    div,
    moduleServer,
    NS,
    req
  ],
  dplyr[
    mutate,
    group_by,
    across,
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  echarts4rOutput(ns("plot"))
}

#' @export
server <- function(id, data, show_counts) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderEcharts4r({
      
      tmp <- data()
      
      req(nrow(tmp) > 0)
      
      if (show_counts()) {
        echart <- tmp |>
          mutate(year = as.character(year)) |>
          group_by(Clutch) |>
          e_charts(year, timeline = T) |>
          e_bar(`Number of females`) |>
          e_bar(`Number of clutches laid`) |>
          e_bar(`Eggs laid`) |>
          e_bar(`Chicks hatched`) |>
          e_bar(`Chicks fledged`) |>
          e_tooltip()
      } else {
        echart <- tmp |>
          mutate(year = as.character(year),
                 across(contains("fledged"), round, digits =2)) |>
          group_by(Clutch) |>
          e_charts(year, timeline = T) |>
          e_bar(`Chicks fledged per clutch`) |>
          e_bar(`Chicks fledged per female`) |>
          e_tooltip()
      }
      
      
      echart
    })
  })
}