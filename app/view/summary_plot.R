box::use(
  echarts4r[
    echarts4rOutput,
    renderEcharts4r,
    e_charts,
    e_bar,
    e_tooltip,
    e_toolbox_feature,
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
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderEcharts4r({
      
      tmp <- data()
      
      req(nrow(tmp) > 0)
      echart <- tmp |>
        mutate(year = as.character(year),
               across(contains("fledged"), round, digits =2)) |>
        e_charts(year) |>
        e_bar(`Chicks fledged per clutch`) |>
        e_bar(`Breeding Success (proxy)`) |>
        e_tooltip() |> 
        e_toolbox_feature(feature = c("saveAsImage"))
      
      echart
    })
  })
}