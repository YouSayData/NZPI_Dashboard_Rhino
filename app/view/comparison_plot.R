box::use(
  echarts4r[
    echarts4rOutput,
    renderEcharts4r,
    e_charts,
    e_bar,
    e_tooltip,
    e_toolbox_feature,
    e_title,
    e_legend,
    e_grid,
    e_y_axis,
    e_axis_formatter,
    e_tooltip_item_formatter,
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
    select,
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
      
      plot_title <- unique(tmp$Site)
      
      if (show_counts()) {
        echart <- tmp |> 
          select(
            Season = year,
            `Number of clutches laid` = Clutches,
            `Eggs laid` = Total_eggs,
            `Chicks hatched` = Total_chicks,
            `Chicks fledged` = Total_fledged,
            `Chicks uplifted` = Total_uplifted,
            `Failed` = Total_failed,
            `Unknown Outcome` = Total_unknown,
            `Active Nests`,
            `Inactive Nests`
          )  |>
          mutate(
            Season = as.character(Season)
          ) |>
          e_charts(Season) |> 
          e_bar(`Number of clutches laid`) |>
          e_bar(`Eggs laid`) |>
          e_bar(`Chicks hatched`) |>
          e_bar(`Chicks fledged`) |>
          e_tooltip() |> 
          e_toolbox_feature(feature = c("saveAsImage")) |> 
          e_title(plot_title, "Counts") |> 
          e_legend(
            orient = 'vertical', 
            right = 0, 
            top = 0
          ) |>
          e_grid(height = "60%", top = "25%")
      } else {
        echart <- tmp |>
          select(
            Season = year,
            `Number of clutches laid` = Clutches,
            `Chicks fledged per clutch`,
            contains("success"),
            `Total Nests`,
            `Active Nests (perc)`,
            `Inactive Nests (perc)`
          )  |>
          mutate(
            across(-Season, ~round(., digits = 2)),
            Season = as.character(Season)
          ) |>
          e_charts(Season) |>  
          e_bar(`Hatching Success (Fertility)`) |>
          e_bar(`Fledging Success (Chicks Survival)`) |>
          e_bar(`Reproductive Success`) |>
          # e_bar(`Breeding Success (proxy)`) |>
          e_tooltip(
            formatter = e_tooltip_item_formatter(
              style = "percent",
              digits = 0
            )
          ) |> 
          e_toolbox_feature(feature = c("saveAsImage")) |>
          e_title(plot_title, "Breeding Success") |> 
          e_legend(
            orient = 'vertical', 
            right = 0, 
            top = 0
          ) |>
          e_grid(height = "60%", top = "25%") |> 
          e_y_axis(
            formatter = e_axis_formatter("percent", digits = 0)
          )
      }
      
      
      echart
    })
  })
}