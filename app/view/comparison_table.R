box::use(
  reactable[
    reactable,
    colDef,
    colFormat,
  ],
  dplyr[
    select,
    across,
    mutate,
  ],
  shiny[
    tagList,
    moduleServer,
    NS,
    div,
    uiOutput,
    renderUI,
  ],
  tippy[
    tippy,
  ],
  stringr[
    str_c,
  ],
)

box::use(
  app / logic / data_utils,
)

with_tooltip <- function(value, tooltip, ...) {
  div(
    style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
    tippy(value, tooltip, ...)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("table"))
}

#' @export
server <- function(id, data, show_counts) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderUI({
      table_id <- str_c(id, "react_tbl", sep = "-")

      if (show_counts()) {
        display_data <- data() |>
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
            `Inactive Nests`,
            `Active - not breeding`
          ) |>
          mutate(
            Season = as.character(Season)
          )

        tagList(
          data_utils$csvDownloadButton(
            table_id
          ),
          display_data |>
            reactable(
              highlight = T,
              defaultPageSize = 5,
              columns = list(
                Season = colDef(
                  rowHeader = T,
                  align = "center",
                  vAlign = "center"
                ),
                `Number of clutches laid` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Eggs laid` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Chicks hatched` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Chicks fledged` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Chicks uplifted` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Failed` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Unknown Outcome` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Active Nests` = colDef(
                  header = with_tooltip(
                    "Active Nests",
                    "Nest Sites with Breeding (Proxy for Pairs)"
                  ),
                  align = "center",
                  vAlign = "center"
                ),
                `Inactive Nests` = colDef(
                  header = with_tooltip(
                    "Inactive Nests",
                    "Nest Sites without Breeding or Clear Signs of Penguin Activity"
                  ),
                  align = "center",
                  vAlign = "center"
                ),
                `Active - not breeding` = colDef(
                  header = with_tooltip(
                    "Active (Not Breeding)",
                    "Other Penguin Activity (e.g. Loafing)"
                  ),
                  align = "center",
                  vAlign = "center"
                )
              )
            )
        )
      } else {
        display_data <- data() |>
          select(
            Season = year,
            `Number of clutches laid` = Clutches,
            `Chicks fledged per clutch`,
            contains("success"),
            `Total Nests`,
            `Active Nests (perc)`,
            `Inactive Nests (perc)`,
            `Active - not breeding (perc)`
          ) |>
          mutate(
            across(-Season, ~ round(., digits = 2)),
            Season = as.character(Season)
          )

        tagList(
          data_utils$csvDownloadButton(
            table_id
          ),
          display_data |>
            reactable(
              highlight = T,
              defaultPageSize = 5,
              columns = list(
                `Season` = colDef(
                  rowHeader = T,
                  align = "center",
                  vAlign = "center"
                ),
                `Number of clutches laid` = colDef(
                  align = "center",
                  vAlign = "center"
                ),
                `Breeding Success (proxy)` = colDef(
                  header = with_tooltip(
                    "Breeding Success (proxy)",
                    "Chicks Fledged / Active Nests"
                  ),
                  format = colFormat(digits = 2),
                  align = "center",
                  vAlign = "center"
                ),
                `Chicks fledged per clutch` = colDef(
                  format = colFormat(digits = 2),
                  align = "center",
                  vAlign = "center"
                ),
                `Hatching Success (Fertility)` = colDef(
                  header = with_tooltip(
                    "Hatching Success (Fertility)",
                    "Chicks Hatched / Eggs Laid"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Fledging Success (Chicks Survival)` = colDef(
                  header = with_tooltip(
                    "Fledging Success (Chick Survival)",
                    "Chicks Fledged / Chicks Hatched"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Reproductive Success` = colDef(
                  header = with_tooltip(
                    "Reproductive Success",
                    "Chicks Fledged / Eggs Laid"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Active Nests (perc)` = colDef(
                  header = with_tooltip(
                    "Active Nests",
                    "Active Nests / Total Nests"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Inactive Nests (perc)` = colDef(
                  header = with_tooltip(
                    "Inactive Nests",
                    "Inactive Nests / Total Nests"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Active - not breeding (perc)` = colDef(
                  header = with_tooltip(
                    "Active (Not Breeding)",
                    "Other Penguin Activity (e.g. Loafing)"
                  ),
                  format = colFormat(percent = TRUE, digits = 0),
                  align = "center",
                  vAlign = "center"
                ),
                `Total Nests` = colDef(
                  align = "center",
                  vAlign = "center"
                )
              ),
              elementId = table_id
            )
        )
      }
    })
  })
}
