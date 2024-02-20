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
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
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
           `Number of clutches laid` = Clutches,
           `Eggs laid` = Total_eggs,
           `Chicks hatched` = Total_chicks,
           `Chicks fledged` = Total_fledged,
           `Chicks uplifted` = Total_uplifted,
           `Failed` = Total_failed,
           `Unknown Outcome` = Total_unknown,
           `Active Nests`,
           `Inactive Nests`
         ) 
        
        tagList(
          data_utils$csvDownloadButton(
            table_id),
        display_data |>
          reactable(
            highlight = T,
            defaultPageSize = 5,
            columns = list(
              `Number of clutches laid` = colDef(rowHeader = T,
                                                 align = "center",
                                                 vAlign = "center"),
              `Eggs laid` = colDef(align = "center",
                                   vAlign = "center"),
              `Chicks hatched` = colDef(align = "center",
                                        vAlign = "center"),
              `Chicks fledged` = colDef(align = "center",
                                        vAlign = "center"),
              `Chicks uplifted` = colDef(align = "center",
                                        vAlign = "center"),
              `Failed` = colDef(align = "center",
                                        vAlign = "center"),
              `Unknown Outcome` = colDef(align = "center",
                                        vAlign = "center")
            ),
            elementId = table_id
          )
        )
      } else {
        display_data <- data() |>
          select(
            `Number of clutches laid` = Clutches,
            `Chicks fledged per clutch`,
            contains("success"),
            `Total Nests`,
            `Active Nests (perc)`,
            `Inactive Nests (perc)`
          )  |>
          mutate(
            across(contains("fledged"), round, digits = 2)
          )
        
        tagList(
          data_utils$csvDownloadButton(
            table_id),
        display_data |>
          reactable(
            filterable = F,
            highlight = T,
            defaultPageSize = 5,
            columns = list(
              `Number of clutches laid` = colDef(rowHeader = T,
                                vAlign = "center"),
              `Breeding Success (proxy)` = colDef(align = "center",
                                                   vAlign = "center"),
              `Chicks fledged per clutch` = colDef(align = "center",
                                                   vAlign = "center"),
              `Hatching Success (Fertility)` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                         align = "center",
                                                         vAlign = "center"),
              `Fledging Success (Chicks Survival)` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                            align = "center",
                                                            vAlign = "center"),
              `Reproductive Success` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                    align = "center",
                                                    vAlign = "center"),
              `Breeding Success (proxy)` = colDef(format = colFormat(percent = F, digits = 2),
                                                  align = "center",
                                                  vAlign = "center"),
              `Active Nests (perc)` = colDef(format = colFormat(percent = TRUE, digits = 2),
                                             align = "center",
                                             vAlign = "center"),
              `Inactive Nests (perc)` = colDef(format = colFormat(percent = TRUE, digits = 2),
                                               align = "center",
                                               vAlign = "center")
            ),
            elementId = table_id
          )
        )
      }
    })
  })
}