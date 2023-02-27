box::use(
  reactable[
    reactableOutput,
    renderReactable,
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
    moduleServer, 
    NS,
    div,
  ],
  tippy[
    tippy,
  ],
)

with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("table"))
}

#' @export
server <- function(id, data, show_counts) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      
      if (show_counts()) {
        display_data <- data() |>
         select(
           `Clutch`,
           `Number of females`,
           `Number of clutches laid`,
           `Eggs laid`,
           `Chicks hatched`,
           `Chicks fledged`
         ) 
        
        display_data |>
          reactable(
            highlight = T,
            defaultPageSize = 5,
            columns = list(
              `Clutch` = colDef(rowHeader = T,
                                header = with_tooltip("Clutch", 
                                                      "Single clutch: 1 or 2 eggs</br>
                                                    Replacement clutch: clutch laid after first clutch failed</br>
                                                    All single broods: single and replacement clutches combined"),
                                vAlign = "center"),
              `Number of females` = colDef(align = "center",
                                           vAlign = "center"),
              `Number of clutches laid` = colDef(align = "center",
                                                 vAlign = "center"),
              `Eggs laid` = colDef(align = "center",
                                   vAlign = "center"),
              `Chicks hatched` = colDef(align = "center",
                                        vAlign = "center"),
              `Chicks fledged` = colDef(align = "center",
                                        vAlign = "center")
            )
          )
      } else {
        display_data <- data() |>
          select(
            `Clutch`,
            `Chicks fledged per female`,
            `Chicks fledged per clutch`,
            contains("success")
          )  |>
          mutate(
            across(contains("fledged"), round, digits = 2)
          )
        
        display_data |>
          reactable(
            filterable = F,
            highlight = T,
            defaultPageSize = 5,
            columns = list(
              `Clutch` = colDef(rowHeader = T,
                                header = with_tooltip("Clutch", 
                                                      "Single clutch: 1 or 2 eggs</br>
                                                    Replacement clutch: clutch laid after first clutch failed</br>
                                                    All single broods: single and replacement clutches combined"),
                                vAlign = "center"),
              `Chicks fledged per female` = colDef(align = "center",
                                                   vAlign = "center"),
              `Chicks fledged per clutch` = colDef(align = "center",
                                                   vAlign = "center"),
              `Hatching success (hatched/laid)` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                         align = "center",
                                                         vAlign = "center"),
              `Fledging success (fledged/hatched)` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                            align = "center",
                                                            vAlign = "center"),
              `Egg success (fledged/laid)` = colDef(format = colFormat(percent = TRUE, digits = 1),
                                                    align = "center",
                                                    vAlign = "center")
            )
          )
      }
    })
  })
}