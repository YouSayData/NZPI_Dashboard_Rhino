box::use(
  reactable[
    reactableOutput,
    renderReactable,
    reactable,
    colDef,
    colFormat,
  ],
  shiny[
    moduleServer, 
    NS,
    ],
  dplyr[
    select,
    arrange,
    desc,
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
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      data() |>
        select(
          Date,
          Time,
          Interaction,
          `Nest Status`,
          `Number of Adults` = number_of_adults,
          `Number of Eggs` = number_of_eggs,
          `Number of Chicks` = number_of_chicks
        ) |>
        arrange(
          desc(Date),
          desc(Time)
          ) |>
        reactable(
          filterable = F,
          highlight = T,
          defaultPageSize = 5,
          columns = list(
            `Date` = colDef(rowHeader = T,
                            format = colFormat(
                              date = T
                            ),
                            vAlign = "center"
                            ),
            `Time` = colDef(
                            vAlign = "center"
            ),
            `Interaction` = colDef(
              vAlign = "center"
            ),
            `Nest Status` = colDef(
              vAlign = "center"
            ),
            `Number of Adults` = colDef(align = "center",
                                         vAlign = "center"),
            `Number of Eggs` = colDef(align = "center",
                                         vAlign = "center"),
            `Number of Chicks` = colDef(align = "center",
                                         vAlign = "center")
          )
        )
    })
  })
}