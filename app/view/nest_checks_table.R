box::use(
  reactable[
    reactable,
    colDef,
    colFormat,
  ],
  shiny[
    tagList,
    moduleServer, 
    NS,
    div,
    uiOutput,
    renderUI,
  ],
  dplyr[
    filter,
    select,
    arrange,
    desc,
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
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderUI({
      table_id <- str_c(id, "react_tbl", sep = "-")
      
      tagList(
        data_utils$csvDownloadButton(
          table_id),
        data() |> 
        select(
          Date,
          Time,
          Interaction,
          `Latest Nest Status`,
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
            `Latest Nest Status` = colDef(
              vAlign = "center"
            ),
            `Number of Adults` = colDef(align = "center",
                                         vAlign = "center"),
            `Number of Eggs` = colDef(align = "center",
                                         vAlign = "center"),
            `Number of Chicks` = colDef(align = "center",
                                         vAlign = "center")
          ),
          elementId = table_id
        )
        )
    })
  })
}