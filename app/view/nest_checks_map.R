box::use(
  leaflet[
    leafletOutput,
    renderLeaflet,
    addMarkers,
    setView,
  ],
  shiny[moduleServer, NS, req],
  dplyr[
    distinct,
    filter,
    select,
  ]
)

box::use(
  app/logic/map_utils,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("map"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet(
      {
        req(nrow(data()) > 0)
        
        tmp  <- data() |> 
          select(Nest, nest_lat, nest_lon) |>
          filter(nest_lat != 0, nest_lon != 0) |> 
          distinct()
        
        req(nrow(tmp) > 0)
        
        map_utils$basemap |>
          addMarkers(
            lat = tmp$nest_lat,
            lng = tmp$nest_lon,
            label = tmp$Nest
          ) |> 
          setView(tmp$nest_lon, tmp$nest_lat, zoom = 17)
      }
    )
  })
}