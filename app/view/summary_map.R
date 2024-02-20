box::use(
  leaflet[
    leafletOutput,
    renderLeaflet,
    addMarkers,
  ],
  shiny[moduleServer, NS, req],
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
        
        tmp  <- data()
        
        map_utils$basemap |>
          addMarkers(
            lat = tmp$site_lat,
            lng = tmp$site_lon,
            label = tmp$Site
          )
      }
    )
  })
}