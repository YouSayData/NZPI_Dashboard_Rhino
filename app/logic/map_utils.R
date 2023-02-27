box::use(
  leaflet,
)

basemap <- leaflet$leaflet() |>
  leaflet$addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) |>
  leaflet$addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) |>
  # add a layers control
  leaflet$addLayersControl(
    baseGroups = c(
      "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )