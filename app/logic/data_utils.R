box::use(
  htmltools[
    tags,
  ],
)

#' @export
csvDownloadButton <- function(tableId, label = "Download as CSV", filename = "data.csv") {
  htmltools::tags$button(
    label,
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", tableId, filename)
  )
}