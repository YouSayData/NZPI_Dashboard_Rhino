box::use(
  shiny[reactive],
  pool[dbPool],
  RMariaDB,
  DBI,
  dbplyr,
  dplyr,
  tidyr[pivot_wider, tibble],
  shiny[req, isolate],
)

#' @export
db_list_tables <- function(db_pool) {
  DBI$dbListTables(db_pool, tbl_name)
}

#' @export
db_collect <- function(tbl_name, db_pool) {
  dplyr$tbl(db_pool, tbl_name) |>
    dplyr$collect()
}

#' @export
db_view <- function(tbl_name, db_pool) {
  dplyr$tbl(db_pool, tbl_name)
}

#' @export
db_get_nests_by_site <- function(.site_id, db_pool) {
  db_view("nest_id", db_pool) |>
    dplyr$semi_join(db_view("nest_checks", db_pool) |>
                      dplyr$select(id = nest_id, site) |> 
                      dplyr$distinct()) |> 
    dplyr$filter(site == .site_id) |>
    dplyr$select(site, nest_id = id, nest_name = nest_id) |>
    dplyr$distinct() |>
    dplyr$collect()
}


