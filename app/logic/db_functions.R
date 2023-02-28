box::use(
  DBI[
    dbListTables,
  ],
  dplyr[
    tbl,
    collect,
    semi_join,
    filter,
    select,
    distinct,
  ],
)

#' @export
db_list_tables <- function(db_pool) {
  dbListTables(db_pool, tbl_name)
}

#' @export
db_collect <- function(tbl_name, db_pool) {
  tbl(db_pool, tbl_name) |>
    collect()
}

#' @export
db_view <- function(tbl_name, db_pool) {
  tbl(db_pool, tbl_name)
}

#' @export
db_get_nests_by_site <- function(.site_id, db_pool) {
  db_view("nest_id", db_pool) |>
    semi_join(db_view("nest_checks", db_pool) |>
                      select(id = nest_id, site) |> 
                      distinct()) |> 
    filter(site == .site_id) |>
    select(site, nest_id = id, nest_name = nest_id) |>
    distinct() |>
    collect()
}


