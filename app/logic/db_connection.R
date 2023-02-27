box::use(
  shiny[onStop],
  RMariaDB,
  pool[dbPool, poolClose]
)

#' @export
penguin_data_pool <- dbPool(
  RMariaDB$MariaDB(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  username = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PW")
)

#' @export
db_connected <- function() {
  print(DBI::dbIsValid(penguin_data_pool))
}

onStop(function() {
  message("before close - is valid? ", DBI::dbIsValid(penguin_data_pool))
  poolClose(penguin_data_pool)
  message("after close - is valid? ", DBI::dbIsValid(penguin_data_pool))
})