box::use(
  app / logic / db_connection,
  app / logic / db_functions[
    db_list_tables,
    db_collect,
  ],
  here[
    here,
  ],
  stringr[
    str_detect,
    str_c,
  ],
  purrr[
    walk
  ],
)

save_tbl <- function(table_name, con) {
  db_collect(table_name, con) |>
    saveRDS(
      here(
        "rds",
        str_c(
          table_name, ".rds"
        )
      )
    )
}

base_tables <- db_list_tables(db_connection$penguin_data_pool) |>
  sort()

base_tables <- base_tables[str_detect(base_tables, "BASE")]

base_tables |>
  walk(save_tbl, con = db_connection$penguin_data_pool)
