box::use(
  dplyr[
    anti_join,
    filter,
    collect,
    select,
    left_join,
    mutate,
    rename,
  ],
  tidyr[
    replace_na,
  ],
  stringr[
    str_c,
  ],
  lubridate[
    date,
    hour,
    minute,
  ],
  pins[
    board_folder,
    pin_read,
  ],
  here[
    here,
  ],
)

box::use(
  app / logic / globals,
 # app / logic / db_connection,
#  app / logic / db_functions,
)

#' @export
load_data <- function(dataset) {
  board <- board_folder(here("pins"))
  pin_read(board, dataset)
}

#' @export
load_summary_data <- function(dataset) {
  board <- board_folder(here("pins"))
  pin_read(board, dataset)
}

#' @export
load_nest_check_data_old <- function() {
  db_functions$db_collect(
    "nest_checks",
    db_connection$penguin_data_pool
  ) |>
    select(-ends_with("down_pct")) |>
    select(-starts_with("s_")) |>
    left_join(
      globals$BASE_sites |>
        select(
          site_id = id,
          site_name = site,
          site_lat = latitude,
          site_long = longitude,
          Region = region,
          Area = area
        ),
      by = c("site" = "site_id")
    ) |>
    left_join(
      globals$BASE_species |>
        select(1:4) |>
        replace_na(
          list(
            te_reo = "?",
            common_name = "?",
            scientific_name = "?"
          )
        ) |>
        mutate(species_name = str_c(
          te_reo, " (Te Reo Māori); ",
          common_name, " (English); ",
          scientific_name, " (Scientific)"
        )) |>
        select(species_id = id, species_name),
      by = c("nest_check_species" = "species_id")
    ) |>
    left_join(
      db_functions$db_collect(
        "nest_id",
        db_connection$penguin_data_pool
      ) |>
        select(
          nest_id = id,
          site,
          nest_name = nest_id,
          nest_lat = latitude,
          nest_long = longitude,
          nest_img = image_url
        ),
      by = c("site", "nest_id")
    ) |>
    left_join(
      globals$BASE_interaction |>
        select(interaction = id, type, action, wand_code) |>
        replace_na(list(wand_code = "-")) |>
        mutate(interaction_description = str_c(type, action, wand_code, sep = " | ")) |>
        select(interaction, interaction_description)
    ) |>
    left_join(
      globals$BASE_people |>
        select(nest_observer = id, Observer = name)
    ) |>
    left_join(
      globals$BASE_nest_status |>
        select(nest_status_raw = id, `Nest Status` = status)
    ) |>
    mutate(
      Date = date(nest_check_datetime),
      Time = str_c(
        formatC(hour(nest_check_datetime),
          width = 2,
          flag = 0
        ),
        ":",
        formatC(minute(nest_check_datetime),
          width = 2,
          flag = 0
        )
      )
    ) |>
    select(Date,
      Time,
      Region,
      Area,
      Site = site_name,
      Nest = nest_name,
      `Nest Status`,
      Species = species_name,
      Season = d_breeding_season,
      Cycle = breeding_cycle,
      Interaction = interaction_description,
      Observer,
      starts_with("number"),
      starts_with("adult"),
      starts_with("chick"),
      nest_lat,
      nest_long,
      nest_img,
      site_lat,
      site_long,
      nest_check_datetime
    )
}

#' @export
load_nest_check_data <- function() {
  
  board <- board_folder(here("pins"))
  exclusions <- pin_read(board, "nest_report_exclusions")
  nest_ids <- pin_read(board, "nest_ids")
  nest_checks <- pin_read(board, "nest_checks") |> 
    anti_join(exclusions, by = c("nest_id" = "nest_db_id"))
  
  nest_checks |>
    select(-ends_with("down_pct")) |>
    select(-starts_with("s_")) |>
    rename(site_name = site_long) |> 
    left_join(
      globals$BASE_sites |>
        select(
          site_id = id,
          site_lat = latitude,
          site_lon = longitude,
          Region = region,
          Area = area
        ),
      by = c("site" = "site_id")
    ) |>
    left_join(
      globals$BASE_species |>
        select(1:4) |>
        replace_na(
          list(
            te_reo = "?",
            common_name = "?",
            scientific_name = "?"
          )
        ) |>
        mutate(species_name = str_c(
          te_reo, " (Te Reo Māori); ",
          common_name, " (English); ",
          scientific_name, " (Scientific)"
        )) |>
        select(species_id = id, species_name),
      by = c("nest_check_species" = "species_id")
    ) |>
    left_join(
      nest_ids |>
        select(
          nest_id = id,
          site,
          nest_name = nest_id,
          nest_lat = latitude,
          nest_lon = longitude,
          nest_img = image_url
        ),
      by = c("site", "nest_id")
    ) |>
    left_join(
      globals$BASE_interaction |>
        select(interaction = id, type, action, wand_code) |>
        replace_na(list(wand_code = "-")) |>
        mutate(interaction_description = str_c(type, action, wand_code, sep = " | ")) |>
        select(interaction, interaction_description)
    ) |>
    left_join(
      globals$BASE_people |>
        select(nest_observer = id, Observer = name)
    ) |>
    left_join(
      globals$BASE_nest_status |>
        select(nest_status_raw = id, `Nest Status` = status)
    ) |>
    mutate(
      Date = date(nest_check_datetime),
      Time = str_c(
        formatC(hour(nest_check_datetime),
                width = 2,
                flag = 0
        ),
        ":",
        formatC(minute(nest_check_datetime),
                width = 2,
                flag = 0
        )
      )
    ) |>
    select(Date,
           Time,
           Region,
           Area,
           Site = site_name,
           Nest = nest_name,
           `Latest Nest Status` = latest_nest_status_long,
           Species = species_name,
           Season = d_breeding_season,
           Cycle = breeding_cycle,
           Interaction = interaction_description,
           Observer,
           starts_with("number"),
           starts_with("adult"),
           starts_with("chick"),
           nest_lat,
           nest_lon,
           nest_img,
           site_lat,
           site_lon,
           nest_check_datetime
    )
}

#' @export
update_nest_data <- function(dataset, data_filters) {
  site_filter <- data_filters["site_filter"] |>
    unlist()

  nest_filter <- data_filters["nest_filter"] |>
    unlist()

  dataset <- dataset |>
      filter(Site %in% site_filter,
             Nest %in% nest_filter,)
}

#' @export
update_summary_data <- function(dataset, data_filters) {
  area_filter <- data_filters["area_filter"] |>
    unlist()
  
  site_filter <- data_filters["site_filter"] |>
    unlist()
  
  season_filter <- data_filters["season_filter"] |>
    unlist()
  
  dataset <- dataset |>
    filter(
      Area %in% area_filter,
      Site %in% site_filter,
      year %in% season_filter
      )
}

#' @export
update_comparison_data <- function(dataset, data_filters) {
  area_filter <- data_filters["area_filter"] |>
    unlist()
  
  site_filter <- data_filters["site_filter"] |>
    unlist()
  
  dataset <- dataset |>
    filter(
      Area %in% area_filter,
      Site %in% site_filter
      )
}
