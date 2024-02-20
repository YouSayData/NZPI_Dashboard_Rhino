box::use(
  DBI[
    dbListTables,
  ],
  dplyr[
    across,
    anti_join,
    count,
    ungroup,
    tibble,
    tbl,
    rename,
    collect,
    semi_join,
    filter,
    select,
    distinct,
    left_join,
    mutate,
    summarise,
    starts_with,
    group_by,
    na_if,
  ],
  tidyr[
    replace_na,
    pivot_wider,
  ],
  here[
    here,
  ],
  pins[
    board_folder,
    pin_read,
    pin_write,
  ],
  RMariaDB,
  pool[
    dbPool, 
    poolClose,
    ],
  stringr[
    str_to_title,
    str_c,
    str_extract,
  ],
  lubridate[
    date,
    hour,
    minute,
  ],
)

penguin_data_pool <- dbPool(
  RMariaDB$MariaDB(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  username = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PW")
)

db_list_tables <- function(db_pool) {
  dbListTables(db_pool, tbl_name)
}

db_collect <- function(tbl_name, db_pool) {
  tbl(db_pool, tbl_name) |>
    collect()
}

db_view <- function(tbl_name, db_pool) {
  tbl(db_pool, tbl_name)
}

nest_summary <- db_collect("nest_summary", penguin_data_pool)
colnames(nest_summary) <- str_to_title(colnames(nest_summary))

colnames(nest_summary)
nest_summary <- nest_summary |> 
  rename(
    Site = Site_long,
    year = D_breeding_season
    )

base_sites <- db_collect("BASE_sites", penguin_data_pool)

site_summary_latlon <- base_sites |> 
  select(Region = region, Area = area, Site = site, site_lat = latitude, site_lon = longitude)

site_summary_nest_status <- nest_summary |> 
  select(Region, Area, Site, year, Season_nest_status_long) |> 
  group_by(Region, Area, Site, year) |> 
  count(Season_nest_status_long) |> 
  ungroup() |> 
  pivot_wider(names_from = Season_nest_status_long, values_from = n, values_fill = 0)

site_summary_counts <- nest_summary |> 
  group_by(Region, Area, Site, year) |> 
  summarise(
    Clutches = sum(Number_clutches, na.rm = T), 
    across(starts_with("Total"), \(x) sum(x, na.rm = T)))

site_summary <- site_summary_nest_status |> 
  left_join(site_summary_counts) |> 
  left_join(site_summary_latlon) |> 
  mutate(
    `Chicks fledged per clutch` = Total_fledged / Clutches,
    `Hatching Success (Fertility)` = Total_chicks / Total_eggs,
    `Fledging Success (Chicks Survival)` = Total_fledged / Total_chicks,
    `Reproductive Success` = Total_fledged / Total_eggs,
    `Breeding Success (proxy)` = Total_fledged / (`Active - failed` + `Active - fledged`), 
    across(`Chicks fledged per clutch`:`Breeding Success (proxy)`, \(x) dplyr::na_if(x, NaN)),
    across(`Chicks fledged per clutch`:`Breeding Success (proxy)`, \(x) dplyr::na_if(x, Inf)),
    `Active Nests` = `Active - failed` + `Active - fledged` + `Active - insufficient data` + `Active - for investigation`,
    `Inactive Nests` = Inactive,
    `Total Nests` = `Active Nests` + `Inactive Nests`,
    `Active Nests (perc)` = `Active Nests` / `Total Nests`,
    `Inactive Nests (perc)` = `Inactive Nests` / `Total Nests`,
    across(is.numeric, round, digits = 4)
    )

site_comparison <- site_summary

nest_summary_nest_status <- nest_summary |> 
  select(Region, Area, Site, Nest_id_long, year, Season_nest_status_long) |> 
  group_by(Region, Area, Site, Nest_id_long, year) |> 
  count(Season_nest_status_long) |> 
  ungroup() |> 
  pivot_wider(names_from = Season_nest_status_long, values_from = n, values_fill = 0)

nest_summary_counts <- nest_summary |> 
  group_by(Region, Area, Site, Nest_id_long, year) |> 
  summarise(
    Clutches = sum(Number_clutches, na.rm = T), 
    across(starts_with("Total"), \(x) sum(x, na.rm = T)))

nest_summary <- nest_summary_nest_status |>
  left_join(nest_summary_counts) |> 
  left_join(site_summary_latlon) |> 
  mutate(
    `Chicks fledged per clutch` = Total_fledged / Clutches,
    `Hatching Success (Fertility)` = Total_chicks / Total_eggs,
    `Fledging Success (Chicks Survival)` = Total_fledged / Total_chicks,
    `Reproductive Success` = Total_fledged / Total_eggs,
    `Breeding Success (proxy)` = Total_fledged / (`Active - failed` + `Active - fledged`), 
    across(`Chicks fledged per clutch`:`Breeding Success (proxy)`, \(x) dplyr::na_if(x, NaN)),
    across(`Chicks fledged per clutch`:`Breeding Success (proxy)`, \(x) dplyr::na_if(x, Inf))
  )

nest_checks <- db_collect("nest_checks", penguin_data_pool)
nest_report_exclusions <- db_collect("nest_report_exclusions", penguin_data_pool)
nest_ids <- db_collect("nest_id", penguin_data_pool)
base_interaction <- db_collect("BASE_interaction", penguin_data_pool)
nest_report_exclusions_names <- nest_report_exclusions |> 
  left_join(
    nest_ids |> 
      select(
        nest_db_id = id, 
        Nest_id_long = nest_id)
    ) |> 
  select(Nest_id_long)

nest_checks <- nest_checks |> 
  select(-ends_with("down_pct")) |>
  select(-starts_with("s_")) |>
  rename(
    site_name = site_long,
    nest_name = nest_id_long
    ) |> 
  left_join(
    nest_ids |> 
      select(
        nest_id = id, 
        nest_lat = latitude, 
        nest_lon = longitude, 
        nest_img = image_url)
    ) |> 
  left_join(
    base_sites |>
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
    base_interaction |>
      select(interaction = id, type, action, wand_code) |>
      replace_na(list(wand_code = "-")) |>
      mutate(interaction_description = str_c(type, action, wand_code, sep = " | ")) |>
      select(interaction, interaction_description)
  ) |> 
  mutate(
    nest_img = str_extract(nest_img, "^[^ ]+"),
    Species = "Kororā",
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
  )  |> 
  select(Date,
         Time,
         Region,
         Area,
         Site = site_name,
         Nest = nest_name,
         `Latest Nest Status` = latest_nest_status_long,
         Species,
         Season = d_breeding_season,
         Cycle = breeding_cycle,
         Interaction = interaction_description,
         # Observer,
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
  
board <- board_folder(here("pins"))
pin_write(board, nest_summary, "nest_summary")
pin_write(board, nest_checks, "nest_checks")
pin_write(board, nest_ids, "nest_ids")
pin_write(board, site_summary, "site_summary")
pin_write(board, site_comparison, "site_comparison")


auth_pin <- tibble(
  user = c("admin", "wellington"),
  password = c("admin", "wellington"),
  access = c("all", "wellington")
)

pin_write(board, auth_pin, "auth_pin")
