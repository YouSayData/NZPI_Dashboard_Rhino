box::use(
  here[
    here,
  ],
  dplyr[
    arrange,
    select,
    distinct,
    pull,
  ],
  tidyr[
    pivot_wider,
  ],
)

BASE_sites <- readRDS(
  here("rds", "BASE_sites.rds")
)

BASE_species <- readRDS(
  here("rds", "BASE_species.rds")
)

BASE_interaction <- readRDS(
  here("rds", "BASE_interaction.rds")
)

BASE_people <- readRDS(
  here("rds", "BASE_people.rds")
)

BASE_nest_status <- readRDS(
  here("rds", "BASE_nest_status.rds")
)

sites_choices <- BASE_sites |>
  select(site) |>
  distinct() |>
  arrange(site) |>
  pull(site)

area_choices <- BASE_sites |>
  select(area) |>
  distinct() |>
  arrange(area) |>
  pull(area)

region_choices <- BASE_sites |>
  select(region) |>
  distinct() |>
  arrange(region) |>
  pull(region)
