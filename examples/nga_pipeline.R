################################################################################
# mipR pipeline example: Nigeria, admin 1
################################################################################

# install the package from GitHub
remotes::install_github("truenomad/mipR@main")

## ---------------------------------------------------------------------------##
# Boundaries -------------------------------------------------------------------
## ---------------------------------------------------------------------------##

shp <- mipR::get_boundaries(
  country = "NGA",
  adm_level = 1,
  source = "who",
  cache_dir = here::here("01_data/1a_shapefiles")
)

## ---------------------------------------------------------------------------##
# Source data ------------------------------------------------------------------
## ---------------------------------------------------------------------------##

# IHME geospatial MCV1 and MCV2 coverage (MCV1 rasters are 26-layer stacks,
# one layer per year from 2000 to 2025)
mipR::get_ihme_mcv(
  dest_dir = here::here("01_data/1c_ihme/raw")
)

# UN WPP 2024 rates, age distribution and mortality, covering every year of
# the cohort reconstruction
demog <- mipR::get_demog_wpp(
  country = "NGA",
  year = 2000:2030
)

# WorldPop single-year age rasters (age 0 is also used as population
# weights); years before 2015 come from the legacy 2000-2020 data
mipR::get_worldpop_single_age(
  country_codes = "NGA",
  years = 2000:2030,
  ages = 0:15,
  out_dir = here::here("01_data/1b_worldpop/raw")
)

# WorldPop total population rasters, kept apart from the age rasters; the
# population aged 15 and over is the total less ages 0 to 14
mipR::get_worldpop(
  country_codes = "NGA",
  years = 2000:2030,
  dest_dir = here::here("01_data/1b_worldpop/raw_total")
)

## ---------------------------------------------------------------------------##
# Population by age and admin unit ---------------------------------------------
## ---------------------------------------------------------------------------##

# set up pop paths
pop_path <- here::here(
  "01_data/1b_worldpop/processed/nga_aged_pop_2000_2030.rds"
)

if (file.exists(pop_path)) {
  # read file
  aged_pop_admin <- readRDS(pop_path)
} else {
  # extract pop
  aged_pop_admin <- mipR::extract_raster(
    rasters = here::here("01_data/1b_worldpop/raw"),
    shapefile = shp,
    id_cols = c("adm0", "adm1", "adm1_code"),
    pattern = "_age\\d+_\\d{4}\\.tif$"
  ) |>
    dplyr::rename(pop = sum)

  # save extracted df
  dir.create(dirname(pop_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    aged_pop_admin,
    pop_path
  )
}

# reference date of each year: legacy years refer to 1 July, R2025A years
# to 1 January
aged_pop_admin <- aged_pop_admin |>
  dplyr::mutate(ref_date = mipR::worldpop_reference_date(year))

## ---------------------------------------------------------------------------##
# Population by broad age band -------------------------------------------------
## ---------------------------------------------------------------------------##

# total population by admin unit and year
total_pop_path <- here::here(
  "01_data/1b_worldpop/processed/nga_total_pop_2000_2030.rds"
)

if (file.exists(total_pop_path)) {
  total_pop_admin <- readRDS(total_pop_path)
} else {
  total_pop_admin <- mipR::extract_raster(
    rasters = here::here("01_data/1b_worldpop/raw_total"),
    shapefile = shp,
    id_cols = c("adm0", "adm1", "adm1_code")
  ) |>
    dplyr::select("adm0", "adm1", "adm1_code", "year", total_pop = "sum")

  saveRDS(total_pop_admin, total_pop_path)
}

# under 5, 5-9 and 10-14 from the single-year ages, and 15 and over as the
# total less ages 0 to 14
pop_bands <- aged_pop_admin |>
  dplyr::filter(age < 15) |>
  dplyr::mutate(
    age_band = dplyr::case_when(
      age < 5 ~ "<5",
      age < 10 ~ "5-9",
      TRUE ~ "10-14"
    )
  ) |>
  dplyr::summarise(
    pop = sum(pop),
    .by = c("adm0", "adm1", "adm1_code", "year", "age_band")
  )

pop_15_plus <- pop_bands |>
  dplyr::summarise(
    pop_u15 = sum(pop),
    .by = c("adm0", "adm1", "adm1_code", "year")
  ) |>
  dplyr::left_join(
    total_pop_admin,
    by = c("adm0", "adm1", "adm1_code", "year")
  ) |>
  dplyr::transmute(
    adm0, adm1, adm1_code, year,
    age_band = "15+",
    pop = total_pop - pop_u15
  )

pop_bands <- dplyr::bind_rows(pop_bands, pop_15_plus) |>
  dplyr::mutate(
    age_band = factor(age_band, levels = c("<5", "5-9", "10-14", "15+"))
  ) |>
  dplyr::arrange(adm1, year, age_band)

# a negative 15+ count means the age rasters and the total disagree
pop_bands |>
  dplyr::filter(age_band == "15+", pop < 0)

## ---------------------------------------------------------------------------##
# MCV1 coverage by admin unit --------------------------------------------------
## ---------------------------------------------------------------------------##

# the mean, lower and upper rasters, each weighted by the under-one
# population of the same year (or the nearest)
mcv1 <- purrr::map(c("mean", "lower", "upper"), \(bound) {
  mipR::extract_raster(
    rasters = here::here(
      paste0("01_data/1c_ihme/raw/mcv1_coverage_", bound, "_2000_2025.tif")
    ),
    shapefile = shp,
    id_cols = c("adm0", "adm1", "adm1_code"),
    fun = "weighted_mean",
    pop_rasters = here::here("01_data/1b_worldpop/raw"),
    pop_pattern = "_age00_\\d{4}\\.tif$",
    quiet = TRUE
  ) |>
    dplyr::select("adm0", "adm1", "adm1_code", "year", "weighted_mean") |>
    dplyr::rename(!!paste0("coverage_", bound) := "weighted_mean")
}) |>
  purrr::reduce(dplyr::left_join, by = c("adm0", "adm1", "adm1_code", "year"))

## ---------------------------------------------------------------------------##
# MCV2 coverage by admin unit --------------------------------------------------
## ---------------------------------------------------------------------------##

# IHME publishes MCV2 only as admin tables, with its own unit names
mcv2 <- mipR::read_ihme_mcv_admin(
  here::here("01_data/1c_ihme/raw"),
  dose = "mcv2",
  adm_level = 1,
  country = "NGA"
)

# match IHME unit names to the boundaries without prompting; names that
# cannot be matched are left as they are, and saved decisions are reused
mcv2 <- sntutils::prep_geonames(
  target_df = mcv2,
  lookup_df = sf::st_drop_geometry(shp),
  level0 = "adm0",
  level1 = "adm1",
  cache_path = here::here("01_data/1c_ihme/processed/geoname_cache.rds"),
  interactive = FALSE
)

# add the boundary codes, which come from the matched names
mcv2 <- dplyr::left_join(
  mcv2,
  shp |>
    sf::st_drop_geometry() |>
    dplyr::select("adm0", "adm1", "adm1_code"),
  by = c("adm0", "adm1")
)

## ---------------------------------------------------------------------------##
# Campaigns --------------------------------------------------------------------
## ---------------------------------------------------------------------------##

# campaign records, survey indicators and diagnostics, prepared by
# examples/nga_campaign_data.R with mipR::prepare_campaigns()
campaigns <- readRDS(
  here::here("01_data/1d_campaigns/processed/nga_campaigns_adm1.rds")
)

# problems to resolve before the campaigns are applied to cohorts, such as
# the 2017-2018 state dates, which have to be completed by hand
campaigns$diagnostics

## ---------------------------------------------------------------------------##
# Monthly birth cohorts --------------------------------------------------------
## ---------------------------------------------------------------------------##

cohorts <- mipR::reconstruct_birth_cohorts(
  aged_pop_admin,
  geography = c("adm0", "adm1", "adm1_code"),
  population = "pop",
  ref_date = "ref_date",
  method = "constrained",
  mortality = demog$mortality,
  births = demog$rates,
  start = "2000-01",
  end = "2025-12"
)

# population by admin unit, birth month and calendar month
cohorts$cohorts

# anchors outside the 5% tolerance, for review
cohorts$anchor_check |>
  dplyr::filter(!.data$within_tolerance)

# change in each cohort between anchors that mortality does not explain;
# the switch from legacy to R2025A data shows between July 2014 and
# January 2015
cohorts$cohort_changes

## ---------------------------------------------------------------------------##
# Vaccination history ----------------------------------------------------------
## ---------------------------------------------------------------------------##

# routine coverage by state, year and dose, with IHME lower and upper bounds
routine_coverage <- dplyr::bind_rows(
  mcv1 |>
    dplyr::transmute(
      adm0, adm1, adm1_code, year, dose = "mcv1",
      coverage = coverage_mean, coverage_lower, coverage_upper
    ),
  mcv2 |>
    dplyr::transmute(
      adm0, adm1, adm1_code, year, dose = "mcv2",
      coverage = coverage_mean, coverage_lower, coverage_upper
    )
) |>
  dplyr::filter(!is.na(adm1_code), !is.na(coverage))

# national schedule; the MCV2 introduction date and age need confirming
# against the Nigeria EPI schedule
schedule <- tibble::tribble(
  ~dose, ~age_months, ~start_date,
  "mcv1", 9, NA,
  "mcv2", 15, as.Date("2019-01-01")
)

history <- mipR::build_vaccination_history(
  cohorts,
  routine_coverage = routine_coverage,
  campaigns = campaigns,
  schedule = schedule,
  geography = c("adm0", "adm1", "adm1_code")
)

# opportunities that could not be applied, such as campaigns without dates
history$diagnostics

## ---------------------------------------------------------------------------##
# Immunity ---------------------------------------------------------------------
## ---------------------------------------------------------------------------##

immunity <- mipR::estimate_immunity(
  history, cohorts,
  coupling = "dependent"
)

# model doses against the post-campaign surveys
checks <- mipR::check_immunity(immunity, campaigns)
checks$summary

# susceptible children by state in mid-2025, in the same bands as
# pop_bands; the profile does not cover ages 15 and over
summary_2025 <- mipR::summarise_immunity(
  immunity,
  at = "2025-06",
  age_breaks = c(0, 60, 120)
)

# intervals from the IHME and survey coverage bounds
draws_2025 <- mipR::immunity_draws(
  history, cohorts,
  n_draws = 200, at = "2025-06"
)

## ---------------------------------------------------------------------------##
# Plots and maps ---------------------------------------------------------------
## ---------------------------------------------------------------------------##

mipR::plot_cohort_protection(
  immunity,
  at = "2025-06",
  output_file = here::here("03_outputs/plots/nga_cohort_protection.png"),
  width = 9, height = 6
)

# the cohort born in October 2019, in states from the 10th to the 90th
# percentile of its susceptible share in mid-2025
mipR::plot_cohort_journey(
  immunity,
  birth = "2019-10",
  at = "2025-06",
  label = "adm1",
  output_file = here::here("03_outputs/plots/nga_cohort_journey.png")
)

mipR::map_immunity(
  summary_2025, shp,
  type = "bins",
  palette = "susceptibility",
  ncol = 3,
  output_file = here::here("03_outputs/plots/nga_susceptible_map.png"),
  width = 10, height = 4.5
)

