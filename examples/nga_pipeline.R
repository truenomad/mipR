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
# MCV1 coverage by admin unit --------------------------------------------------
## ---------------------------------------------------------------------------##

# weighted by the under-one population of the same year (or the nearest)
mcv1 <- mipR::extract_raster(
  rasters = here::here(
    "01_data/1c_ihme/raw/mcv1_coverage_mean_2000_2025.tif"
  ),
  shapefile = shp,
  id_cols = c("adm0", "adm1", "adm1_code"),
  fun = c("mean", "weighted_mean"),
  pop_rasters = here::here("01_data/1b_worldpop/raw"),
  pop_pattern = "_age00_\\d{4}\\.tif$"
)

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

# match IHME unit names to the boundaries; decisions are cached so each
# name is confirmed once
mcv2 <- sntutils::prep_geonames(
  target_df = mcv2,
  lookup_df = sf::st_drop_geometry(shp),
  level0 = "adm0",
  level1 = "adm1",
  cache_path = here::here("01_data/1c_ihme/processed/geoname_cache.rds")
)

# add the boundary codes, which come from the matched names
mcv2 <- dplyr::left_join(
  mcv2,
  sf::st_drop_geometry(shp)[, c("adm0", "adm1", "adm1_code")],
  by = c("adm0", "adm1")
)

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
cohorts$anchor_check[!cohorts$anchor_check$within_tolerance, ]

# change in each cohort between anchors that mortality does not explain;
# the switch from legacy to R2025A data shows between July 2014 and
# January 2015
cohorts$cohort_changes
