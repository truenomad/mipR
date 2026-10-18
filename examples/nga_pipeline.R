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

# WorldPop single-year age rasters (age 0 is also used as population weights)
mipR::get_worldpop_single_age(
  country_codes = "NGA",
  years = 2010:2030,
  ages = 0:15,
  out_dir = here::here("01_data/1b_worldpop/raw")
)

## ---------------------------------------------------------------------------##
# Population by age and admin unit ---------------------------------------------
## ---------------------------------------------------------------------------##

aged_pop_admin <- mipR::extract_raster(
  rasters = here::here("01_data/1b_worldpop/raw"),
  shapefile = shp,
  id_cols = c("adm0", "adm1", "adm1_code"),
  pattern = "_age\\d+_\\d{4}\\.tif$"
) |>
  dplyr::rename(pop = sum)

dir.create(
  here::here("01_data/1b_worldpop/processed"),
  recursive = TRUE, showWarnings = FALSE
)

saveRDS(
  aged_pop_admin,
  here::here("01_data/1b_worldpop/processed/nga_aged_pop_2010_2030.rds")
)

## ---------------------------------------------------------------------------##
# MCV1 coverage by admin unit --------------------------------------------------
## ---------------------------------------------------------------------------##

# weighted by the under-one population of the same year (or the nearest)
mcv1 <- mipR::extract_raster(
  rasters = here::here("01_data/1c_ihme/raw/mcv1_coverage_mean_2000_2025.tif"),
  shapefile = shp,
  id_cols = c("adm0", "adm1", "adm1_code"),
  fun = c("mean", "weighted_mean"),
  pop_rasters = here::here("01_data/1b_worldpop/raw"),
  pop_pattern = "_age00_\\d{4}\\.tif$"
)

## ---------------------------------------------------------------------------##
# Monthly birth cohorts --------------------------------------------------------
## ---------------------------------------------------------------------------##

cohorts <- mipR::reconstruct_birth_cohorts(
  aged_pop_admin,
  geography = c("adm0", "adm1", "adm1_code"),
  population = "pop",
  ref_date = "01-01",
  method = "constrained",
  mortality = demog$mortality,
  births = demog$rates,
  start = "2010-01",
  end = "2025-12"
)

# population by admin unit, birth month and calendar month
cohorts$cohorts

# anchors outside the 5% tolerance, for review
cohorts$anchor_check[!cohorts$anchor_check$within_tolerance, ]
