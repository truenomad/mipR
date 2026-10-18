# setup and utilities ----------------------------------------------------------

testthat::test_that("setup: required packages are available", {
  testthat::expect_true(requireNamespace("httr2", quietly = TRUE))
  testthat::expect_true(requireNamespace("terra", quietly = TRUE))
  testthat::expect_true(requireNamespace("cli", quietly = TRUE))
})

# create temporary directory for tests
setup_temp_dir <- function() {
  temp_dir <- tempfile("worldpop_test")
  dir.create(temp_dir, recursive = TRUE)
  return(temp_dir)
}

# get_worldpop function tests --------------------------------------------------

testthat::test_that("get_worldpop: input validation works correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  testthat::expect_true(exists("get_worldpop"))

  # test type argument validation
  testthat::expect_error(
    get_worldpop("GBR", type = "invalid"),
    "should be one of.*count.*density"
  )

  # test that country_codes is required
  testthat::expect_error(
    get_worldpop(),
    "argument \"country_codes\" is missing"
  )

  # density with years >= 2015 throws error (density only in legacy)
  testthat::expect_error(
    get_worldpop("GBR", years = 2015:2020, type = "density"),
    "Density data only available for years 2000-2020"
  )

  # 100m with years < 2015 throws error (100m only in r2025a)
  testthat::expect_error(
    get_worldpop("GBR", years = 2010:2015, resolution = "100m"),
    "100m resolution only available for years >= 2015"
  )

  # years before 2000 throw an error
  testthat::expect_error(
    get_worldpop("GBR", years = 1999:2005),
    "Legacy dataset only available for years 2000-2020"
  )

  # years after 2030 throw an error
  testthat::expect_error(
    get_worldpop("GBR", years = 2025:2031),
    "R2025A dataset only available for years 2015-2030"
  )

  # invalid resolution
  testthat::expect_error(
    get_worldpop("GBR", resolution = "5km"),
    "should be one of.*1km.*100m"
  )
})

testthat::test_that("get_worldpop: directory creation works", {
  temp_dir <- file.path(tempdir(), "nonexistent", "nested", "dir")
  on.exit(unlink(dirname(temp_dir), recursive = TRUE))

  testthat::expect_false(dir.exists(temp_dir))

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  testthat::expect_true(dir.exists(temp_dir))

  # a mock existing file means the function skips the download
  mock_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
  writeLines("mock content", mock_file)

  result <- get_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  testthat::expect_true(dir.exists(temp_dir))
})

testthat::test_that("get_worldpop: url construction is correct for r2025a", {
  # 1km r2025a: URL has R2025A, local filename does not
  result <- .build_worldpop_url("GIN", 2015, "count", "r2025a", "1km")
  expected_url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2015/GIN/v1/1km_ua/constrained/gin_pop_2015_CN_1km_R2025A_UA_v1.tif"
  )
  testthat::expect_equal(result$url, expected_url)
  testthat::expect_equal(result$filename, "gin_pop_2015_CN_1km_UA_v1.tif")

  # 100m r2025a (no _UA suffix)
  result_100m <- .build_worldpop_url("GIN", 2020, "count", "r2025a", "100m")
  expected_url_100m <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2020/GIN/v1/100m/constrained/gin_pop_2020_CN_100m_R2025A_v1.tif"
  )
  testthat::expect_equal(result_100m$url, expected_url_100m)
  testthat::expect_equal(result_100m$filename, "gin_pop_2020_CN_100m_v1.tif")

  # country code case handling: uppercase in path, lowercase in filename
  result_lower <- .build_worldpop_url("civ", 2015, "count", "r2025a", "1km")
  testthat::expect_true(grepl("/CIV/", result_lower$url))
  testthat::expect_true(grepl("^civ_", result_lower$filename))
})

testthat::test_that("get_worldpop: url construction for GLOBAL mosaic", {
  result <- .build_worldpop_url(
    "GLOBAL", 2020, "count", "r2025a", "1km"
  )
  expected_url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2020/0_Mosaicked/v1/1km_ua/constrained/",
    "global_pop_2020_CN_1km_R2025A_UA_v1.tif"
  )
  testthat::expect_equal(result$url, expected_url)
  testthat::expect_equal(result$filename, "global_pop_2020_CN_1km_UA_v1.tif")

  # case-insensitive
  result_lower <- .build_worldpop_url(
    "global", 2020, "count", "r2025a", "1km"
  )
  testthat::expect_equal(result_lower$url, expected_url)

  # GLOBAL + 100m rejected
  testthat::expect_error(
    .build_worldpop_url("GLOBAL", 2020, "count", "r2025a", "100m"),
    "GLOBAL"
  )

  # GLOBAL + legacy rejected
  testthat::expect_error(
    .build_worldpop_url("GLOBAL", 2010, "count", "legacy", "1km"),
    "GLOBAL"
  )
})

testthat::test_that("get_worldpop: GLOBAL validation in main function", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # GLOBAL + legacy year
  testthat::expect_error(
    get_worldpop("GLOBAL", years = 2010, dest_dir = temp_dir, quiet = TRUE),
    "GLOBAL"
  )
  # GLOBAL + 100m
  testthat::expect_error(
    get_worldpop(
      "GLOBAL",
      years = 2020,
      resolution = "100m", dest_dir = temp_dir, quiet = TRUE
    ),
    "GLOBAL"
  )
  # GLOBAL + density
  testthat::expect_error(
    get_worldpop(
      "GLOBAL",
      years = 2020,
      type = "density", dest_dir = temp_dir, quiet = TRUE
    ),
    "Density|GLOBAL"
  )
})

testthat::test_that("get_worldpop: GLOBAL skip when cached", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  mock_file <- file.path(temp_dir, "global_pop_2020_CN_1km_UA_v1.tif")
  writeLines("mock", mock_file)

  result <- get_worldpop(
    "GLOBAL",
    years = 2020, dest_dir = temp_dir, quiet = TRUE
  )

  testthat::expect_true(file.exists(mock_file))
  testthat::expect_equal(unname(result$counts[["GLOBAL"]]), 1)
})

testthat::test_that("get_worldpop: url construction is correct for legacy", {
  result_count <- .build_worldpop_url("GBR", 2020, "count", "legacy", "1km")
  expected_count <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/",
    "2020/GBR/gbr_ppp_2020_1km_Aggregated_UNadj.tif"
  )
  testthat::expect_equal(result_count$url, expected_count)
  testthat::expect_equal(
    result_count$filename,
    "gbr_ppp_2020_1km_Aggregated_UNadj.tif"
  )

  result_density <- .build_worldpop_url(
    "FRA", 2019, "density", "legacy", "1km"
  )
  expected_density <- paste0(
    "https://data.worldpop.org/GIS/Population_Density/",
    "Global_2000_2020_1km_UNadj/2019/FRA/fra_pd_2019_1km_UNadj.tif"
  )
  testthat::expect_equal(result_density$url, expected_density)
  testthat::expect_equal(result_density$filename, "fra_pd_2019_1km_UNadj.tif")
})

testthat::test_that("get_worldpop: handles existing files correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # mock existing file (r2025a format, local name without R2025A)
  existing_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
  writeLines("existing content", existing_file)

  testthat::expect_true(file.exists(existing_file))

  result <- get_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # file should still exist and not be modified
  testthat::expect_true(file.exists(existing_file))
  testthat::expect_equal(
    readLines(existing_file),
    "existing content"
  )
})

testthat::test_that("get_worldpop: handles existing legacy files correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # mock existing file (legacy format for year < 2015)
  existing_file <- file.path(
    temp_dir, "gbr_ppp_2010_1km_Aggregated_UNadj.tif"
  )
  writeLines("existing content", existing_file)

  testthat::expect_true(file.exists(existing_file))

  result <- get_worldpop(
    "GBR",
    years = 2010,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  testthat::expect_true(file.exists(existing_file))
  testthat::expect_equal(
    readLines(existing_file),
    "existing content"
  )
})

testthat::test_that("get_worldpop: returns correct structure", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  mock_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
  writeLines("mock content", mock_file)

  result <- get_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("files", "counts"))
  testthat::expect_type(result$files, "character")
  testthat::expect_type(result$counts, "integer")
})

testthat::test_that("get_worldpop: handles multiple countries and years", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  countries <- c("GBR", "FRA")
  years <- c(2019, 2020)

  for (cc in countries) {
    for (yr in years) {
      mock_file <- file.path(
        temp_dir,
        sprintf("%s_pop_%s_CN_1km_UA_v1.tif", tolower(cc), yr)
      )
      writeLines("mock content", mock_file)
    }
  }

  result <- get_worldpop(
    countries,
    years = years,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # one file per country and year
  testthat::expect_length(result$files, 4)
  testthat::expect_length(result$counts, 2)
})

# age-band URL tests -----------------------------------------------------------
# integration tests use the legacy dataset (years < 2015) for reliability

testthat::test_that(".build_worldpop_age_band_url: legacy per-country", {
  result <- .build_worldpop_age_band_url(
    "TUN", "m", 5, 2010, "1km"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2000_2020_1km/unconstrained/2010/TUN/tun_m_5_2010_1km.tif"
    )
  )
  testthat::expect_equal(result$filename, "tun_m_5_2010_1km.tif")

  # legacy ignores release param
  result_r <- .build_worldpop_age_band_url(
    "TUN", "f", 5, 2014, "1km", "R2024B"
  )
  testthat::expect_true(
    grepl("Global_2000_2020_1km", result_r$url, fixed = TRUE)
  )
})

testthat::test_that(".build_worldpop_age_band_url: legacy rejects bad combos", {
  # 100m not in legacy
  testthat::expect_error(
    .build_worldpop_age_band_url("TUN", "m", 5, 2010, "100m"),
    "100m"
  )
  # 't' total not in legacy
  testthat::expect_error(
    .build_worldpop_age_band_url("TUN", "t", 5, 2010, "1km"),
    "total"
  )
  # GLOBAL mosaic not in legacy
  testthat::expect_error(
    .build_worldpop_age_band_url("GLOBAL", "m", 5, 2010, "1km"),
    "GLOBAL"
  )
})

testthat::test_that(".build_worldpop_age_band_url: R2025A per-country", {
  # 1km, t total
  result <- .build_worldpop_age_band_url(
    "GIN", "t", 0, 2020, "1km", "R2025A"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/GIN/v1/1km_ua/constrained/",
      "gin_t_00_2020_CN_1km_R2025A_UA_v1.tif"
    )
  )
  testthat::expect_equal(result$filename, "gin_t_00_2020_CN_1km_UA_v1.tif")

  # 100m, m
  result_100m <- .build_worldpop_age_band_url(
    "GIN", "m", 5, 2020, "100m", "R2025A"
  )
  testthat::expect_equal(
    result_100m$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/GIN/v1/100m/constrained/",
      "gin_m_05_2020_CN_100m_R2025A_v1.tif"
    )
  )
})

testthat::test_that(".build_worldpop_age_band_url: R2024B per-country", {
  # 1km, t total
  result <- .build_worldpop_age_band_url(
    "BDI", "t", 0, 2015, "1km", "R2024B"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2015/BDI/v1/1km_ua/unconstrained/",
      "bdi_t_00_2015_UC_1km_R2024B_UA_v1.tif"
    )
  )

  # 100m, f
  result_100m <- .build_worldpop_age_band_url(
    "BDI", "f", 0, 2015, "100m", "R2024B"
  )
  testthat::expect_equal(
    result_100m$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2015/BDI/v1/100m/unconstrained/",
      "bdi_f_00_2015_UC_100m_R2024B_v1.tif"
    )
  )
})

testthat::test_that(".build_worldpop_age_band_url: GLOBAL mosaic", {
  # R2025A
  result_r2025a <- .build_worldpop_age_band_url(
    "GLOBAL", "t", 0, 2020, "1km", "R2025A"
  )
  testthat::expect_equal(
    result_r2025a$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/0_Mosaicked/v1/1km_ua/constrained/",
      "global_t_00_2020_CN_1km_R2025A_UA_v1.tif"
    )
  )

  # R2024B
  result_r2024b <- .build_worldpop_age_band_url(
    "global", "t", 0, 2020, "1km", "R2024B"
  )
  testthat::expect_equal(
    result_r2024b$url,
    paste0(
      "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2020/0_Mosaicked/v1/1km_ua/unconstrained/",
      "global_t_00_2020_UC_1km_R2024B_UA_v1.tif"
    )
  )

  # GLOBAL + 100m is rejected
  testthat::expect_error(
    .build_worldpop_age_band_url(
      "GLOBAL", "t", 0, 2020, "100m", "R2025A"
    ),
    "1km"
  )
})
