testthat::test_that(".worldpop_band_lower depends on the release period", {
  testthat::expect_identical(
    .worldpop_band_lower(2010),
    c(0, 1, seq(5, 80, by = 5))
  )
  testthat::expect_identical(
    .worldpop_band_lower(2025),
    c(0, 1, seq(5, 90, by = 5))
  )
})

testthat::test_that(".worldpop_reference_date labels each product", {
  r2025a <- .worldpop_reference_date("R2025A", 2025)
  testthat::expect_identical(r2025a$date, "01-01")
  testthat::expect_true(r2025a$documented)

  r2024b <- .worldpop_reference_date("R2024B", 2020)
  testthat::expect_identical(r2024b$date, "01-01")
  testthat::expect_false(r2024b$documented)

  legacy <- .worldpop_reference_date("R2025A", 2010)
  testthat::expect_identical(legacy$product, "legacy")
  testthat::expect_identical(legacy$date, "07-01")
  testthat::expect_false(legacy$documented)
})

testthat::test_that(".wpp_pop_at aligns WPP years with the reference date", {
  # wpp2024 package year Y is 31 December Y
  wpp_cc <- data.frame(
    year = rep(2009:2011, each = 2),
    age = rep(0:1, 3),
    pop = c(10, 20, 30, 40, 50, 60)
  )

  jan <- .wpp_pop_at(wpp_cc, 2011, "01-01", "pop")
  testthat::expect_identical(jan$pop, c(30, 40))

  jul <- .wpp_pop_at(wpp_cc, 2010, "07-01", "pop")
  testthat::expect_identical(jul$pop, c(20, 30))

  missing <- .wpp_pop_at(wpp_cc, 2009, "01-01", "pop")
  testthat::expect_identical(nrow(missing), 0L)
})

testthat::test_that("get_worldpop_single_age validates ages", {
  testthat::skip_if_not_installed("wpp2024")

  testthat::expect_error(
    get_worldpop_single_age("NGA", 2025, ages = c(1, 101)),
    "between 0 and 100"
  )
  testthat::expect_error(
    get_worldpop_single_age("GLOBAL", 2025),
    "not supported"
  )
})

testthat::test_that(".split_age_groups splits a group by WPP shares", {
  testthat::skip_if_not(
    requireNamespace("terra", quietly = TRUE),
    "terra package not available"
  )

  out_dir <- tempfile("single_age_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  group <- suppressWarnings(terra::rast(nrows = 2, ncols = 2, vals = 100))
  suppressWarnings(terra::writeRaster(
    group,
    file.path(out_dir, "gin_t_01_2020_CN_1km_UA_v1.tif")
  ))

  # ages 1 to 4 hold 10, 20, 30 and 40 people
  wpp_yr <- data.frame(age = 0:100, pop = 0)
  wpp_yr$pop[wpp_yr$age %in% 1:4] <- c(10, 20, 30, 40)

  out_fnames <- file.path(out_dir, c("age01.tif", "age04.tif"))
  .split_age_groups(
    "GIN", 2020, c(1, 4), out_fnames, wpp_yr, "pop",
    sex = "both", resolution = "1km", release = "R2025A",
    out_dir = out_dir, quiet = TRUE
  )

  testthat::expect_equal(terra::values(terra::rast(out_fnames[1]))[1], 10)
  testthat::expect_equal(terra::values(terra::rast(out_fnames[2]))[1], 40)
})
