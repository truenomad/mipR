testthat::test_that(".raster_file_info reads ages and years", {
  info <- .raster_file_info(c(
    "nga_age01_2025.tif", "nga_age02_2025.tif",
    "nga_age02_2026.tif"
  ))

  testthat::expect_identical(info$year, c(2025L, 2025L, 2026L))
  testthat::expect_identical(info$age, c(1L, 2L, 2L))
  testthat::expect_null(info$month)
})

testthat::test_that(".raster_file_info keeps age when some files lack it", {
  info <- .raster_file_info(c(
    "nga_t_01_2025_CN_1km_UA_v1.tif", "nga_age02_2025.tif"
  ))

  testthat::expect_identical(info$year, c(2025L, 2025L))
  testthat::expect_identical(info$age, c(NA, 2L))
})

testthat::test_that(".raster_file_info ignores WorldPop group codes", {
  info <- .raster_file_info(c(
    "nga_t_01_2025_CN_1km_UA_v1.tif", "nga_t_05_2020_CN_1km_UA_v1.tif"
  ))

  testthat::expect_identical(info$year, c(2025L, 2020L))
  testthat::expect_null(info$month)
})

testthat::test_that(".raster_file_info reads monthly and daily dates", {
  monthly <- .raster_file_info(c("rain_2022-01.tif", "rain_2022-11.tif"))
  testthat::expect_identical(monthly$month, c(1L, 11L))

  euro <- .raster_file_info(c("rain_03.2021.tif", "rain_04.2021.tif"))
  testthat::expect_identical(euro$year, c(2021L, 2021L))
  testthat::expect_identical(euro$month, c(3L, 4L))

  same_month <- .raster_file_info(c("rain_2022-01.tif", "rain_2023-01.tif"))
  testthat::expect_identical(same_month$month, c(1L, 1L))

  daily <- .raster_file_info("chirps_2020.05.17.tif")
  testthat::expect_identical(daily$year, 2020L)
  testthat::expect_identical(daily$month, 5L)
})

testthat::test_that(".raster_file_info drops shared version numbers", {
  info <- .raster_file_info(c("pop_v2_10_2020.tif", "pop_v2_10_2021-03.tif"))
  testthat::expect_identical(info$year, c(2020L, 2021L))
})

testthat::test_that(".raster_file_info takes the last year and handles none", {
  info <- .raster_file_info("mcv1_coverage_mean_2000_2025.tif")
  testthat::expect_identical(info$year, 2025L)

  testthat::expect_length(.raster_file_info("travel_time.tif"), 0)
  testthat::expect_error(
    .raster_file_info(c("pop_2020.tif", "rain_2020-01.tif")),
    "different date formats"
  )
})

testthat::test_that("extract_raster sums values over polygons", {
  r <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = c(1, 2, 3, 4)
  )
  raster_dir <- tempfile("extract_")
  dir.create(raster_dir)
  on.exit(unlink(raster_dir, recursive = TRUE), add = TRUE)
  suppressWarnings(terra::writeRaster(
    r, file.path(raster_dir, "gin_age03_2020.tif")
  ))

  # left and right halves of the grid
  polys <- sf::st_sf(
    adm2 = c("west", "east"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 2), c(0, 2), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 2), c(1, 2), c(1, 0)))),
      crs = 4326
    )
  )

  out <- extract_raster(raster_dir, polys, id_cols = "adm2", quiet = TRUE)

  testthat::expect_named(
    out,
    c("adm2", "year", "age", "sum")
  )
  testthat::expect_equal(out$sum, c(4, 6))
  testthat::expect_identical(out$age, c(3L, 3L))
})

testthat::test_that("extract_raster uses pattern to pick files in a folder", {
  r <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = c(1, 2, 3, 4)
  )
  raster_dir <- tempfile("extract_")
  dir.create(raster_dir)
  on.exit(unlink(raster_dir, recursive = TRUE), add = TRUE)
  for (f in c("gin_age03_2020.tif", "gin_t_01_2020_CN_1km_UA_v1.tif")) {
    suppressWarnings(terra::writeRaster(r, file.path(raster_dir, f)))
  }
  polys <- sf::st_sf(
    adm2 = "all",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      crs = 4326
    )
  )

  out <- extract_raster(
    raster_dir, polys,
    id_cols = "adm2", pattern = "_age\\d+_\\d{4}\\.tif$", quiet = TRUE
  )
  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_identical(out$age, 3L)

  testthat::expect_error(
    extract_raster(raster_dir, polys, id_cols = "adm2", pattern = "^none"),
    "No raster files found"
  )
})
