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

testthat::test_that("extract_raster rounds sums to whole numbers", {
  r <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = c(1.2, 2.2, 3.2, 4.3)
  )
  path <- tempfile("pop_2020_", fileext = ".tif")
  on.exit(unlink(path), add = TRUE)
  suppressWarnings(terra::writeRaster(r, path))
  polys <- sf::st_sf(
    adm2 = "all",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      crs = 4326
    )
  )

  out <- extract_raster(
    path, polys, id_cols = "adm2", fun = c("sum", "mean"), quiet = TRUE
  )
  testthat::expect_identical(out$sum, 11)
  testthat::expect_equal(out$mean, 2.725, tolerance = 1e-6)
})

# population weights -----------------------------------------------------------

weight_fixture <- function(dir) {
  value <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = c(0.1, 0.2, 0.3, 0.4)
  )
  for (yr in 2020:2021) {
    suppressWarnings(terra::writeRaster(
      value, file.path(dir, paste0("mcv1_", yr, ".tif"))
    ))
  }
  pop_dir <- file.path(dir, "pop")
  dir.create(pop_dir)
  # 2020: all weight on the 0.4 cell; 2021: even weights at finer resolution
  pop_2020 <- terra::rast(value, vals = c(0, 0, 0, 10))
  pop_2021 <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = 1
  )
  suppressWarnings(terra::writeRaster(
    pop_2020, file.path(pop_dir, "nga_pop_2020.tif")
  ))
  suppressWarnings(terra::writeRaster(
    pop_2021, file.path(pop_dir, "nga_pop_2021.tif")
  ))
  polys <- sf::st_sf(
    adm2 = "all",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      crs = 4326
    )
  )
  list(values = dir, pop = pop_dir, polys = polys)
}

testthat::test_that("extract_raster weights values by population by year", {
  dir <- tempfile("weights_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- weight_fixture(dir)

  out <- extract_raster(
    fx$values, fx$polys,
    id_cols = "adm2", pattern = "^mcv1_",
    fun = c("mean", "weighted_mean", "weighted_median"),
    pop_rasters = fx$pop, quiet = TRUE
  )

  testthat::expect_named(
    out,
    c("adm2", "year", "mean", "weighted_mean", "weighted_median", "pop_sum")
  )
  testthat::expect_equal(out$mean, c(0.25, 0.25))
  testthat::expect_equal(out$weighted_mean, c(0.4, 0.25))
  testthat::expect_equal(out$weighted_median[1], 0.4)
  testthat::expect_identical(out$pop_sum, c(10, 16))
})

testthat::test_that("extract_raster uses the nearest population year", {
  dir <- tempfile("weights_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- weight_fixture(dir)
  file.rename(
    file.path(fx$values, "mcv1_2021.tif"),
    file.path(fx$values, "mcv1_2023.tif")
  )

  testthat::expect_warning(
    out <- extract_raster(
      fx$values, fx$polys,
      id_cols = "adm2", pattern = "^mcv1_",
      fun = "weighted_mean", pop_rasters = fx$pop, quiet = TRUE
    ),
    "nearest year"
  )
  testthat::expect_equal(out$weighted_mean[out$year == 2023], 0.25)
})

testthat::test_that("extract_raster checks weighted inputs", {
  dir <- tempfile("weights_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- weight_fixture(dir)

  testthat::expect_error(
    extract_raster(
      fx$values, fx$polys,
      id_cols = "adm2", pattern = "^mcv1_", fun = "weighted_mean"
    ),
    "pop_rasters"
  )
  file.copy(
    file.path(fx$pop, "nga_pop_2020.tif"),
    file.path(fx$pop, "nga_pop_2020_copy.tif")
  )
  testthat::expect_error(
    extract_raster(
      fx$values, fx$polys,
      id_cols = "adm2", pattern = "^mcv1_",
      fun = "weighted_mean", pop_rasters = fx$pop, quiet = TRUE
    ),
    "share year"
  )
})

testthat::test_that(".weighted_median finds the half-weight value", {
  testthat::expect_identical(.weighted_median(c(3, 1, 2), c(1, 1, 5)), 2)
  testthat::expect_identical(.weighted_median(c(1, 2), c(0, 0)), NA_real_)
})
