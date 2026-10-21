testthat::test_that("population weighting preserves counts across CRSs", {
  population <- terra::rast(
    nrows = 100, ncols = 100, xmin = 0, xmax = 200000,
    ymin = 0, ymax = 200000, crs = "EPSG:3857",
    vals = rep(c(rep(1, 50), rep(3, 50)), 100)
  )
  geometry <- sf::st_as_sfc(
    "POLYGON ((0 0, 200000 0, 200000 200000, 0 200000, 0 0))",
    crs = 3857
  ) |>
    sf::st_transform(4326)
  extent <- sf::st_bbox(geometry)
  values <- terra::rast(
    nrows = 1, ncols = 2,
    xmin = purrr::pluck(extent, "xmin"),
    xmax = purrr::pluck(extent, "xmax"),
    ymin = purrr::pluck(extent, "ymin"),
    ymax = purrr::pluck(extent, "ymax"),
    crs = "EPSG:4326", vals = c(0, 1)
  )
  directory <- tempfile("projection_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  pop_path <- file.path(directory, "pop_2024.tif")
  value_path <- file.path(directory, "coverage_2024.tif")
  terra::writeRaster(population, pop_path)
  terra::writeRaster(values, value_path)
  boundaries <- sf::st_sf(adm1 = "A", geometry = geometry)
  result <- extract_raster(
    value_path, boundaries, id_cols = "adm1", fun = "weighted_mean",
    pop_rasters = pop_path
  )
  # Allow the small numerical error from projecting curved cell boundaries.
  # The right half contains three times as many people as the left half.
  testthat::expect_equal(result$weighted_mean, 0.75, tolerance = 1e-4)
  testthat::expect_equal(result$pop_sum, 20000)
})
