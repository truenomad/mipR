testthat::test_that("quiet WHO cache reads emit no progress messages", {
  directory <- tempfile("boundary_cache_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  geometry <- sf::st_as_sfc(
    "POLYGON ((43 -12, 44 -12, 44 -11, 43 -11, 43 -12))", crs = 4326
  )
  boundaries <- sf::st_sf(
    adm0_code = "COM", adm0 = "Comoros", geometry = geometry
  )
  sf::st_write(
    boundaries, file.path(directory, "com_adm0_latest_who.gpkg"),
    quiet = TRUE
  )
  testthat::expect_silent(
    result <- get_boundaries(
      "COM", adm_level = 0, source = "who",
      cache_dir = directory, quiet = TRUE
    )
  )
  testthat::expect_equal(result$adm0, "Comoros")
})
