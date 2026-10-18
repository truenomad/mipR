skip_who_integration <- function() {
  skip_if_not_integration("WHO shapefile integration test skipped.")
}

# get_boundaries and list_boundaries -------------------------------------------

testthat::test_that("get_boundaries validates country", {
  testthat::expect_error(get_boundaries(123), "ISO3")
  testthat::expect_error(get_boundaries(character(0)), "ISO3")
  testthat::expect_error(get_boundaries("KENYA"), "ISO3")
})

testthat::test_that("get_boundaries validates level for each source", {
  testthat::expect_error(
    get_boundaries("KEN", adm_level = 3, source = "who"),
    "Invalid"
  )
  testthat::expect_error(
    get_boundaries("KEN", adm_level = "ADM5", source = "ocha"),
    "Invalid"
  )
  testthat::expect_error(
    get_boundaries("KEN", adm_level = "invalid", source = "gadm"),
    "Invalid"
  )
  testthat::expect_error(
    get_boundaries("KEN", adm_level = -1, source = "geoboundaries"),
    "Invalid"
  )
})

testthat::test_that("get_boundaries rejects unknown sources", {
  testthat::expect_error(
    get_boundaries("KEN", source = "osm"),
    "should be one of"
  )
})

testthat::test_that("get_boundaries accepts source in any case", {
  testthat::expect_error(
    get_boundaries("KEN", adm_level = 3, source = "WHO"),
    "Invalid"
  )
})

testthat::test_that(".check_level accepts numbers and ADM strings", {
  testthat::expect_identical(.check_level(2, "ocha"), 2L)
  testthat::expect_identical(.check_level("ADM1", "gadm"), 1L)
  testthat::expect_identical(.check_level("adm0", "who"), 0L)
})

testthat::test_that(".standardise_boundaries returns the standard columns", {
  x <- sf::st_sf(
    adm0_code = "AFG",
    adm0 = "Afghanistan",
    adm2 = "Kabul",
    extra = 1,
    geom = sf::st_sfc(sf::st_point(c(69, 34)), crs = 4326)
  )

  out <- .standardise_boundaries(x, level = 2, source = "ocha")

  testthat::expect_identical(
    names(out),
    c(
      "adm0_code", "adm0", "adm1", "adm1_code", "adm2", "adm2_code",
      "source", "geometry"
    )
  )
  testthat::expect_true(is.na(out$adm1))
  testthat::expect_identical(out$source, "ocha")
})

testthat::test_that("get_boundaries reports network errors", {
  testthat::with_mocked_bindings(
    request = function(...) stop("Network error"),
    .package = "httr2",
    {
      testthat::expect_error(
        get_boundaries("KEN", source = "who", cache = FALSE),
        "Network error"
      )
    }
  )
})

testthat::test_that("get_boundaries downloads OCHA COD-AB boundaries", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_if_not_integration("OCHA integration test skipped.")

  result <- get_boundaries("TCD", adm_level = 2, source = "ocha", cache = FALSE)

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true(all(result$adm0_code == "TCD"))
  testthat::expect_false(anyNA(result$adm2_code))
  testthat::expect_identical(unique(result$source), "ocha")
})

testthat::test_that("get_boundaries downloads geoBoundaries boundaries", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_if_not_integration("geoBoundaries integration test skipped.")

  result <- get_boundaries(
    "COM", adm_level = 1, source = "geoboundaries", cache = FALSE
  )

  testthat::expect_s3_class(result, "sf")
  testthat::expect_gt(nrow(result), 0)
  testthat::expect_false(anyNA(result$adm1))
})

testthat::test_that("get_boundaries downloads GADM boundaries", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_if_not_integration("GADM integration test skipped.")

  result <- get_boundaries("COM", adm_level = 1, source = "gadm", cache = FALSE)

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true(all(result$adm0_code == "COM"))
  testthat::expect_false(anyNA(result$adm1_code))
})

testthat::test_that("list_boundaries reports each source", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_if_not_integration("list_boundaries integration test skipped.")

  result <- list_boundaries("AFG")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    result$source,
    c("ocha", "geoboundaries", "gadm", "who")
  )
  testthat::expect_true(all(result$available))
})

# WHO source -------------------------------------------------------------------

testthat::test_that(".get_boundaries_who validates admin_level parameter", {
  testthat::expect_error(
    .get_boundaries_who("KEN", admin_level = "ADM5"),
    "Invalid admin_level"
  )

  testthat::expect_error(
    .get_boundaries_who("KEN", admin_level = "invalid"),
    "Invalid admin_level"
  )

  testthat::expect_error(
    .get_boundaries_who("KEN", admin_level = 123),
    "Invalid admin_level"
  )
})

testthat::test_that(".get_boundaries_who accepts valid admin levels", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  testthat::expect_no_error({
    result <- .get_boundaries_who("COM", admin_level = "ADM0")
  })

  testthat::expect_no_error({
    result <- .get_boundaries_who("COM", admin_level = "ADM1")
  })

  testthat::expect_no_error({
    result <- .get_boundaries_who("COM", admin_level = "ADM2")
  })
})

testthat::test_that(".get_boundaries_who returns sf when dest_path is NULL", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result <- .get_boundaries_who("COM", admin_level = "ADM0")

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true("adm0_code" %in% names(result))
  testthat::expect_true("adm0" %in% names(result))
  testthat::expect_equal(unique(result$adm0_code), "COM")
})

testthat::test_that(".get_boundaries_who returns columns for each level", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result_adm0 <- .get_boundaries_who("COM", admin_level = "ADM0")
  testthat::expect_equal(
    setdiff(names(result_adm0), "geometry"),
    c("adm0_code", "adm0", "start_date", "end_date")
  )

  result_adm1 <- .get_boundaries_who("COM", admin_level = "ADM1")
  testthat::expect_equal(
    setdiff(names(result_adm1), "geometry"),
    c("adm0_code", "adm0", "adm1", "start_date", "end_date")
  )

  result_adm2 <- .get_boundaries_who("COM", admin_level = "ADM2")
  testthat::expect_equal(
    setdiff(names(result_adm2), "geometry"),
    c("adm0_code", "adm0", "adm1", "adm2", "start_date", "end_date")
  )
})

testthat::test_that(".get_boundaries_who saves to file when dest_path set", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  temp_dir <- tempdir()
  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0_latest.gpkg")

  if (file.exists(expected_file)) unlink(expected_file)

  result <- .get_boundaries_who(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(file.exists(expected_file))
  testthat::expect_s3_class(result, "sf")

  saved_data <- sf::st_read(expected_file, quiet = TRUE)
  testthat::expect_equal(nrow(saved_data), nrow(result))
  # geometry column name may change when saving/reading
  testthat::expect_equal(
    setdiff(names(saved_data), c("geom", "geometry")),
    setdiff(names(result), c("geom", "geometry"))
  )

  unlink(expected_file)
})

testthat::test_that(".get_boundaries_who handles incremental updates", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  temp_dir <- tempdir()
  first_file <- file.path(temp_dir, "who_shapefile_com_adm0_latest.gpkg")
  second_file <- file.path(temp_dir, "who_shapefile_com_syc_adm0_latest.gpkg")

  if (file.exists(first_file)) unlink(first_file)
  if (file.exists(second_file)) unlink(second_file)

  result1 <- .get_boundaries_who(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(file.exists(first_file))
  testthat::expect_equal(unique(result1$adm0_code), "COM")

  # a request with an additional country creates a new file
  result2 <- .get_boundaries_who(
    c("COM", "SYC"),
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(file.exists(second_file))
  testthat::expect_true(all(c("COM", "SYC") %in% result2$adm0_code))
  testthat::expect_true(nrow(result2) >= 2)

  # the same countries again should not download
  testthat::expect_message(
    result3 <- .get_boundaries_who(
      c("COM", "SYC"),
      admin_level = "ADM0",
      dest_path = temp_dir
    ),
    "All requested country codes are already"
  )

  testthat::expect_equal(nrow(result3), nrow(result2))

  unlink(first_file)
  unlink(second_file)
})

testthat::test_that(".get_boundaries_who handles multiple country codes", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result <- .get_boundaries_who(
    c("COM", "SYC", "MUS"),
    admin_level = "ADM0"
  )

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true(all(c("COM", "SYC", "MUS") %in% result$adm0_code))
  testthat::expect_equal(length(unique(result$adm0_code)), 3)
})

testthat::test_that(".get_boundaries_who creates missing directories", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  temp_dir <- file.path(tempdir(), "new_test_dir", "nested")
  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0_latest.gpkg")

  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  result <- .get_boundaries_who(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(dir.exists(temp_dir))
  testthat::expect_true(file.exists(expected_file))

  unlink(dirname(temp_dir), recursive = TRUE)
})

testthat::test_that(".get_boundaries_who handles case-insensitive levels", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result_lower <- .get_boundaries_who("COM", admin_level = "adm0")
  result_upper <- .get_boundaries_who("COM", admin_level = "ADM0")
  result_mixed <- .get_boundaries_who("COM", admin_level = "Adm0")

  testthat::expect_equal(nrow(result_lower), nrow(result_upper))
  testthat::expect_equal(nrow(result_lower), nrow(result_mixed))
  testthat::expect_equal(names(result_lower), names(result_upper))
  testthat::expect_equal(names(result_lower), names(result_mixed))
})

testthat::test_that(".get_boundaries_who filters by ENDDATE when latest", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result_latest <- .get_boundaries_who(
    "COM", admin_level = "ADM2", latest = TRUE
  )

  testthat::expect_s3_class(result_latest, "sf")
  testthat::expect_true(nrow(result_latest) > 0)

  # default behaviour is latest = TRUE
  result_default <- .get_boundaries_who("COM", admin_level = "ADM2")
  testthat::expect_equal(nrow(result_default), nrow(result_latest))
})

testthat::test_that(".get_boundaries_who returns all boundaries if !latest", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result_all <- .get_boundaries_who(
    "COM", admin_level = "ADM0", latest = FALSE
  )
  result_latest <- .get_boundaries_who(
    "COM", admin_level = "ADM0", latest = TRUE
  )

  testthat::expect_s3_class(result_all, "sf")
  testthat::expect_s3_class(result_latest, "sf")

  # all boundaries include at least as many as the latest ones
  testthat::expect_gte(nrow(result_all), nrow(result_latest))
})

testthat::test_that(".get_boundaries_who handles network errors", {
  testthat::skip_on_cran()

  testthat::with_mocked_bindings(
    request = function(...) stop("Network error"),
    .package = "httr2",
    {
      testthat::expect_error(
        .get_boundaries_who("KEN"),
        "Network error"
      )
    }
  )
})

testthat::test_that(".get_boundaries_who validates geometries", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  skip_who_integration()

  result <- .get_boundaries_who("COM", admin_level = "ADM0")

  testthat::expect_true(all(sf::st_is_valid(result)))

  temp_dir <- tempdir()
  result_saved <- .get_boundaries_who(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(all(sf::st_is_valid(result_saved)))

  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0_latest.gpkg")
  if (file.exists(expected_file)) unlink(expected_file)
})
