testthat::test_that("IHME release documents use numeric version order", {
  pattern <- "^VCM_Version_([0-9]+)_Release_Document_.*\\.pdf$"
  files <- c(
    "VCM_Version_2_Release_Document_2024.pdf",
    "VCM_Version_10_Release_Document_2025.pdf",
    "VCM_Methods_Introduction.pdf"
  )
  testthat::expect_equal(
    .newest_release_document(files, pattern),
    "VCM_Version_10_Release_Document_2025.pdf"
  )
  testthat::expect_identical(
    .newest_release_document("other.pdf", pattern), NA_character_
  )
})

testthat::test_that("IHME urls escape file and folder segments", {
  url <- .ihme_webdav_url("MCV/MCV1 - Routine", "coverage 2025.tif")
  testthat::expect_match(
    url, "/MCV/MCV1%20-%20Routine/coverage%202025.tif$", fixed = FALSE
  )
  testthat::expect_false(grepl(" ", url, fixed = TRUE))
})

testthat::test_that("current IHME cache entries require no network probes", {
  directory <- tempfile("ihme_cache_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  year <- max(
    as.integer(format(Sys.Date(), "%Y")), .ihme_first_release_year
  )
  files <- .ihme_mcv_files()
  versioned <- files |>
    dplyr::filter(grepl("{year}", .data$local, fixed = TRUE))
  paths <- file.path(directory, .ihme_fill_year(versioned$local, year))
  file.create(paths)
  testthat::local_mocked_bindings(
    .ihme_request = \(url) cli::cli_abort("Unexpected network probe")
  )
  resolved <- .ihme_resolve_years(files, directory)
  testthat::expect_equal(
    resolved, c(rep(year, nrow(versioned)), NA_integer_, NA_integer_)
  )
})
