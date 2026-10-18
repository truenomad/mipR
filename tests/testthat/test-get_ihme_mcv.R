# read_ihme_mcv_admin ----------------------------------------------------------

fixture_dir <- testthat::test_path("fixtures")

testthat::test_that("read_ihme_mcv_admin reads the admin 1 table", {
  out <- read_ihme_mcv_admin(fixture_dir, dose = "mcv2", country = "nga")

  testthat::expect_named(
    out,
    c(
      "iso3", "adm0", "adm1", "year", "dose", "coverage_mean",
      "coverage_lower", "coverage_upper", "pop_under_1"
    )
  )
  testthat::expect_identical(out$adm1, c("Kano", "Kano", "Lagos"))
  testthat::expect_identical(out$year, c(2020L, 2021L, 2020L))
  testthat::expect_equal(out$coverage_mean, c(0.2, 0.3, 0.6))
  testthat::expect_identical(unique(out$dose), "mcv2")
})

testthat::test_that("read_ihme_mcv_admin reads admin 2 and a file path", {
  file <- file.path(fixture_dir, "mcv2_coverage_adm1_adm2_2000_2025.xlsx")
  out <- read_ihme_mcv_admin(file, dose = "mcv2", adm_level = 2)

  testthat::expect_true("adm2" %in% names(out))
  testthat::expect_identical(nrow(out), 4L)
  testthat::expect_identical(sort(unique(out$iso3)), c("GHA", "NGA"))
})

testthat::test_that("read_ihme_mcv_admin reports missing tables and rows", {
  testthat::expect_error(
    read_ihme_mcv_admin(fixture_dir, dose = "mcv1"),
    "No MCV1 admin table"
  )
  testthat::expect_error(
    read_ihme_mcv_admin(fixture_dir, country = "KEN"),
    "No rows"
  )
  testthat::expect_error(
    read_ihme_mcv_admin(fixture_dir, adm_level = 3),
    "must be 1 or 2"
  )
})
