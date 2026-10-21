testthat::test_that("default bins cover zero and small susceptible shares", {
  testthat::expect_equal(.default_breaks(0), 0)
  testthat::expect_equal(.default_breaks(1), 0)
  testthat::expect_equal(.default_breaks(8), c(0, 5))
  testthat::expect_equal(.default_breaks(15), c(0, 5, 10))
  testthat::expect_equal(.default_breaks(20), c(0, 5, 10))
  testthat::expect_equal(.default_breaks(60), c(0, 5, 10, 20, 40))
})

testthat::test_that("maps render low and zero susceptibility", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("sntutils")
  geometry <- sf::st_as_sfc(
    "POLYGON ((8 11, 9 11, 9 12, 8 12, 8 11))", crs = 4326
  )
  boundaries <- sf::st_sf(adm1 = "A", geometry = geometry)
  summary <- dplyr::tibble(
    adm1 = "A", month = as.Date("2025-01-01"),
    age_band = "1-4 years", susceptible_share = 0
  )
  purrr::walk(c(0, 0.04, 0.08, 0.15), \(share) {
    values <- dplyr::mutate(summary, susceptible_share = share)
    purrr::walk(c("bins", "gradient"), \(type) {
      plot <- map_immunity(values, boundaries, type = type)
      testthat::expect_s3_class(plot, "ggplot")
      testthat::expect_no_error(ggplot2::ggplot_build(plot))
    })
  })
  testthat::expect_error(
    map_immunity(
      dplyr::mutate(summary, adm1 = "B"), boundaries
    ),
    "No summary geographies"
  )
  testthat::expect_error(
    map_immunity(summary, boundaries, breaks = c(0, 10, 5)),
    "increase from zero"
  )
})
