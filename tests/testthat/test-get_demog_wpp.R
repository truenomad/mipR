locations <- data.frame(
  country_code = c(178, 180, 566, 702),
  name = c("Congo", "Democratic Republic of the Congo", "Nigeria", "Singapore")
)

testthat::test_that(".match_wpp_country accepts ISO3 codes", {
  testthat::expect_identical(.match_wpp_country("NGA", locations), "Nigeria")
  testthat::expect_identical(
    .match_wpp_country("cod", locations),
    "Democratic Republic of the Congo"
  )
})

testthat::test_that(".match_wpp_country prefers an exact name match", {
  testthat::expect_identical(.match_wpp_country("congo", locations), "Congo")
  testthat::expect_identical(
    .match_wpp_country("NIGERIA", locations),
    "Nigeria"
  )
})

testthat::test_that(".match_wpp_country uses a unique partial match only", {
  testthat::expect_message(
    out <- .match_wpp_country("Singa", locations),
    "Partial match"
  )
  testthat::expect_identical(out, "Singapore")
  testthat::expect_error(
    .match_wpp_country("ongo", locations),
    "several"
  )
  testthat::expect_error(.match_wpp_country("Narnia", locations), "not found")
})

testthat::test_that(".closest_year picks the nearest year", {
  testthat::expect_identical(.closest_year(2000:2023, 2150), 2023L)
  testthat::expect_identical(.closest_year(2000:2023, 1990), 2000L)
})

testthat::test_that("get_demog_wpp returns EMOD demographic inputs", {
  testthat::skip_if_not_installed("wpp2024")

  out <- suppressMessages(get_demog_wpp("BDI", 2022))

  testthat::expect_named(
    out,
    c(
      "age_distribution", "crude_birth_rate", "crude_death_rate",
      "birth_rate_daily", "country", "year"
    )
  )
  cdf <- out$age_distribution$DistributionValues
  testthat::expect_false(is.unsorted(cdf))
  testthat::expect_equal(utils::tail(cdf, 1), 1)
  testthat::expect_equal(out$birth_rate_daily, out$crude_birth_rate / 365)
})

testthat::test_that("get_demog_wpp returns one entry per year", {
  testthat::skip_if_not_installed("wpp2024")

  out <- suppressMessages(get_demog_wpp("NGA", 2022:2025))

  testthat::expect_named(out, as.character(2022:2025))
  testthat::expect_identical(out[["2025"]]$year, 2025L)
  cbr <- vapply(out, function(x) x$crude_birth_rate, numeric(1))
  testthat::expect_false(anyNA(cbr))
})
