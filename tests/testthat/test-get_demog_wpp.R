testthat::test_that(".match_wpp_country prefers an exact match", {
  available <- c("Congo", "Democratic Republic of the Congo", "Kenya")

  testthat::expect_identical(.match_wpp_country("congo", available), "Congo")
  testthat::expect_identical(.match_wpp_country("KENYA", available), "Kenya")
})

testthat::test_that(".match_wpp_country falls back to a partial match", {
  available <- c("Democratic Republic of the Congo", "Kenya")

  testthat::expect_message(
    out <- .match_wpp_country("Republic of the Congo", available),
    "Partial match"
  )
  testthat::expect_identical(out, "Democratic Republic of the Congo")
  testthat::expect_error(
    .match_wpp_country("Narnia", available),
    "not found"
  )
})

testthat::test_that(".closest_year picks the nearest year", {
  testthat::expect_identical(.closest_year(2000:2023, 2150), 2023L)
  testthat::expect_identical(.closest_year(2000:2023, 1990), 2000L)
})

testthat::test_that("get_demog_wpp returns EMOD demographic inputs", {
  testthat::skip_if_not_installed("wpp2024")

  out <- suppressMessages(get_demog_wpp("Burundi", 2022))

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
