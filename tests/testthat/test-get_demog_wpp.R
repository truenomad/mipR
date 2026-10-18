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

testthat::test_that("get_demog_wpp returns rates and age distribution", {
  testthat::skip_if_not_installed("wpp2024")

  out <- suppressMessages(get_demog_wpp("NGA", 2022:2025))

  testthat::expect_named(out, c("rates", "age_distribution"))
  testthat::expect_identical(unique(out$rates$country), "NIGERIA")
  testthat::expect_identical(unique(out$rates$iso3), "NGA")
  testthat::expect_identical(
    names(out$age_distribution)[1:3],
    c("country", "iso3", "year")
  )
  testthat::expect_identical(out$rates$year, 2022:2025)
  testthat::expect_equal(
    out$rates$birth_rate_daily,
    out$rates$crude_birth_rate / 1000 / 365
  )

  ad <- out$age_distribution
  testthat::expect_identical(nrow(ad), 4L * 101L)
  final <- ad$cumulative_proportion[ad$age == 100]
  testthat::expect_equal(final, rep(1, 4))
  testthat::expect_identical(ad$age_days[ad$age == 0], rep(365, 4))
})

testthat::test_that("get_demog_wpp matches published WPP 2024 rates", {
  testthat::skip_if_not_installed("wpp2024")

  # UN WPP 2024 values for Nigeria, as also published by the World Bank
  out <- suppressMessages(get_demog_wpp("NGA", c(2000, 2023)))

  testthat::expect_equal(out$rates$crude_birth_rate, c(43.892, 32.954))
  testthat::expect_equal(out$rates$crude_death_rate, c(17.439, 11.740))
})
