# reconstruct_birth_cohorts ----------------------------------------------------

kano <- data.frame(
  adm1 = "Kano",
  year = rep(c(2024, 2025), each = 2),
  age = rep(0:1, 2),
  pop = c(1180, 1130, 1200, 1150)
)

testthat::test_that(".make_anchors converts years of age to months", {
  anchors <- .make_anchors(kano, "adm1", "year", "age", "pop", "01-01")

  testthat::expect_named(
    anchors,
    c("adm1", "ref_date", "age_lower", "age_upper", "population")
  )
  testthat::expect_identical(anchors$ref_date[3], as.Date("2025-01-01"))
  testthat::expect_identical(anchors$age_lower, c(0L, 12L, 0L, 12L))
  testthat::expect_identical(anchors$age_upper, c(12L, 24L, 12L, 24L))
})

testthat::test_that("a single anchor is spread over birth months by day", {
  pop <- data.frame(geography = "A", year = 2025, age = 0, pop = 366)
  out <- reconstruct_birth_cohorts(
    pop, geography = "geography", method = "uniform"
  )
  cohorts <- out$cohorts

  # aged 0 on 1 January 2025 means born in 2024, a leap year
  first_month <- cohorts[cohorts$month == as.Date("2025-01-01"), ]
  testthat::expect_identical(
    first_month$birth_month,
    seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  )
  testthat::expect_equal(
    first_month$population,
    c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  )
  testthat::expect_identical(first_month$age_months, 12:1)
  testthat::expect_equal(out$anchor_check$difference, 0)
})

testthat::test_that("ref_date can vary by row through a column", {
  pop <- data.frame(
    geography = "A",
    year = c(2014, 2015),
    age = 0,
    pop = c(365, 365),
    ref = c("07-01", "01-01")
  )
  out <- reconstruct_birth_cohorts(
    pop, geography = "geography", ref_date = "ref", method = "uniform"
  )

  testthat::expect_identical(
    out$anchor_check$ref_date,
    as.Date(c("2014-07-01", "2015-01-01"))
  )
  # aged 0 on 1 July 2014: born July 2013 to June 2014
  births <- unique(out$cohorts$birth_month)
  testthat::expect_identical(min(births), as.Date("2013-07-01"))
  testthat::expect_identical(max(births), as.Date("2014-12-01"))
  testthat::expect_error(
    reconstruct_birth_cohorts(
      transform(pop, ref = c("07-01", "July")),
      geography = "geography", ref_date = "ref", method = "uniform"
    ),
    "MM-DD"
  )
})

testthat::test_that("a 1 July reference date shifts the birth window", {
  pop <- data.frame(geography = "A", year = 2025, age = 0, pop = 365)
  out <- reconstruct_birth_cohorts(
    pop, geography = "geography", ref_date = "07-01", method = "uniform"
  )

  births <- unique(out$cohorts$birth_month)
  testthat::expect_identical(min(births), as.Date("2024-07-01"))
  testthat::expect_identical(max(births), as.Date("2025-06-01"))
})

testthat::test_that("cohorts are interpolated along the cohort", {
  out <- reconstruct_birth_cohorts(kano, geography = "adm1", method = "uniform")

  # the January 2024 cohort is age 0 on 1 Jan 2025 and has one anchor;
  # the January 2023 cohort is seen at age 0 (2024) and age 1 (2025)
  jan23 <- out$cohort_changes[
    out$cohort_changes$birth_month == as.Date("2023-01-01"),
  ]
  testthat::expect_equal(jan23$from_population, 1180 * 31 / 365)
  testthat::expect_equal(jan23$to_population, 1150 * 31 / 365)

  panel <- out$cohorts[out$cohorts$birth_month == as.Date("2023-01-01"), ]
  mid_2024 <- panel$population[panel$month == as.Date("2024-07-01")]
  testthat::expect_true(
    mid_2024 > min(jan23$from_population, jan23$to_population) &&
      mid_2024 < max(jan23$from_population, jan23$to_population)
  )
  testthat::expect_equal(out$anchor_check$difference, rep(0, 4))
})

testthat::test_that("max_age_months and the date range limit the output", {
  out <- reconstruct_birth_cohorts(
    kano,
    geography = "adm1", method = "uniform",
    start = "2024-06", end = "2024-12", max_age_months = 6
  )

  testthat::expect_true(all(out$cohorts$age_months <= 6))
  testthat::expect_identical(
    range(out$cohorts$month),
    as.Date(c("2024-06-01", "2024-12-01"))
  )
})

testthat::test_that("reconstruct_birth_cohorts validates its input", {
  testthat::expect_error(
    reconstruct_birth_cohorts(kano, geography = "adm2", method = "uniform"),
    "not in"
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(kano, geography = "adm1", method = "survival"),
    "mortality"
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(
      kano, geography = "adm1", method = "uniform", seasonality = 1:6
    ),
    "12 non-negative"
  )
  bad_age <- transform(kano, age = c(0, -1, 0, 1))
  testthat::expect_error(
    reconstruct_birth_cohorts(bad_age, geography = "adm1", method = "uniform"),
    "whole years"
  )
  bad_pop <- transform(kano, pop = c(1, NA, 1, 1))
  testthat::expect_error(
    reconstruct_birth_cohorts(bad_pop, geography = "adm1", method = "uniform"),
    "zero or more"
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(
      kano, geography = "adm1", ref_date = "1 Jan", method = "uniform"
    ),
    "MM-DD"
  )
})

# a population with one birth a day and no deaths, observed on 1 January
# 2024 and 2025 at ages 0 and 1
steady <- data.frame(
  adm1 = "A",
  year = rep(c(2024, 2025), each = 2),
  age = rep(0:1, 2),
  pop = c(365, 365, 366, 365)
)
no_deaths <- data.frame(
  year = rep(2020:2026, each = 101), age = rep(0:100, 7), mx = 0
)

testthat::test_that(".infant_survival reproduces infant death probability", {
  m0 <- c(0.03, 0.07, 0.12)
  a0 <- ifelse(m0 >= 0.107, 0.34, 0.049 + 2.742 * m0)
  q0 <- m0 / (1 + (1 - a0) * m0)
  first_year <- .infant_survival(m0, 0L) * .infant_survival(m0, 1L)^11

  testthat::expect_equal(first_year, 1 - q0)
  testthat::expect_equal(.infant_survival(m0, 0L), 1 - 0.5 * q0)
})

testthat::test_that("survival method recovers births with no deaths", {
  out <- reconstruct_birth_cohorts(
    steady, geography = "adm1", method = "survival", mortality = no_deaths
  )

  births <- out$births
  jan24 <- births$births[births$birth_month == as.Date("2024-01-01")]
  feb23 <- births$births[births$birth_month == as.Date("2023-02-01")]
  testthat::expect_equal(jan24, 31)
  testthat::expect_equal(feb23, 28)
  testthat::expect_true(all(out$anchor_check$within_tolerance))
  testthat::expect_equal(out$cohort_changes$expected_change, rep(0, 12))
})

testthat::test_that("survival method follows cohorts through mortality", {
  deaths <- transform(no_deaths, mx = 0.02)
  out <- reconstruct_birth_cohorts(
    steady, geography = "adm1", method = "survival", mortality = deaths,
    start = "2024-01", end = "2024-12"
  )

  jan23 <- out$cohorts[out$cohorts$birth_month == as.Date("2023-01-01"), ]
  testthat::expect_true(all(diff(jan23$population) < 0))
  testthat::expect_true(all(out$cohort_changes$expected_change < 0))
})

testthat::test_that("constrained method meets anchors within tolerance", {
  # the 2025 age-1 anchor is 20% above what the 2024 age-0 anchor implies
  uneven <- transform(steady, pop = c(365, 365, 366, 438))
  survival <- reconstruct_birth_cohorts(
    uneven, geography = "adm1", method = "survival", mortality = no_deaths
  )
  constrained <- reconstruct_birth_cohorts(
    uneven, geography = "adm1", method = "constrained",
    mortality = no_deaths, tolerance = 0.05
  )

  # the two anchors on the 2023 cohort cannot both be met, so the
  # constrained fit balances them and flags them
  checks <- constrained$anchor_check
  conflict <- checks$ref_date == as.Date("2024-01-01") & checks$age_lower == 0 |
    checks$ref_date == as.Date("2025-01-01") & checks$age_lower == 12
  testthat::expect_true(all(checks$within_tolerance[!conflict]))
  testthat::expect_lt(
    max(abs(checks$relative_difference[conflict])),
    max(abs(survival$anchor_check$relative_difference)) + 1e-9
  )
  testthat::expect_true(
    all(abs(constrained$cohort_changes$unexplained_change - 0.2) < 1e-9)
  )
  testthat::expect_gt(constrained$metadata$iterations, 0)
})

testthat::test_that("cohorts past the last anchor follow the births trend", {
  births <- data.frame(year = 2023:2026, births = c(1, 1, 1, 2))
  out <- reconstruct_birth_cohorts(
    steady, geography = "adm1", method = "survival", mortality = no_deaths,
    births = births, end = "2026-06"
  )

  b <- out$births
  testthat::expect_identical(
    unique(b$basis[b$birth_month >= as.Date("2025-01-01")]),
    "extended"
  )
  jan25 <- b$births[b$birth_month == as.Date("2025-01-01")]
  jan26 <- b$births[b$birth_month == as.Date("2026-01-01")]
  testthat::expect_equal(jan25, 31)
  testthat::expect_equal(jan26, 62)
  june26 <- out$cohorts[out$cohorts$month == as.Date("2026-06-01"), ]
  testthat::expect_true("extended" %in% june26$basis)
})

testthat::test_that("seasonality shifts births between months", {
  season <- c(2, rep(1, 11))
  out <- reconstruct_birth_cohorts(
    steady, geography = "adm1", method = "survival", mortality = no_deaths,
    seasonality = season
  )

  b <- out$births
  jan <- b$births[b$birth_month == as.Date("2024-01-01")]
  mar <- b$births[b$birth_month == as.Date("2024-03-01")]
  testthat::expect_equal(jan / mar, 2)
  testthat::expect_true(all(out$anchor_check$within_tolerance))
})

# cohort_eligibility -----------------------------------------------------------

testthat::test_that("routine eligibility is fractional within the month", {
  testthat::expect_equal(
    cohort_eligibility("2024-01", start = "2024-10-15", age_min_months = 9),
    15 / 31
  )
  testthat::expect_equal(
    cohort_eligibility(
      "2024-01", start = "2024-10-15",
      age_min_months = 9, convention = "midmonth"
    ),
    1
  )
  testthat::expect_equal(
    cohort_eligibility(
      c("2024-01", "2023-12", "2024-02"),
      start = "2024-10-15", age_min_months = 9
    ),
    c(15 / 31, 1, 0)
  )
})

testthat::test_that("campaign eligibility uses the whole campaign interval", {
  # 9-59 months during 1-10 March 2025
  share <- cohort_eligibility(
    c("2020-02", "2020-03", "2024-06", "2024-05"),
    start = "2025-03-01", end = "2025-03-10",
    age_min_months = 9, age_max_months = 60
  )

  # born March 2020: turns 60 months in March 2025, eligible if still
  # under 60 months on 1 March, so born after 1 March 2020
  testthat::expect_equal(share[1], 0)
  testthat::expect_equal(share[2], 30 / 31)
  # born June 2024: reaches 9 months by 10 March 2025 only if born by
  # 10 June 2024
  testthat::expect_equal(share[3], 10 / 30)
  testthat::expect_equal(share[4], 1)
})

testthat::test_that("cohort_eligibility validates its arguments", {
  testthat::expect_error(
    cohort_eligibility("2024-01", start = "2024-10-15", end = "2024-10-01"),
    "not before"
  )
  testthat::expect_error(
    cohort_eligibility(
      "2024-01", start = "2024-10-15",
      age_min_months = 9, age_max_months = 9
    ),
    "above"
  )
})

# date helpers -----------------------------------------------------------------

testthat::test_that(".add_months keeps the day or moves to month end", {
  testthat::expect_identical(
    .add_months(as.Date("2024-01-31"), 1),
    as.Date("2024-02-29")
  )
  testthat::expect_identical(
    .add_months(as.Date("2025-03-15"), -14),
    as.Date("2024-01-15")
  )
  testthat::expect_identical(
    .add_months(as.Date("2025-01-01"), c(-12, 9)),
    as.Date(c("2024-01-01", "2025-10-01"))
  )
})

testthat::test_that("month index round-trips and counts days", {
  idx <- .month_index(as.Date(c("2024-02-10", "2025-12-31")))
  testthat::expect_identical(
    .month_from_index(idx),
    as.Date(c("2024-02-01", "2025-12-01"))
  )
  testthat::expect_identical(.days_in_month(idx), c(29L, 31L))
  testthat::expect_identical(.as_date("2025-03"), as.Date("2025-03-01"))
  testthat::expect_error(.as_date("March 2025"), "must be a Date")
})
