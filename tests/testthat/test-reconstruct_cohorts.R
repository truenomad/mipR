# reconstruct_cohorts ----------------------------------------------------------

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
  out <- reconstruct_cohorts(pop, geography = "geography")
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

testthat::test_that("a 1 July reference date shifts the birth window", {
  pop <- data.frame(geography = "A", year = 2025, age = 0, pop = 365)
  out <- reconstruct_cohorts(pop, geography = "geography", ref_date = "07-01")

  births <- unique(out$cohorts$birth_month)
  testthat::expect_identical(min(births), as.Date("2024-07-01"))
  testthat::expect_identical(max(births), as.Date("2025-06-01"))
})

testthat::test_that("cohorts are interpolated along the cohort", {
  out <- reconstruct_cohorts(kano, geography = "adm1")

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
  out <- reconstruct_cohorts(
    kano,
    geography = "adm1",
    start = "2024-06", end = "2024-12", max_age_months = 6
  )

  testthat::expect_true(all(out$cohorts$age_months <= 6))
  testthat::expect_identical(
    range(out$cohorts$month),
    as.Date(c("2024-06-01", "2024-12-01"))
  )
})

testthat::test_that("reconstruct_cohorts validates its input", {
  testthat::expect_error(
    reconstruct_cohorts(kano, geography = "adm2"),
    "not in"
  )
  bad_age <- transform(kano, age = c(0, -1, 0, 1))
  testthat::expect_error(
    reconstruct_cohorts(bad_age, geography = "adm1"),
    "whole years"
  )
  bad_pop <- transform(kano, pop = c(1, NA, 1, 1))
  testthat::expect_error(
    reconstruct_cohorts(bad_pop, geography = "adm1"),
    "zero or more"
  )
  testthat::expect_error(
    reconstruct_cohorts(kano, geography = "adm1", ref_date = "1 Jan"),
    "MM-DD"
  )
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
