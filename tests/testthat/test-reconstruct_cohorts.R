kano <- data.frame(
  adm1 = "Kano",
  year = rep(c(2024, 2025), each = 2),
  age = rep(0:1, 2),
  sum = c(1180, 1130, 1200, 1150)
)

testthat::test_that("make_anchors converts years of age to months", {
  anchors <- make_anchors(kano, geography = "adm1")

  testthat::expect_named(
    anchors,
    c("adm1", "ref_date", "age_lower", "age_upper", "population")
  )
  testthat::expect_identical(anchors$ref_date[3], as.Date("2025-01-01"))
  testthat::expect_identical(anchors$age_lower, c(0L, 12L, 0L, 12L))
  testthat::expect_identical(anchors$age_upper, c(12L, 24L, 12L, 24L))
  testthat::expect_error(
    make_anchors(kano, geography = "adm2"),
    "not in"
  )
})

testthat::test_that("a single anchor is spread over birth months by day", {
  anchors <- data.frame(
    geography = "A",
    ref_date = as.Date("2025-01-01"),
    age_lower = 0, age_upper = 12,
    population = 366
  )
  out <- reconstruct_cohorts(anchors)
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
  anchors <- data.frame(
    geography = "A",
    ref_date = as.Date("2025-07-01"),
    age_lower = 0, age_upper = 12,
    population = 365
  )
  out <- reconstruct_cohorts(anchors)

  births <- unique(out$cohorts$birth_month)
  testthat::expect_identical(min(births), as.Date("2024-07-01"))
  testthat::expect_identical(max(births), as.Date("2025-06-01"))
})

testthat::test_that("cohorts are interpolated along the cohort", {
  anchors <- make_anchors(kano, geography = "adm1")
  out <- reconstruct_cohorts(anchors, geography = "adm1")

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
  anchors <- make_anchors(kano, geography = "adm1")
  out <- reconstruct_cohorts(
    anchors,
    geography = "adm1",
    start = "2024-06", end = "2024-12", max_age_months = 6
  )

  testthat::expect_true(all(out$cohorts$age_months <= 6))
  testthat::expect_identical(
    range(out$cohorts$month),
    as.Date(c("2024-06-01", "2024-12-01"))
  )
})

testthat::test_that("reconstruct_cohorts validates anchors", {
  bad <- data.frame(
    geography = "A", ref_date = as.Date("2025-01-01"),
    age_lower = 12, age_upper = 12, population = 10
  )
  testthat::expect_error(reconstruct_cohorts(bad), "age_upper")
  testthat::expect_error(
    reconstruct_cohorts(bad[, -5]),
    "make_anchors"
  )
})
