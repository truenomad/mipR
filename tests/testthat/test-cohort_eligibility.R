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
