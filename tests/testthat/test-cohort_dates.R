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
