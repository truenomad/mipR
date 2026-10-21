testthat::test_that("eligibility rejects truncated or undefined ages", {
  purrr::walk(list(NA_real_, -1, 1.5, c(0, 1), factor("9")), \(age) {
    testthat::expect_error(
      cohort_eligibility("2023-01", "2023-10-15", age_min_months = age),
      "whole month"
    )
  })
  testthat::expect_error(
    cohort_eligibility("2023-01", "2023-10-15", age_max_months = 9.5),
    "whole month"
  )
})

testthat::test_that("zero seasonality preserves finite births and zeros", {
  population <- dplyr::tibble(
    adm1 = "A", year = 2024, age = 0:1, pop = c(334, 334)
  )
  mortality <- dplyr::tibble(year = 2023, age = 0:2, mx = 0)
  purrr::walk(c("survival", "constrained"), \(method) {
    result <- reconstruct_birth_cohorts(
      population, "adm1", mortality = mortality, method = method,
      seasonality = c(0, rep(1, 11)), end = "2024-12"
    )
    projected <- result$births |>
      dplyr::filter(.data$basis == "extended")
    january <- result$births |>
      dplyr::filter(format(.data$birth_month, "%m") == "01")
    testthat::expect_true(all(is.finite(projected$births)))
    testthat::expect_equal(january$births, rep(0, nrow(january)))
    testthat::expect_equal(sum(projected$births), 335)
    testthat::expect_equal(result$anchor_check$rebuilt, c(334, 334))
  })
})

testthat::test_that("zero tolerance cannot corrupt constrained births", {
  population <- dplyr::tibble(
    adm1 = "A", year = 2024, age = 0, pop = 365
  )
  mortality <- dplyr::tibble(year = 2023, age = 0:2, mx = 0)
  testthat::expect_error(
    reconstruct_birth_cohorts(
      population, "adm1", mortality = mortality, tolerance = 0
    ),
    "must be positive"
  )
  exact <- reconstruct_birth_cohorts(
    population, "adm1", method = "uniform", tolerance = 0
  )
  testthat::expect_equal(exact$anchor_check$rebuilt, 365)
  testthat::expect_true(all(exact$anchor_check$within_tolerance))
})

testthat::test_that("sparse demographic years use nearest available years", {
  births <- dplyr::tibble(year = c(2022, 2024), births = c(100, 200))
  months <- .month_index(as.Date(c("2021-01-01", "2023-01-01",
                                  "2024-01-01", "2025-01-01")))
  testthat::expect_equal(
    .national_births(births, months), c(100, 100, 200, 200)
  )
  mortality <- tidyr::expand_grid(year = c(2022, 2024), age = 0:2) |>
    dplyr::mutate(mx = dplyr::if_else(.data$year == 2022, 0, 0.12))
  sparse <- .survival_table(
    mortality, .month_index(as.Date("2022-01-01")),
    .month_index(as.Date("2024-01-01"))
  ) |>
    dplyr::filter(
      .data$birth_index == .month_index(as.Date("2022-01-01")),
      .data$age_months == 24
    )
  testthat::expect_equal(sparse$survival, 1)
})

testthat::test_that("invalid anchors and demographic trends fail explicitly", {
  population <- dplyr::tibble(
    adm1 = "A", year = 2024, age = 0, pop = 365
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(
      dplyr::bind_rows(population, population), "adm1", method = "uniform"
    ),
    "Duplicate"
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(
      dplyr::mutate(population, pop = Inf), "adm1", method = "uniform"
    ),
    "finite"
  )
  testthat::expect_error(
    reconstruct_birth_cohorts(
      population, "adm1", method = "uniform", tolerance = NA_real_
    ),
    "finite"
  )
  testthat::expect_error(
    .national_births(dplyr::tibble(year = 2023, births = 0), 2024 * 12),
    "positive finite"
  )
  testthat::expect_error(.infant_survival(4, 0), "death probability")
  testthat::expect_error(
    reconstruct_birth_cohorts(
      dplyr::mutate(population, pop = factor(.data$pop)),
      "adm1", method = "uniform"
    ),
    "numeric column"
  )
})
