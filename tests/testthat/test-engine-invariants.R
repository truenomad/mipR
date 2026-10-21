testthat::test_that("schedule-age opportunities are not cross-joined", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date("2023-12-01"), age_months = 11, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = "mcv1", coverage = 1
  )
  schedule <- dplyr::tibble(
    dose = "mcv1", age_months = c(9, 10),
    start_date = as.Date(c("2023-01-01", "2023-11-01")),
    end_date = as.Date(c("2023-10-31", "2023-12-31"))
  )
  history <- build_vaccination_history(
    panel, coverage, schedule = schedule, geography = "adm1"
  )
  # The supplied schedule offers two dated opportunities to these births.
  # Each history row must be applied once, without a many-to-many join.
  testthat::expect_equal(nrow(history$history), 2L)
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(result$dose_2, 1)
  testthat::expect_equal(result$dose_3_plus, 0)
})

testthat::test_that("internal keys preserve delimiters in identifiers", {
  locations <- dplyr::tibble(
    adm0 = c("A|B", "A"), adm1 = c("C", "B|C")
  )
  records <- .record_key(locations, c("adm0", "adm1"), "campaign")
  testthat::expect_length(unique(records), 2)
  markers <- .marker_key(list(c("a", "b"), "a|b"))
  testthat::expect_length(unique(markers), 2)
})

testthat::test_that("campaign identifiers cannot collide with dose types", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2022-01-01"),
    month = as.Date("2023-03-01"), age_months = 14, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2022:2023, dose = "mcv1", coverage = 1
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  campaign <- dplyr::tibble(
    adm1 = "A", campaign_id = "mcv1",
    start_date = as.Date("2023-03-01"), end_date = as.Date("2023-03-01"),
    age_min_months = 9, age_max_months = 60, coverage = 1
  )
  history <- build_vaccination_history(
    panel, coverage, campaign, schedule, "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(result$dose_2, 1)
  testthat::expect_equal(result$dose_3_plus, 0)
})

testthat::test_that("survey dose shares condition on eligible birth days", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date(c("2023-09-01", "2023-11-01")),
    age_months = c(8, 10), population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = "mcv1", coverage = 0
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  calendar <- dplyr::tibble(
    adm1 = "A", campaign_id = "partial",
    start_date = as.Date("2023-10-15"), end_date = as.Date("2023-10-15"),
    target_age = "9-59 months"
  )
  evaluation <- dplyr::tibble(
    adm1 = "A", campaign_id = "partial", survey = "survey",
    indicator = c("sia_coverage", "mcv_0_dose"),
    value_pct = c(100, 0)
  )
  campaigns <- prepare_campaigns(calendar, evaluation, geography = "adm1")
  history <- build_vaccination_history(
    panel, coverage, campaigns, schedule, "adm1"
  )
  estimates <- estimate_immunity(history, panel)
  comparison <- check_immunity(estimates, campaigns)$comparison
  # All 15 eligible birth days were vaccinated. The other 16 birth days
  # must not enter the numerator or denominator of this survey comparison.
  testthat::expect_equal(comparison$model_value, 0)
  testthat::expect_equal(comparison$denominator, 150)
})

testthat::test_that("overlapping campaigns never repeat their own dose", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2022-01-01"),
    month = as.Date("2023-03-01"), age_months = 14, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2022:2023, dose = "mcv1", coverage = 0
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  campaigns <- dplyr::tibble(
    adm1 = "A", campaign_id = c("first", "second"),
    start_date = as.Date("2023-03-01"), end_date = as.Date("2023-03-02"),
    age_min_months = 9, age_max_months = 60, coverage = 1
  )
  history <- build_vaccination_history(
    panel, coverage, campaigns, schedule, "adm1"
  )
  result <- estimate_immunity(
    history, panel, coupling = "independent"
  )$immunity
  testthat::expect_equal(result$dose_2, 1)
  testthat::expect_equal(result$dose_3_plus, 0)
  testthat::expect_equal(result$dose_0 + result$dose_1, 0)
})

testthat::test_that("survey windows stay specific to their campaigns", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2022-01-01"),
    month = seq(as.Date("2023-02-01"), as.Date("2023-07-01"), by = "month"),
    age_months = 13:18, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2022:2023, dose = "mcv1", coverage = 0
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  calendar <- dplyr::tibble(
    adm1 = "A", campaign_id = c("first", "second"),
    start_date = as.Date(c("2023-03-01", "2023-06-01")),
    target_age = "9-59 months"
  ) |>
    dplyr::mutate(end_date = .data$start_date)
  evaluation <- tidyr::expand_grid(
    adm1 = "A", campaign_id = c("first", "second"), survey = "survey",
    indicator = c("sia_coverage", "mcv_0_dose")
  ) |>
    dplyr::mutate(
      value_pct = dplyr::case_when(
        .data$indicator == "mcv_0_dose" ~ 0,
        .data$campaign_id == "first" ~ 20,
        .default = 50
      )
    )
  campaigns <- prepare_campaigns(calendar, evaluation, geography = "adm1")
  history <- build_vaccination_history(
    panel, coverage, campaigns, schedule, "adm1"
  )
  estimates <- estimate_immunity(history, panel, coupling = "independent")
  comparison <- check_immunity(estimates, campaigns)$comparison |>
    dplyr::arrange(.data$campaign_id)
  testthat::expect_equal(comparison$model_value, c(0.8, 0.4))
})

testthat::test_that("campaign VE follows actual dates across an age band", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date(c("2023-10-01", "2023-11-01")),
    age_months = c(9, 10), population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = "mcv1", coverage = 0
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  campaign <- dplyr::tibble(
    adm1 = "A", campaign_id = "month",
    start_date = as.Date("2023-10-01"), end_date = as.Date("2023-10-31"),
    age_min_months = 0, age_max_months = 60, coverage = 1
  )
  history <- build_vaccination_history(
    panel, coverage, campaign, schedule, "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity |>
    dplyr::arrange(.data$month)
  # Uniform vaccination days: on October day t, t of 31 birthdays
  # have reached nine months. Each child receives exactly one dose.
  partial <- sum((31 - 1:15) * 0.58 + (1:15) * 0.84) / 31^2
  complete <- 0.58 + (0.84 - 0.58) * 16 / 31
  testthat::expect_equal(
    1 - result$susceptible_share, c(partial, complete)
  )
  testthat::expect_equal(result$dose_1, c(15 / 31, 1))
  testthat::expect_equal(result$dose_2, c(0, 0))
})

testthat::test_that("same-birthday routine doses preserve child identity", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date("2023-10-01"), age_months = 9, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = c("mcv1", "mcv2"), coverage = 1
  )
  schedule <- dplyr::tibble(
    dose = c("mcv1", "mcv2"), age_months = 9
  )
  history <- build_vaccination_history(
    panel, coverage, schedule = schedule, geography = "adm1"
  )
  # Public dose_0/1/2/3_plus columns need at least three tracked doses.
  testthat::expect_error(
    estimate_immunity(history, panel, max_doses = 2),
    "3|three"
  )
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(result$dose_2, 15 / 31)
  testthat::expect_equal(result$dose_1, 0)
  testthat::expect_equal(result$dose_0, 16 / 31)
})

testthat::test_that("future routine doses cannot alter midmonth immunity", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date("2023-10-01"), age_months = 9, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = "mcv1", coverage = 1
  )
  schedule <- dplyr::tibble(
    dose = "mcv1", age_months = 9, start_date = as.Date("2023-10-16")
  )
  campaign <- dplyr::tibble(
    adm1 = "A", campaign_id = "early",
    start_date = as.Date("2023-10-01"), end_date = as.Date("2023-10-01"),
    age_min_months = 6, age_max_months = 60, coverage = 0.4
  )
  history <- build_vaccination_history(
    panel, coverage, campaign, schedule, "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity
  # Only 1 January births have reached nine months on 1 October.
  expected <- 0.4 * (0.84 + 30 * 0.58) / 31
  testthat::expect_equal(1 - result$susceptible_share, expected)
  testthat::expect_equal(result$dose_0, 0.6)
  testthat::expect_equal(result$dose_1, 0.4)
  testthat::expect_equal(result$dose_2, 0)
})

testthat::test_that("complementary campaigns respect birth-day eligibility", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2023-01-01"),
    month = as.Date("2023-11-01"), age_months = 10, population = 310
  )
  coverage <- dplyr::tibble(
    adm1 = "A", year = 2023, dose = "mcv1", coverage = 0
  )
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  campaigns <- dplyr::tibble(
    adm1 = "A", campaign_id = c("first", "second"),
    start_date = as.Date(c("2023-10-15", "2023-10-16")),
    age_min_months = 9, age_max_months = 10, coverage = 1
  ) |>
    dplyr::mutate(end_date = .data$start_date)
  history <- build_vaccination_history(
    panel, coverage, campaigns, schedule, "adm1"
  )
  result <- estimate_immunity(
    history, panel, ve = ve_cdc(1, 1, 1, breaks = 0),
    coupling = "complementary"
  )$immunity
  # Fifteen birth days are reached twice, one once, and fifteen never.
  testthat::expect_equal(result$dose_2, 15 / 31)
  testthat::expect_equal(result$dose_1, 1 / 31)
  testthat::expect_equal(result$dose_0, 15 / 31)
  testthat::expect_equal(result$susceptible_share, 15 / 31)
})
