# fixtures ---------------------------------------------------------------------

# one January 2023 cohort of 310 children followed month by month
.one_cohort <- function(months, population = 310) {
  dplyr::tibble(
    adm1 = "A",
    birth_month = as.Date("2023-01-01"),
    month = as.Date(paste0(months, "-01")),
    age_months = .month_index(.data$month) -
      .month_index(.data$birth_month),
    population = population
  )
}

# routine coverage for one dose and years
.routine <- function(years, coverage, dose = "mcv1") {
  dplyr::tibble(adm1 = "A", year = years, dose = dose, coverage = coverage)
}

# a campaign record on a range of dates
.campaign <- function(id, start, end = start, coverage = 1, age_min = 9,
                     age_max = 60) {
  dplyr::tibble(
    adm1 = "A", campaign_id = id,
    start_date = as.Date(start), end_date = as.Date(end),
    age_min_months = age_min, age_max_months = age_max, coverage = coverage
  )
}

.value_at <- function(immunity, month, column) {
  immunity |>
    dplyr::filter(.data$month == as.Date(paste0(!!month, "-01"))) |>
    dplyr::pull(dplyr::all_of(column))
}

# causal monthly reporting -----------------------------------------------------

testthat::test_that("mid-month reports never go negative and stay causal", {
  panel <- .one_cohort(c("2023-10", "2023-11"))
  schedule <- dplyr::tibble(
    dose = "mcv1", age_months = 9, start_date = as.Date("2023-10-16")
  )
  history <- build_vaccination_history(
    panel, .routine(2023, 0.6), .campaign("c", "2023-10-01", coverage = 0.4,
                                        age_min = 6),
    schedule, "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity

  # 15 October: only the campaign has happened; born on 1 January are 9
  # months old on 1 October (VE 0.84), the other 30 birth days are younger
  testthat::expect_equal(.value_at(result, "2023-10", "dose_1"), 0.4)
  testthat::expect_equal(.value_at(result, "2023-10", "dose_0"), 0.6)
  testthat::expect_equal(
    1 - .value_at(result, "2023-10", "susceptible_share"),
    0.4 * (0.84 + 30 * 0.58) / 31
  )

  # 15 November: born 16-31 January also had MCV1 (coverage 0.6) after the
  # campaign; dependent coupling reaches the 0.4 vaccinated first
  protected_late <- 0.4 * 0.58 + 0.4 * (1 - 0.58) * 0.84 + 0.2 * 0.84
  protected_early <- c(0.4 * 0.84, rep(0.4 * 0.58, 14))
  testthat::expect_equal(
    1 - .value_at(result, "2023-11", "susceptible_share"),
    (sum(protected_early) + 16 * protected_late) / 31
  )
  testthat::expect_equal(.value_at(result, "2023-11", "dose_2"), 16 * 0.4 / 31)
  testthat::expect_equal(
    .value_at(result, "2023-11", "dose_1"), (15 * 0.4 + 16 * 0.2) / 31
  )
  testthat::expect_equal(
    .value_at(result, "2023-11", "dose_0"), (15 * 0.6 + 16 * 0.4) / 31
  )
  shares <- result |>
    dplyr::select("dose_0", "dose_1", "dose_2", "dose_3_plus")
  testthat::expect_true(all(shares >= 0 & shares <= 1))
  testthat::expect_equal(rowSums(shares), rep(1, nrow(shares)))
})

testthat::test_that("a multi-month campaign evolves month by month", {
  panel <- dplyr::tibble(
    adm1 = "A", birth_month = as.Date("2022-01-01"),
    month = as.Date(c("2023-10-01", "2023-11-01", "2023-12-01")),
    age_months = 21:23, population = 100
  )
  history <- build_vaccination_history(
    panel, .routine(2022:2023, 0),
    .campaign("long", "2023-10-01", "2023-11-30"),
    dplyr::tibble(dose = "mcv1", age_months = 9), "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity |>
    dplyr::arrange(.data$month)

  # 61 campaign days, 15 by 15 October and 46 by 15 November; age 21+
  # months so VE is 0.95 throughout
  testthat::expect_equal(result$dose_1, c(15 / 61, 46 / 61, 1))
  testthat::expect_equal(
    result$susceptible_share, 1 - c(15 / 61, 46 / 61, 1) * 0.95
  )
})

testthat::test_that("repeated campaigns reach only their eligible days", {
  panel <- .one_cohort("2023-12")
  campaigns <- dplyr::bind_rows(
    .campaign("a", "2023-10-01"), .campaign("b", "2023-10-20")
  )
  history <- build_vaccination_history(
    panel, .routine(2023, 0), campaigns,
    dplyr::tibble(dose = "mcv1", age_months = 9), "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity

  # born 1 January: both campaigns (9 months on 1 October); born 2-20
  # January: campaign b only; born 21-31 January: neither
  testthat::expect_equal(result$dose_2, 1 / 31)
  testthat::expect_equal(result$dose_1, 19 / 31)
  testthat::expect_equal(result$dose_0, 11 / 31)
  testthat::expect_equal(
    result$susceptible_share, (0.16^2 + 19 * 0.16 + 11) / 31
  )
})

testthat::test_that("campaign VE uses the age on the dose date", {
  panel <- .one_cohort("2024-02")
  history <- build_vaccination_history(
    panel, .routine(2023:2024, 0), .campaign("twelve", "2024-01-01"),
    dplyr::tibble(dose = "mcv1", age_months = 9), "adm1"
  )
  result <- estimate_immunity(history, panel)$immunity

  # only children born on 1 January are 12 months old on 1 January 2024
  testthat::expect_equal(
    1 - result$susceptible_share, (0.95 + 30 * 0.84) / 31
  )
})

# schedules and coverage years -------------------------------------------------

kano_pop <- dplyr::tibble(
  adm1 = "Kano", year = c(2024, 2024, 2025, 2025), age = c(0, 1, 0, 1),
  pop = c(1200, 1150, 1240, 1180)
)
kano_cohorts <- reconstruct_birth_cohorts(
  kano_pop, geography = "adm1", method = "uniform",
  start = "2022-06", end = "2025-06"
)
kano_coverage <- dplyr::tibble(
  adm1 = "Kano", year = 2022:2025, dose = "mcv1", coverage = 0.6
)

testthat::test_that("a schedule split into periods gives the same results", {
  whole <- dplyr::tibble(dose = "mcv1", age_months = 9)
  split <- dplyr::tibble(
    dose = "mcv1", age_months = 9,
    start_date = as.Date(c("1900-01-01", "2024-01-01")),
    end_date = as.Date(c("2023-12-31", "2999-12-31"))
  )
  history_whole <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = whole, geography = "adm1"
  )
  history_split <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = split, geography = "adm1"
  )

  testthat::expect_equal(history_split$history, history_whole$history)
  testthat::expect_equal(
    estimate_immunity(history_split, kano_cohorts)$immunity,
    estimate_immunity(history_whole, kano_cohorts)$immunity
  )
})

testthat::test_that("a gap in the schedule leaves two runs of birth days", {
  gapped <- dplyr::tibble(
    dose = "mcv1", age_months = 9,
    start_date = as.Date(c("1900-01-01", "2023-10-21")),
    end_date = as.Date(c("2023-10-10", "2999-12-31"))
  )
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = gapped, geography = "adm1"
  )
  jan23 <- history$history |>
    dplyr::filter(.data$birth_month == as.Date("2023-01-01"))

  testthat::expect_identical(jan23$day_from, c(1L, 21L))
  testthat::expect_identical(jan23$day_to, c(10L, 31L))
  testthat::expect_equal(jan23$eligible_share, c(10, 11) / 31)
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, kano_coverage, geography = "adm1",
      schedule = dplyr::tibble(dose = "mcv1", age_months = c(9, 12))
    ),
    "overlapping periods"
  )
})

testthat::test_that("missing coverage years use the nearest year", {
  coverage <- dplyr::tibble(
    adm1 = "Kano", year = c(2022, 2024), dose = "mcv1",
    coverage = c(0.5, 0.7)
  )
  history <- build_vaccination_history(
    kano_cohorts, coverage,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  used <- history$history |>
    dplyr::mutate(year = as.integer(format(.data$month, "%Y"))) |>
    dplyr::distinct(.data$year, .data$coverage_year, .data$coverage)

  # 2023 is a tie between 2022 and 2024 and takes the earlier year; 2025
  # takes 2024
  testthat::expect_equal(
    dplyr::filter(used, .data$year == 2023)$coverage_year, 2022
  )
  testthat::expect_equal(
    dplyr::filter(used, .data$year == 2023)$coverage, 0.5
  )
  testthat::expect_equal(
    dplyr::filter(used, .data$year == 2025)$coverage_year, 2024
  )
  substituted <- history$diagnostics |>
    dplyr::filter(.data$check == "coverage_year_substituted")
  testthat::expect_setequal(substituted$year, c(2023, 2025))
})

# validation -------------------------------------------------------------------

testthat::test_that("inputs are validated before they reach the engine", {
  schedule <- dplyr::tibble(dose = "mcv1", age_months = 9)
  factor_dose <- dplyr::mutate(kano_coverage, dose = factor(.data$dose))
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, factor_dose, schedule = schedule, geography = "adm1"
    ),
    "wrong type"
  )
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, dplyr::mutate(kano_coverage, year = .data$year + 0.5),
      schedule = schedule, geography = "adm1"
    ),
    "whole years"
  )
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts,
      dplyr::mutate(kano_coverage, coverage_lower = 0.7),
      schedule = schedule, geography = "adm1"
    ),
    "both bounds"
  )
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, kano_coverage,
      .campaign("too-high", "2024-03-01", coverage = 1.04) |>
        dplyr::mutate(adm1 = "Kano"),
      schedule, "adm1"
    ),
    "too-high"
  )
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, kano_coverage,
      .campaign("half", "2024-03-01", age_min = 8.5) |>
        dplyr::mutate(adm1 = "Kano"),
      schedule, "adm1"
    ),
    "whole months"
  )
  testthat::expect_error(
    build_vaccination_history(
      kano_cohorts, kano_coverage,
      dplyr::bind_rows(
        .campaign("twice", "2024-03-01"), .campaign("twice", "2024-03-01")
      ) |>
        dplyr::mutate(adm1 = "Kano"),
      schedule, "adm1"
    ),
    "more than one record"
  )
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = schedule, geography = "adm1"
  )
  testthat::expect_error(
    estimate_immunity(history, kano_cohorts, max_doses = 2), "3 or more"
  )
  testthat::expect_error(
    immunity_draws(history, kano_cohorts, n_draws = 0), "n_draws"
  )
  testthat::expect_error(
    immunity_draws(history, kano_cohorts, rho_years = 2), "rho_years"
  )
  testthat::expect_error(
    immunity_draws(history, kano_cohorts, max_doses = 2), "3 or more"
  )
  testthat::expect_error(ve_cdc(breaks = c(9, 0)), "increasing")
})

# empty histories --------------------------------------------------------------

testthat::test_that("no opportunities leave every child unvaccinated", {
  elsewhere <- dplyr::mutate(kano_coverage, adm1 = "Lagos")
  history <- build_vaccination_history(
    kano_cohorts, elsewhere,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  testthat::expect_identical(nrow(history$history), 0L)
  testthat::expect_true(
    "missing_routine_coverage" %in% history$diagnostics$check
  )

  immunity <- estimate_immunity(history, kano_cohorts)
  testthat::expect_true(all(immunity$immunity$susceptible_share == 1))
  testthat::expect_true(all(immunity$immunity$dose_0 == 1))
  testthat::expect_identical(nrow(immunity$contributions), 0L)

  draws <- immunity_draws(
    history, kano_cohorts, n_draws = 4, at = "2025-01"
  )
  testthat::expect_true(all(draws$share_median == 1))
  testthat::expect_true(all(draws$share_lower == 1))
  testthat::expect_true(all(draws$share_upper == 1))

  zero <- build_vaccination_history(
    kano_cohorts, dplyr::mutate(kano_coverage, coverage = 0),
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  zero_immunity <- estimate_immunity(zero, kano_cohorts)
  testthat::expect_true(all(zero_immunity$immunity$dose_0 == 1))
  testthat::expect_true(all(zero_immunity$contributions$reached == 0))
})

# uncertainty ------------------------------------------------------------------

testthat::test_that("draws do not depend on batching or disturb the RNG", {
  coverage <- dplyr::mutate(
    kano_coverage, coverage_lower = 0.5, coverage_upper = 0.7
  )
  history <- build_vaccination_history(
    kano_cohorts, coverage,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  small <- immunity_draws(
    history, kano_cohorts, n_draws = 7, batch_size = 3, at = "2025-01"
  )
  large <- immunity_draws(
    history, kano_cohorts, n_draws = 7, batch_size = 20, at = "2025-01"
  )
  testthat::expect_identical(small, large)

  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  immunity_draws(history, kano_cohorts, n_draws = 2, at = "2025-01")
  testthat::expect_identical(stats::runif(1), before)
})

testthat::test_that(".ar1 handles gaps and empty input", {
  testthat::expect_identical(.ar1(numeric(), 0.9), numeric())
  testthat::expect_identical(.ar1(2, 0.9), 2)
  # two years apart: correlation rho^2 and unit variance innovation
  testthat::expect_equal(
    .ar1(c(1, 0), 0.5, gaps = c(NA, 2)), c(1, 0.25)
  )
  testthat::expect_equal(
    .ar1(c(0, 1), 0.5, gaps = c(NA, 2)), c(0, sqrt(1 - 0.5^4))
  )
})

testthat::test_that("a shared campaign record is drawn once", {
  pop <- dplyr::tibble(
    adm1 = c("Kano", "Lagos"), year = 2025, age = 0, pop = 1000
  )
  cohorts <- reconstruct_birth_cohorts(
    pop, geography = "adm1", method = "uniform", end = "2025-06"
  )
  coverage <- dplyr::tibble(
    adm1 = c("Kano", "Lagos"), year = 2024, dose = "mcv1", coverage = 0.6
  )
  national <- dplyr::tibble(
    adm0 = "NGA", campaign_id = "national",
    start_date = as.Date("2025-03-01"), end_date = as.Date("2025-03-10"),
    age_min_months = 6, age_max_months = 60, coverage = 0.8,
    coverage_lower = 0.7, coverage_upper = 0.9
  )
  cohorts$cohorts <- dplyr::mutate(cohorts$cohorts, adm0 = "NGA")
  history <- build_vaccination_history(
    cohorts, dplyr::mutate(coverage, adm0 = "NGA"), national,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9),
    geography = c("adm0", "adm1")
  )
  set.seed(1)
  drawn <- .draw_coverage(
    history$history, c("adm0", "adm1"), rho_years = 0.9, rho_doses = 0.5
  )
  campaign_draws <- drawn |>
    dplyr::filter(.data$type == "sia") |>
    dplyr::distinct(.data$coverage)
  testthat::expect_identical(nrow(campaign_draws), 1L)
  testthat::expect_false(campaign_draws$coverage == 0.8)
  # routine coverage without an interval is never moved
  testthat::expect_true(all(
    dplyr::filter(drawn, .data$type == "mcv1")$coverage == 0.6
  ))
})

# survey comparisons -----------------------------------------------------------

testthat::test_that("check_immunity is campaign-specific, one denominator", {
  calendar <- dplyr::tibble(
    adm1 = "Kano", campaign_id = c("first", "second"),
    start_date = as.Date(c("2024-03-01", "2024-09-01")),
    end_date = as.Date(c("2024-03-31", "2024-09-30")),
    target_age = "9-59 months"
  )
  evaluation <- tidyr::expand_grid(
    adm1 = "Kano", campaign_id = c("first", "second"), survey = "s",
    indicator = c(
      "sia_coverage", "mcv_0_dose", "mcv_before_sia", "sia_first_dose"
    )
  ) |>
    dplyr::mutate(
      value_pct = dplyr::if_else(
        .data$indicator == "sia_coverage",
        dplyr::if_else(.data$campaign_id == "first", 50, 40), 10
      )
    )
  campaigns <- prepare_campaigns(calendar, evaluation, geography = "adm1")
  history <- build_vaccination_history(
    kano_cohorts, dplyr::mutate(kano_coverage, coverage = 0), campaigns,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  immunity <- estimate_immunity(history, kano_cohorts)
  checks <- check_immunity(immunity, campaigns)
  model <- checks$comparison |>
    dplyr::select("campaign_id", "indicator", "model_value") |>
    tidyr::pivot_wider(names_from = "indicator", values_from = "model_value")
  first <- dplyr::filter(model, .data$campaign_id == "first")
  second <- dplyr::filter(model, .data$campaign_id == "second")

  # every target cohort of the first campaign is fully eligible and had no
  # dose before, so its first doses are its coverage
  testthat::expect_equal(first$mcv_before_sia, 0)
  testthat::expect_equal(first$sia_first_dose, 0.5)
  testthat::expect_equal(first$mcv_0_dose, 0.5)

  # the second .campaign (dependent coupling, 0.4 < 0.5) gives no first dose
  # to cohorts the first campaign reached; cohorts born July to December
  # 2023 were too young for the first campaign and get 0.4
  population <- immunity$immunity |>
    dplyr::filter(.data$month == as.Date("2024-10-01")) |>
    dplyr::select("birth_month", "population")
  targets <- immunity$campaign_targets |>
    dplyr::filter(.data$event_id == "second") |>
    dplyr::inner_join(population, by = "birth_month") |>
    dplyr::mutate(
      new_only = .data$birth_month >= as.Date("2023-07-01"),
      weight = .data$population * .data$eligible_share
    )
  testthat::expect_equal(targets$eligible_share, rep(1, nrow(targets)))
  testthat::expect_equal(
    second$sia_first_dose,
    sum(0.4 * targets$weight * targets$new_only) / sum(targets$weight)
  )
  testthat::expect_equal(
    second$mcv_before_sia,
    sum(0.5 * targets$weight * !targets$new_only) / sum(targets$weight)
  )
  testthat::expect_equal(
    dplyr::filter(checks$comparison, .data$campaign_id == "second") |>
      dplyr::distinct(.data$denominator) |>
      dplyr::pull("denominator"),
    sum(targets$weight)
  )
  testthat::expect_identical(nrow(checks$diagnostics), 0L)
})

testthat::test_that("check_immunity reports campaigns it cannot compare", {
  calendar <- dplyr::tibble(
    adm1 = "Kano", campaign_id = c("late", "old"),
    start_date = as.Date(c("2025-06-01", "2010-01-01")),
    end_date = as.Date(c("2025-06-10", "2010-01-10")),
    target_age = "9-59 months"
  )
  evaluation <- dplyr::tibble(
    adm1 = "Kano", campaign_id = c("late", "old"), survey = "s",
    indicator = "mcv_0_dose", value_pct = 10
  )
  campaigns <- prepare_campaigns(calendar, evaluation, geography = "adm1")
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, campaigns,
    schedule = dplyr::tibble(dose = "mcv1", age_months = 9), geography = "adm1"
  )
  testthat::expect_true(
    "campaign_skipped" %in% history$diagnostics$check ||
      !"old" %in% history$history$event_id
  )
  immunity <- estimate_immunity(history, kano_cohorts)
  testthat::expect_error(check_immunity(immunity, campaigns), "compared")
})

testthat::test_that("summaries with no population have no defined share", {
  immunity <- estimate_immunity(
    build_vaccination_history(
      kano_cohorts, kano_coverage,
      schedule = dplyr::tibble(dose = "mcv1", age_months = 9),
      geography = "adm1"
    ),
    kano_cohorts
  )
  empty <- immunity$immunity |>
    dplyr::mutate(population = 0, susceptible = 0)
  summary <- summarise_immunity(empty, at = "2025-01")
  testthat::expect_true(all(summary$population == 0))
  testthat::expect_true(all(is.na(summary$susceptible_share)))
  testthat::expect_true(all(is.na(summary$dose_0)))
})
