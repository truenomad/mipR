# fixtures ---------------------------------------------------------------------

# one January 2023 cohort followed month by month
.conv_panel <- function(months) {
  dplyr::tibble(
    adm1 = "A",
    birth_month = as.Date("2023-01-01"),
    month = as.Date(paste0(months, "-01")),
    age_months = .month_index(.data$month) -
      .month_index(.data$birth_month),
    population = 100
  )
}

.conv_routine <- function(coverage = 0.6) {
  dplyr::tibble(
    adm1 = "A", year = 2023:2024, dose = "mcv1", coverage = coverage
  )
}

.conv_campaign <- function(id, date, coverage = 1, age_min = 6, age_max = 60,
                           coupling = NA_character_) {
  dplyr::tibble(
    adm1 = "A", campaign_id = id,
    start_date = as.Date(date), end_date = as.Date(date),
    age_min_months = age_min, age_max_months = age_max, coverage = coverage,
    coupling = coupling
  )
}

.conv_value <- function(immunity, month, column = "susceptible_share") {
  immunity |>
    dplyr::filter(.data$month == as.Date(paste0(!!month, "-01"))) |>
    dplyr::pull(dplyr::all_of(column))
}

.schedule_9 <- dplyr::tibble(dose = "mcv1", age_months = 9)
.fixed_15 <- mip_conventions(birth = "fixed_day", birth_day = 15)

# constructor ------------------------------------------------------------------

testthat::test_that("mip_conventions defaults are the existing rules", {
  conv <- mip_conventions()
  testthat::expect_s3_class(conv, "mip_conventions")
  testthat::expect_identical(conv$birth, "uniform")
  testthat::expect_identical(conv$age, "calendar")
  testthat::expect_identical(conv$dose_timing, "calendar_months")
  testthat::expect_identical(conv$report_day, 15L)
  testthat::expect_false(conv$report_before_events)
  testthat::expect_identical(conv$same_date_order, "event_id")
  testthat::expect_identical(conv$days_per_month, 365.25 / 12)
  testthat::expect_identical(mip_conventions_preset("default"), conv)
})

testthat::test_that("the v2_pipeline preset holds the pipeline rules", {
  conv <- mip_conventions_preset("v2_pipeline")
  testthat::expect_identical(conv$birth, "fixed_day")
  testthat::expect_identical(conv$birth_day, 15L)
  testthat::expect_identical(conv$age, "days")
  testthat::expect_identical(conv$dose_timing, "days")
  testthat::expect_identical(conv$report_day, 1L)
  testthat::expect_true(conv$report_before_events)
  testthat::expect_identical(conv$same_date_order, "input")
})

testthat::test_that("mip_conventions rejects invalid values", {
  testthat::expect_error(mip_conventions(birth_day = 29), "between 1 and 28")
  testthat::expect_error(mip_conventions(report_day = 0), "report_day")
  testthat::expect_error(mip_conventions(days_per_month = 40))
  testthat::expect_error(
    mip_conventions(report_before_events = NA), "TRUE or FALSE"
  )
  testthat::expect_error(mip_conventions(age = "years"))
  testthat::expect_error(
    build_vaccination_history(
      .conv_panel("2023-10"), .conv_routine(), schedule = .schedule_9,
      geography = "adm1", conventions = list(birth = "uniform")
    ),
    "mip_conventions"
  )
})

# defaults ---------------------------------------------------------------------

testthat::test_that("explicit default conventions change nothing", {
  panel <- .conv_panel(c("2023-09", "2023-10", "2023-11", "2024-01"))
  campaign <- .conv_campaign("c", "2023-10-20", coverage = 0.5)
  implicit <- build_vaccination_history(
    panel, .conv_routine(), dplyr::select(campaign, -"coupling"),
    .schedule_9, "adm1"
  )
  explicit <- build_vaccination_history(
    panel, .conv_routine(), dplyr::select(campaign, -"coupling"),
    .schedule_9, "adm1", conventions = mip_conventions()
  )
  testthat::expect_identical(implicit, explicit)
  testthat::expect_false("input_order" %in% names(implicit$history))
  testthat::expect_false("coupling" %in% names(implicit$history))
  testthat::expect_identical(
    estimate_immunity(implicit, panel),
    estimate_immunity(implicit, panel, conventions = mip_conventions())
  )
})

# births -----------------------------------------------------------------------

testthat::test_that("fixed_day births give one birth date per cohort", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  history <- build_vaccination_history(
    panel, .conv_routine(), schedule = .schedule_9, geography = "adm1",
    conventions = .fixed_15
  )
  testthat::expect_equal(nrow(history$history), 1)
  testthat::expect_equal(history$history$day_from, 15L)
  testthat::expect_equal(history$history$eligible_share, 1)
  testthat::expect_equal(history$history$date_from, as.Date("2023-10-15"))

  # every child gets the dose on 15 October, before the 15 October report
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(.conv_value(result, "2023-10"), 1 - 0.6 * 0.84)
})

testthat::test_that("cohort_eligibility follows the birth and age rules", {
  days_1 <- mip_conventions(birth = "fixed_day", birth_day = 1, age = "days")
  calendar_1 <- mip_conventions(birth = "fixed_day", birth_day = 1)
  # born 1 January, 273 days old on 1 October: 9 calendar months, 8.97 by days
  testthat::expect_equal(
    cohort_eligibility(
      "2023-01", "2023-10-01", age_min_months = 9, conventions = calendar_1
    ),
    1
  )
  testthat::expect_equal(
    cohort_eligibility(
      "2023-01", "2023-10-01", age_min_months = 9, conventions = days_1
    ),
    0
  )
  testthat::expect_identical(
    cohort_eligibility(
      "2023-01", "2023-10-15", age_min_months = 9, convention = "midmonth"
    ),
    cohort_eligibility(
      "2023-01", "2023-10-15", age_min_months = 9, conventions = .fixed_15
    )
  )
})

# age --------------------------------------------------------------------------

testthat::test_that("age in days sets campaign eligibility", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  days <- mip_conventions(birth = "fixed_day", age = "days")
  # born 15 January: 9 calendar months on 15 October, 273 days = 8.97 months
  campaign <- .conv_campaign("c", "2023-10-15", age_min = 9)
  calendar_history <- build_vaccination_history(
    panel, .conv_routine(0), campaign, .schedule_9, "adm1",
    conventions = .fixed_15
  )
  days_history <- build_vaccination_history(
    panel, .conv_routine(0), campaign, .schedule_9, "adm1",
    conventions = days
  )
  testthat::expect_equal(sum(calendar_history$history$type == "sia"), 1)
  testthat::expect_equal(sum(days_history$history$type == "sia"), 0)

  # one day later, 274 days = 9.002 months, the child is eligible
  later <- build_vaccination_history(
    panel, .conv_routine(0), .conv_campaign("c", "2023-10-16", age_min = 9),
    .schedule_9, "adm1", conventions = days
  )
  testthat::expect_equal(sum(later$history$type == "sia"), 1)
})

testthat::test_that("age in days sets the effectiveness band", {
  panel <- .conv_panel(c("2024-01", "2024-02"))
  campaign <- .conv_campaign("c", "2024-01-15")
  # 12 calendar months on 15 January 2024, but 365 days = 11.99 months
  calendar <- build_vaccination_history(
    panel, .conv_routine(0), campaign, .schedule_9, "adm1",
    conventions = .fixed_15
  ) |>
    estimate_immunity(panel)
  days <- build_vaccination_history(
    panel, .conv_routine(0), campaign, .schedule_9, "adm1",
    conventions = mip_conventions(birth = "fixed_day", age = "days")
  ) |>
    estimate_immunity(panel)
  testthat::expect_equal(.conv_value(calendar$immunity, "2024-01"), 0.05)
  testthat::expect_equal(.conv_value(days$immunity, "2024-01"), 0.16)
})

# routine dose dates -----------------------------------------------------------

testthat::test_that("dose_timing days dates doses by rounded days", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  history <- build_vaccination_history(
    panel, .conv_routine(), schedule = .schedule_9, geography = "adm1",
    conventions = mip_conventions(birth = "fixed_day", dose_timing = "days")
  )
  # round(9 * 365.25 / 12) = 274 days after 15 January
  testthat::expect_equal(history$history$date_from, as.Date("2023-10-16"))
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(.conv_value(result, "2023-10"), 1)
  testthat::expect_equal(.conv_value(result, "2023-11"), 1 - 0.6 * 0.84)
})

# reporting date ---------------------------------------------------------------

testthat::test_that("the reporting day and its order with events apply", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  # the dose is on 15 October for every child
  run <- function(...) {
    conv <- mip_conventions(birth = "fixed_day", ...)
    build_vaccination_history(
      panel, .conv_routine(), schedule = .schedule_9, geography = "adm1",
      conventions = conv
    ) |>
      estimate_immunity(panel) |>
      purrr::pluck("immunity")
  }
  after <- 1 - 0.6 * 0.84
  testthat::expect_equal(.conv_value(run(), "2023-10"), after)
  testthat::expect_equal(
    .conv_value(run(report_before_events = TRUE), "2023-10"), 1
  )
  testthat::expect_equal(
    .conv_value(run(report_before_events = TRUE), "2023-11"), after
  )
  testthat::expect_equal(
    .conv_value(run(report_day = 1, report_before_events = TRUE), "2023-10"),
    1
  )
  testthat::expect_equal(.conv_value(run(report_day = 14), "2023-10"), 1)
  testthat::expect_equal(.conv_value(run(report_day = 20), "2023-10"), after)
})

# same-date order --------------------------------------------------------------

testthat::test_that("same-date campaigns follow event_id or input order", {
  panel <- .conv_panel(c("2023-11", "2023-12"))
  # "b" comes first in the input; "a" sorts first by event_id
  campaigns <- dplyr::bind_rows(
    .conv_campaign("b", "2023-11-07", 0.5, coupling = "independent"),
    .conv_campaign("a", "2023-11-07", 0.8, coupling = "complementary")
  )
  run <- function(order) {
    conv <- mip_conventions(birth = "fixed_day", same_date_order = order)
    build_vaccination_history(
      panel, .conv_routine(0), campaigns, .schedule_9, "adm1",
      conventions = conv
    ) |>
      estimate_immunity(panel) |>
      purrr::pluck("immunity")
  }
  # b then a: 0.5 reached, then a reaches the 0.5 unvaccinated and 0.3 more
  testthat::expect_equal(.conv_value(run("input"), "2023-11", "dose_2"), 0.3)
  # a then b: 0.8 reached, then b reaches 0.5 of everyone
  testthat::expect_equal(
    .conv_value(run("event_id"), "2023-11", "dose_2"), 0.4
  )
})

# conventions stored in the history --------------------------------------------

testthat::test_that("estimates use the history's conventions", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  preset <- mip_conventions_preset("v2_pipeline")
  history <- build_vaccination_history(
    panel, .conv_routine(), schedule = .schedule_9, geography = "adm1",
    conventions = preset
  )
  testthat::expect_identical(history$metadata$conventions, preset)
  estimate <- estimate_immunity(history, panel)
  testthat::expect_identical(estimate$metadata$conventions, preset)
  testthat::expect_error(
    estimate_immunity(history, panel, conventions = mip_conventions()),
    "differ"
  )

  history$history$input_order <- NULL
  testthat::expect_error(estimate_immunity(history, panel), "input_order")
})

testthat::test_that("immunity_draws runs with the history's conventions", {
  panel <- .conv_panel(c("2023-10", "2023-11"))
  history <- build_vaccination_history(
    panel, .conv_routine(), schedule = .schedule_9, geography = "adm1",
    conventions = mip_conventions(birth = "fixed_day", dose_timing = "days")
  )
  draws <- immunity_draws(
    history, panel, n_draws = 2, workers = 1, at = "2023-10",
    geography = "adm1"
  )
  # coverage has no interval, so every draw is the point estimate
  testthat::expect_equal(sum(draws$share_median * draws$population) /
                           sum(draws$population), 1)
})

# campaign records -------------------------------------------------------------

testthat::test_that("coupling is carried from prepare_campaigns", {
  calendar <- dplyr::tibble(
    adm1 = "A", campaign_id = c("b", "a"),
    start_date = as.Date("2023-11-07"), end_date = as.Date("2023-11-07"),
    target_age = "6-59 months",
    coupling = c("independent", "complementary")
  )
  reported <- dplyr::tibble(
    adm1 = "A", campaign_id = c("b", "a"), coverage_pct = c(50, 80)
  )
  prepared <- prepare_campaigns(
    calendar, reported_coverage = reported, geography = "adm1"
  )
  testthat::expect_identical(
    prepared$campaigns$coupling, c("complementary", "independent")
  )
  without <- prepare_campaigns(
    dplyr::select(calendar, -"coupling"), reported_coverage = reported,
    geography = "adm1"
  )
  testthat::expect_false("coupling" %in% names(without$campaigns))

  panel <- .conv_panel(c("2023-11", "2023-12"))
  history <- build_vaccination_history(
    panel, .conv_routine(0), prepared, .schedule_9, "adm1",
    conventions = .fixed_15
  )
  testthat::expect_identical(
    sort(unique(history$history$coupling)), c("complementary", "independent")
  )
  # a (complementary, 0.8) then b (independent, 0.5), by event_id
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(.conv_value(result, "2023-11", "dose_2"), 0.4)

  calendar$coupling[1] <- "sometimes"
  testthat::expect_error(
    prepare_campaigns(
      calendar, reported_coverage = reported, geography = "adm1"
    ),
    "coupling"
  )
})

testthat::test_that("repeated campaign records can be applied separately", {
  panel <- .conv_panel(c("2023-11", "2023-12"))
  rounds <- dplyr::bind_rows(
    .conv_campaign("ori", "2023-11-07", 0.3),
    .conv_campaign("ori", "2023-11-07", 0.25)
  )
  testthat::expect_error(
    build_vaccination_history(
      panel, .conv_routine(0), rounds, .schedule_9, "adm1"
    ),
    "more than one record"
  )
  history <- build_vaccination_history(
    panel, .conv_routine(0), rounds, .schedule_9, "adm1",
    conventions = .fixed_15, repeated_campaigns = "separate"
  )
  testthat::expect_identical(
    sort(unique(history$history$event_id[history$history$type == "sia"])),
    c("ori", "ori#2")
  )
  # dependent coupling: the second round reaches the first round's children
  result <- estimate_immunity(history, panel)$immunity
  testthat::expect_equal(.conv_value(result, "2023-11", "dose_2"), 0.25)
  testthat::expect_equal(.conv_value(result, "2023-11", "dose_1"), 0.05)

  clash <- dplyr::bind_rows(rounds, .conv_campaign("ori#2", "2023-12-01"))
  testthat::expect_error(
    build_vaccination_history(
      panel, .conv_routine(0), clash, .schedule_9, "adm1",
      repeated_campaigns = "separate"
    ),
    "already in use"
  )
})

# equivalence with the pipeline engine -----------------------------------------

testthat::test_that("v2_pipeline conventions reproduce the pipeline engine", {
  fixture <- readRDS(testthat::test_path("fixtures", "v2_reference.rds"))
  units <- unique(fixture$cohorts$unit_id)
  panel <- tidyr::expand_grid(fixture$cohorts, month = fixture$months) |>
    dplyr::mutate(
      age_months = .month_index(.data$month) -
        .month_index(.data$birth_month),
      population = .data$size
    ) |>
    dplyr::select(-"size")
  routine <- dplyr::mutate(fixture$routine, dose = tolower(.data$dose))
  schedule <- fixture$schedule |>
    dplyr::transmute(
      dose = tolower(.data$dose), age_months = .data$scheduled_age_months,
      .data$start_date, .data$end_date
    )
  # national rows (no unit) apply to every unit, in their input position
  campaigns <- fixture$campaigns |>
    dplyr::mutate(
      unit_id = purrr::map(.data$unit_id, \(u) if (is.na(u)) units else u)
    ) |>
    tidyr::unnest("unit_id") |>
    dplyr::transmute(
      .data$unit_id, campaign_id = .data$event_id,
      start_date = .data$date, end_date = .data$date,
      age_min_months = .data$age_lo, age_max_months = .data$age_hi,
      .data$coverage, coupling = as.character(.data$coupling)
    )

  run <- function(conventions) {
    build_vaccination_history(
      panel, routine, campaigns, schedule, "unit_id",
      conventions = conventions, repeated_campaigns = "separate"
    ) |>
      estimate_immunity(panel) |>
      purrr::pluck("immunity") |>
      dplyr::inner_join(
        dplyr::rename(fixture$reference, v2_susceptible = "susceptible"),
        by = c("unit_id", "birth_month", "month")
      )
  }
  matched <- run(mip_conventions_preset("v2_pipeline"))
  testthat::expect_equal(nrow(matched), nrow(fixture$reference))
  testthat::expect_lt(
    max(abs(matched$susceptible_share - matched$v2_susceptible)), 1e-10
  )

  # the default conventions do not reproduce it, so the test can fail
  default <- run(mip_conventions(same_date_order = "input"))
  testthat::expect_gt(
    max(abs(default$susceptible_share - default$v2_susceptible)), 0.01
  )
})
