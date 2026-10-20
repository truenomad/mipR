# reference cases from the CDC method checks -----------------------------------

# one cohort through a list of opportunities, returning the final state
one_cohort <- function(type, age, coverage, coupling = "dependent") {
  cells <- dplyr::tibble(
    cell = 1L, adm1 = "A", birth_month = as.Date("2000-01-01")
  )
  events <- dplyr::tibble(
    cell = 1L,
    event_id = paste0("e", seq_along(type)),
    type = type,
    month = as.Date("2000-01-01") + 31 * seq_along(type),
    eligible_share = 1,
    timing_share = 1,
    age_at_dose = age,
    coverage = coverage
  )
  run <- .run_engine(cells, events, coupling, ve_cdc(), 8L)
  dplyr::slice_tail(run$steps, n = 1)
}

f12 <- 1 - 0.95

testthat::test_that("the engine reproduces the hand-calculated cases", {
  cases <- dplyr::tribble(
    ~name, ~type, ~age, ~coverage, ~coupling, ~expected,
    "complete MCV1", "mcv1", 9, 1, "dependent", 0.16,
    "partial MCV1", "mcv1", 9, 0.8, "dependent", 1 - 0.8 * 0.84,
    "MCV1 then MCV2", c("mcv1", "mcv2"), c(9, 18), c(0.8, 0.6),
    "dependent", 0.2 + 0.8 * 0.16 - 0.6 * 0.16 * 0.95,
    "MCV2 above MCV1", c("mcv1", "mcv2"), c(9, 18), c(0.6, 0.8),
    "dependent", 1 - (0.6 * 0.84 + 0.6 * 0.16 * 0.95 + 0.2 * 0.95),
    "two campaigns, dependent", c("sia", "sia"), c(24, 36), c(0.7, 0.8),
    "dependent", 0.2 + 0.1 * f12 + 0.7 * f12 * f12,
    "two campaigns, complementary", c("sia", "sia"), c(24, 36),
    c(0.7, 0.8), "complementary", 0.3 * f12 + 0.5 * f12 * f12 + 0.2 * f12,
    "same children twice", c("sia", "sia"), c(24, 36), c(0.5, 0.5),
    "dependent", 0.5 + 0.5 * f12 * f12,
    "independent campaigns", c("sia", "sia"), c(24, 36), c(0.5, 0.5),
    "independent", 0.25 + 2 * 0.25 * f12 + 0.25 * f12 * f12,
    "campaign at 6 months", "sia", 6, 1, "dependent", 0.42,
    "campaign at 10 months", "sia", 10, 1, "dependent", 0.16,
    "campaign at 13 months", "sia", 13, 1, "dependent", f12,
    "failure then revaccination", c("mcv1", "sia"), c(9, 24), c(1, 1),
    "dependent", 0.16 * f12
  )

  results <- cases |>
    dplyr::mutate(
      engine = purrr::pmap_dbl(
        list(.data$type, .data$age, .data$coverage, .data$coupling),
        \(type, age, coverage, coupling) {
          one_cohort(type, age, coverage, coupling)$susceptible_share
        }
      )
    )

  testthat::expect_equal(results$engine, results$expected, tolerance = 1e-9)
})

testthat::test_that("the engine matches the CDC Figure 1 formulas", {
  # Figure 1 for one cohort: MCV1, MCV2, then campaigns in order
  figure1 <- function(mcv1, mcv2, sia_coverage, sia_age) {
    ve <- ve_cdc()
    vaccinated <- mcv1
    immune <- mcv1 * 0.84
    if (mcv2 < mcv1) {
      immune <- immune + (mcv1 - mcv1 * 0.84) * (mcv2 / mcv1) * 0.95
    } else {
      immune <- immune + (mcv1 - mcv1 * 0.84) * 0.95 + (mcv2 - mcv1) * 0.95
      vaccinated <- mcv2
    }
    for (k in seq_along(sia_coverage)) {
      coverage <- sia_coverage[k]
      effect <- if (sia_age[k] >= 12) 0.95 else 0.84
      if (coverage < vaccinated) {
        immune <- immune +
          (vaccinated - immune) * (coverage / vaccinated) * effect
      } else {
        immune <- immune + (coverage - immune) * effect
        vaccinated <- coverage
      }
    }
    1 - immune
  }

  set.seed(1)
  sequences <- purrr::map(seq_len(50), \(i) {
    k <- sample(0:3, 1)
    list(
      mcv1 = stats::runif(1, 0.05, 1), mcv2 = stats::runif(1),
      sia_coverage = stats::runif(k),
      sia_age = sort(stats::runif(k, 19, 120))
    )
  })

  gaps <- purrr::map_dbl(sequences, \(x) {
    engine <- one_cohort(
      c("mcv1", "mcv2", rep("sia", length(x$sia_coverage))),
      c(9, 18, x$sia_age),
      c(x$mcv1, x$mcv2, x$sia_coverage)
    )$susceptible_share
    abs(engine - figure1(x$mcv1, x$mcv2, x$sia_coverage, x$sia_age))
  })

  testthat::expect_lt(max(gaps), 1e-9)
})

testthat::test_that("shares stay whole and campaigns never add susceptibles", {
  set.seed(2)
  type <- c("mcv1", "mcv2", "sia", "sia", "sia")
  age <- c(9, 15, 20, 40, 60)
  coverage <- stats::runif(5)

  cells <- dplyr::tibble(
    cell = 1L, adm1 = "A", birth_month = as.Date("2000-01-01")
  )
  events <- dplyr::tibble(
    cell = 1L, event_id = paste0("e", 1:5), type = type,
    month = as.Date("2000-01-01") + 31 * 1:5, eligible_share = 1,
    timing_share = 1, age_at_dose = age, coverage = coverage
  )
  steps <- .run_engine(cells, events, "dependent", ve_cdc(), 8L)$steps

  doses <- steps$dose_0 + steps$dose_1 + steps$dose_2 + steps$dose_3_plus
  testthat::expect_equal(doses, rep(1, nrow(steps)), tolerance = 1e-9)
  testthat::expect_true(all(diff(steps$susceptible_share) <= 1e-12))
})

testthat::test_that("complementary leaves the fewest susceptible", {
  susceptible <- purrr::map_dbl(
    c("complementary", "independent", "dependent"),
    \(rule) {
      one_cohort(c("sia", "sia"), c(24, 36), c(0.7, 0.8), rule)$
        susceptible_share
    }
  )
  testthat::expect_true(susceptible[1] < susceptible[2])
  testthat::expect_true(susceptible[2] < susceptible[3])
})

# history and monthly values ---------------------------------------------------

kano_pop <- data.frame(
  adm1 = "Kano", year = c(2024, 2024, 2025, 2025), age = c(0, 1, 0, 1),
  pop = c(1200, 1150, 1240, 1180)
)
kano_cohorts <- reconstruct_birth_cohorts(
  kano_pop, geography = "adm1", method = "uniform",
  start = "2023-01", end = "2025-06"
)
kano_coverage <- data.frame(
  adm1 = "Kano", year = 2023:2025, dose = "mcv1",
  coverage = c(0.6, 0.65, 0.7), coverage_lower = c(0.5, 0.55, 0.6),
  coverage_upper = c(0.7, 0.75, 0.8)
)
kano_schedule <- data.frame(dose = "mcv1", age_months = 9)

testthat::test_that("routine doses fall in the month the age is reached", {
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = kano_schedule, geography = "adm1"
  )
  jan23 <- history$history |>
    dplyr::filter(.data$birth_month == as.Date("2023-01-01"))

  testthat::expect_identical(jan23$month, as.Date("2023-10-01"))
  testthat::expect_equal(jan23$timing_share, 15 / 31)
  testthat::expect_equal(jan23$coverage, 0.6)
  testthat::expect_equal(jan23$coverage_lower, 0.5)
  testthat::expect_identical(jan23$eligible_share, 1)
})

testthat::test_that("a schedule start part-way through a cohort", {
  schedule <- data.frame(
    dose = "mcv1", age_months = 9, start_date = as.Date("2023-10-11")
  )
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = schedule, geography = "adm1"
  )
  jan23 <- history$history |>
    dplyr::filter(.data$birth_month == as.Date("2023-01-01"))

  # born 11-31 January reach 9 months from 11 October
  testthat::expect_equal(jan23$eligible_share, 21 / 31)
})

testthat::test_that("monthly values mix before and after by timing", {
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = kano_schedule, geography = "adm1"
  )
  immunity <- estimate_immunity(history, kano_cohorts)$immunity
  jan23 <- immunity |>
    dplyr::filter(.data$birth_month == as.Date("2023-01-01"))
  value_in <- function(month) {
    jan23$susceptible_share[jan23$month == as.Date(month)]
  }

  after <- 1 - 0.6 * 0.84
  testthat::expect_equal(value_in("2023-09-01"), 1)
  testthat::expect_equal(value_in("2023-10-01"), 1 - (15 / 31) * (1 - after))
  testthat::expect_equal(value_in("2023-11-01"), after)
  testthat::expect_true(all(
    jan23$susceptible == jan23$susceptible_share * jan23$population
  ))
  testthat::expect_identical(
    jan23$maternal_protection, jan23$age_months < 6
  )
})

testthat::test_that("campaigns reach only the eligible share", {
  campaigns <- data.frame(
    adm1 = "Kano", campaign_id = "NGA-MSL-2024",
    start_date = as.Date("2024-03-01"), end_date = as.Date("2024-03-10"),
    age_min_months = 9, age_max_months = 60, coverage = 0.8
  )
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, campaigns,
    schedule = kano_schedule, geography = "adm1"
  )
  sia <- history$history |>
    dplyr::filter(.data$type == "sia")

  # born June 2023: 9 months by 10 March 2024 only if born by 10 June
  june23 <- sia |>
    dplyr::filter(.data$birth_month == as.Date("2023-06-01"))
  testthat::expect_equal(june23$eligible_share, 10 / 30)
  testthat::expect_false(as.Date("2023-07-01") %in% sia$birth_month)

  contributions <- estimate_immunity(history, kano_cohorts)$contributions
  testthat::expect_true(all(contributions$newly_protected >= -1e-12))
})

testthat::test_that("campaign records without dates are skipped and listed", {
  campaigns <- data.frame(
    adm1 = "Kano", campaign_id = "NGA-MSL-2017-2018",
    start_date = as.Date(NA), end_date = as.Date(NA),
    age_min_months = 9, age_max_months = 60, coverage = 0.85
  )
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, campaigns,
    schedule = kano_schedule, geography = "adm1"
  )

  testthat::expect_false("sia" %in% history$history$type)
  testthat::expect_true("campaign_skipped" %in% history$diagnostics$check)
})

testthat::test_that("ve_cdc checks its values", {
  testthat::expect_identical(ve_cdc()$mcv1, c(0.58, 0.84, 0.925))
  testthat::expect_error(ve_cdc(sia = 1.2), "between 0 and 1")
})

# summaries, checks and draws --------------------------------------------------

testthat::test_that("summarise_immunity weights shares by population", {
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = kano_schedule, geography = "adm1"
  )
  immunity <- estimate_immunity(history, kano_cohorts)
  summary <- summarise_immunity(immunity, at = "2025-01")

  testthat::expect_identical(
    as.character(summary$age_band), c("0-11 months", "1-4 years")
  )
  rows <- immunity$immunity |>
    dplyr::filter(
      .data$month == as.Date("2025-01-01"), !.data$maternal_protection,
      .data$age_months < 12
    )
  testthat::expect_equal(
    summary$susceptible_share[1],
    sum(rows$susceptible) / sum(rows$population)
  )
  testthat::expect_equal(
    summary$susceptible_share + summary$immune_share, c(1, 1)
  )
})

testthat::test_that("check_immunity compares model and survey doses", {
  calendar <- data.frame(
    adm1 = "Kano", campaign_id = "NGA-MSL-2024",
    start_date = as.Date("2024-03-01"), end_date = as.Date("2024-03-10"),
    target_age = "9-59 months"
  )
  evaluation <- data.frame(
    adm1 = "Kano", campaign_id = "NGA-MSL-2024", survey = "IPCCS",
    indicator = c("sia_coverage", "mcv_0_dose", "sia_first_dose"),
    value_pct = c(80, 10, 20), lower_pct = c(75, 7, 15),
    upper_pct = c(85, 14, 26)
  )
  campaigns <- prepare_campaigns(calendar, evaluation, geography = "adm1")
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, campaigns,
    schedule = kano_schedule, geography = "adm1"
  )
  checks <- check_immunity(estimate_immunity(history, kano_cohorts), campaigns)

  testthat::expect_setequal(
    checks$comparison$indicator, c("mcv_0_dose", "sia_first_dose")
  )
  testthat::expect_true(all(
    checks$comparison$model_value >= 0 & checks$comparison$model_value <= 1
  ))
  testthat::expect_named(
    checks$summary,
    c(
      "campaign_id", "indicator", "areas", "mean_difference",
      "mean_absolute_difference", "share_within_interval"
    )
  )
})

testthat::test_that("immunity_draws gives intervals around the estimate", {
  history <- build_vaccination_history(
    kano_cohorts, kano_coverage, schedule = kano_schedule, geography = "adm1"
  )
  draws <- immunity_draws(
    history, kano_cohorts, n_draws = 20, seed = 3, at = "2025-01"
  )
  point <- summarise_immunity(
    estimate_immunity(history, kano_cohorts), at = "2025-01"
  )

  testthat::expect_true(all(draws$share_lower <= draws$share_median))
  testthat::expect_true(all(draws$share_median <= draws$share_upper))
  testthat::expect_true(all(draws$share_upper > draws$share_lower))
  testthat::expect_equal(
    draws$share_median, point$susceptible_share, tolerance = 0.05
  )

  again <- immunity_draws(
    history, kano_cohorts, n_draws = 20, seed = 3, at = "2025-01"
  )
  testthat::expect_identical(draws, again)
})

testthat::test_that(".ar1 keeps unit variance and its correlation", {
  set.seed(4)
  x <- .ar1(stats::rnorm(20000), 0.9)
  testthat::expect_equal(stats::sd(x), 1, tolerance = 0.05)
  testthat::expect_equal(stats::cor(x[-1], x[-length(x)]), 0.9,
                         tolerance = 0.02)
})
