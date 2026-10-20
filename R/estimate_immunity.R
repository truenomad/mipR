# immunity engine --------------------------------------------------------------

#' Estimate Vaccination-Derived Immunity of Monthly Birth Cohorts
#'
#' Applies each cohort's vaccination opportunities in date order and tracks
#' the share of the cohort in each vaccination state, then reports the
#' susceptible and immune population by geography, birth month and calendar
#' month. Follows the CDC immunity profile method (Li, Goodson and Perry
#' 2024, Figure 1).
#'
#' @details
#' **States.** Each cohort is split into shares by number of doses received
#' (0 to `max_doses`), whether MCV1 was received, and whether the child is
#' protected. An opportunity reaches `coverage x eligible_share` of the
#' cohort. Reached children who are not yet protected become protected with
#' the vaccine effectiveness for the dose type and age at the dose.
#'
#' **Who a dose reaches** (`coupling`):
#' - `"dependent"` (default): children who already had a dose are reached
#'   before children with none, as in the CDC method.
#' - `"independent"`: every child is equally likely to be reached.
#' - `"complementary"`: children with fewer doses are reached first.
#'
#' MCV2 always reaches MCV1 recipients first. A `coupling` column in the
#' history overrides the default for an opportunity.
#'
#' **Monthly values** refer to the 15th of each month. For an opportunity in
#' that month, the value mixes the states before and after it by the share
#' of the cohort's opportunity that has happened by the 15th
#' (`timing_share`).
#'
#' **Maternal antibodies.** Infants younger than `maternal_months` are
#' marked in `maternal_protection` and left out of [summarise_immunity()] by
#' default, rather than modelled.
#'
#' Vaccination is the only source of immunity: infection is not modelled.
#'
#' @param history The output of [build_vaccination_history()].
#' @param cohorts The output of [reconstruct_birth_cohorts()], or its
#'   `cohorts` data frame, giving the population of each cohort by month.
#' @param ve Vaccine effectiveness by dose type and age, as returned by
#'   [ve_cdc()].
#' @param coupling Default rule for who an opportunity reaches:
#'   `"dependent"`, `"independent"` or `"complementary"`.
#' @param maternal_months Age in months below which infants are marked as
#'   protected by maternal antibodies. Default `6`.
#' @param max_doses Most doses tracked; further doses stay in the top group.
#'   Default `8`.
#'
#' @return A list:
#'   \describe{
#'     \item{immunity}{One row per geography, birth month and month: the
#'       cohort columns, `susceptible_share`, `dose_0`, `dose_1`, `dose_2`
#'       and `dose_3_plus` (shares of the cohort), `susceptible` (people)
#'       and `maternal_protection`}
#'     \item{contributions}{One row per cohort and opportunity: `event_id`,
#'       `type`, `month`, `reached` and `newly_protected` (shares of the
#'       cohort)}
#'     \item{metadata}{The settings used}
#'   }
#'
#' @examples
#' pop <- data.frame(
#'   adm1 = "Kano", year = 2024, age = 0:1, pop = c(1200, 1150)
#' )
#' cohorts <- reconstruct_birth_cohorts(
#'   pop, geography = "adm1", method = "uniform", end = "2024-12"
#' )
#' coverage <- data.frame(
#'   adm1 = "Kano", year = 2023:2024, dose = "mcv1", coverage = c(0.6, 0.65)
#' )
#' history <- build_vaccination_history(
#'   cohorts, coverage,
#'   schedule = data.frame(dose = "mcv1", age_months = 9),
#'   geography = "adm1"
#' )
#' immunity <- estimate_immunity(history, cohorts)
#' head(immunity$immunity)
#' @export
estimate_immunity <- function(
  history,
  cohorts,
  ve = ve_cdc(),
  coupling = c("dependent", "independent", "complementary"),
  maternal_months = 6,
  max_doses = 8L
) {
  coupling <- match.arg(coupling)
  .check_history(history)
  cells <- history$cells
  geography <- setdiff(names(cells), c("cell", "birth_month"))
  panel <- .cohort_panel_input(cohorts, geography)

  run <- .run_engine(
    cells, history$history, coupling, ve, max_doses
  )

  immunity <- .monthly_values(panel, cells, run$steps, geography) |>
    dplyr::mutate(
      susceptible = .data$susceptible_share * .data$population,
      maternal_protection = .data$age_months < maternal_months
    )

  contributions <- run$contributions |>
    dplyr::left_join(cells, by = "cell") |>
    dplyr::select(
      dplyr::all_of(geography), "birth_month", "event_id", "type", "month",
      "reached", "newly_protected"
    )

  list(
    immunity = immunity,
    contributions = contributions,
    metadata = list(
      method = "CDC immunity profile (Li, Goodson and Perry 2024, Figure 1)",
      coupling = coupling,
      ve = ve,
      maternal_months = maternal_months,
      max_doses = max_doses,
      reporting_date = "15th of each month",
      immunity_sources = "vaccination only"
    )
  )
}

#' Vaccine Effectiveness from the CDC Immunity Profile Method
#'
#' Vaccine effectiveness by dose type and age at the dose, from Li, Goodson
#' and Perry (2024), Figure 1 and Table 1: the share of reached children,
#' not yet protected, who become protected.
#'
#' @param mcv1,mcv2,sia Effectiveness by age band for each dose type, with
#'   age bands set by `breaks`.
#' @param breaks Lower bounds of the age bands in months.
#'
#' @return A list with `breaks` and the effectiveness for `mcv1`, `mcv2`
#'   and `sia`, for [estimate_immunity()].
#'
#' @examples
#' ve_cdc()
#'
#' # a single effectiveness for every dose and age
#' ve_cdc(mcv1 = 0.9, mcv2 = 0.9, sia = 0.9, breaks = 0)
#' @export
ve_cdc <- function(
  mcv1 = c(0.58, 0.84, 0.925),
  mcv2 = c(0.95, 0.95, 0.95),
  sia = c(0.58, 0.84, 0.95),
  breaks = c(0, 9, 12)
) {
  values <- list(mcv1 = mcv1, mcv2 = mcv2, sia = sia)
  n <- length(breaks)
  values <- purrr::map(values, \(v) rep_len(v, n))
  all_values <- purrr::list_c(values)
  if (any(!is.finite(all_values)) || any(all_values < 0 | all_values > 1)) {
    cli::cli_abort("Vaccine effectiveness must be between 0 and 1.")
  }
  c(list(breaks = breaks), values)
}

#' @noRd
.check_history <- function(history) {
  if (!is.list(history) || !all(c("history", "cells") %in% names(history))) {
    cli::cli_abort(
      "{.arg history} must be the output of {.fn build_vaccination_history}."
    )
  }
  events <- history$history
  if (!is.null(events) && nrow(events) > 0) {
    if (any(!is.finite(events$coverage)) ||
          any(events$coverage < 0 | events$coverage > 1)) {
      cli::cli_abort("Opportunity coverage must be between 0 and 1.")
    }
    if (!all(events$type %in% c("mcv1", "mcv2", "sia"))) {
      cli::cli_abort("Opportunity types must be mcv1, mcv2 or sia.")
    }
  }
  invisible(TRUE)
}

# state ------------------------------------------------------------------------

# internal helper: a new state, one row per cohort and occupied state. every
# child starts with no doses, no MCV1 and no protection
#' @noRd
.new_state <- function(cells) {
  dplyr::tibble(
    cell = cells$cell,
    doses = 0L,
    mcv1 = FALSE,
    protected = FALSE,
    share = 1
  )
}

# internal helper: shares of each cohort by vaccination state
#' @noRd
.summarise_state <- function(state) {
  state |>
    dplyr::summarise(
      susceptible_share = sum(.data$share[!.data$protected]),
      dose_0 = sum(.data$share[.data$doses == 0]),
      dose_1 = sum(.data$share[.data$doses == 1]),
      dose_2 = sum(.data$share[.data$doses == 2]),
      dose_3_plus = sum(.data$share[.data$doses >= 3]),
      .by = "cell"
    )
}

# internal helper: effectiveness by dose type and lower bound of each age
# band in months
#' @noRd
.ve_table <- function(ve) {
  dplyr::tibble(
    age_from = ve$breaks,
    mcv1 = ve$mcv1,
    mcv2 = ve$mcv2,
    sia = ve$sia
  ) |>
    tidyr::pivot_longer(
      -"age_from",
      names_to = "type", values_to = "effectiveness"
    )
}

# internal helper: order in which states are reached. mcv2 reaches mcv1
# recipients first; dependent reaches anyone with a dose before children
# with none; complementary reaches fewer doses first
#' @noRd
.tier_lookup <- function(max_doses) {
  tidyr::expand_grid(
    type = c("mcv1", "mcv2", "sia"),
    coupling = c("dependent", "independent", "complementary"),
    doses = 0:max_doses,
    mcv1 = c(FALSE, TRUE)
  ) |>
    dplyr::mutate(
      rank = dplyr::case_when(
        .data$coupling == "dependent" ~ as.integer(.data$doses == 0),
        .data$coupling == "independent" ~ 0L,
        .data$coupling == "complementary" ~ .data$doses
      ),
      tier = dplyr::if_else(
        .data$type == "mcv2" & .data$mcv1, 0L, .data$rank + 1L
      )
    ) |>
    dplyr::select(-"rank")
}

# internal helper: reached children get one more dose; an mcv1 dose also
# sets the mcv1 flag
#' @noRd
.add_dose <- function(states, max_doses) {
  states |>
    dplyr::mutate(
      doses = pmin(.data$doses + 1L, max_doses),
      mcv1 = .data$mcv1 | .data$type == "mcv1"
    )
}

# internal helper: apply one opportunity per cohort. each tier takes what is
# left of the reach, in proportion to its share; reached children who are
# not protected become protected with the effectiveness of the dose
#' @noRd
.apply_opportunity <- function(state, events, tiers, max_doses) {
  untouched <- dplyr::anti_join(state, events, by = "cell")

  joined <- state |>
    dplyr::inner_join(events, by = "cell") |>
    dplyr::inner_join(tiers, by = c("type", "coupling", "doses", "mcv1"))

  fraction <- joined |>
    dplyr::summarise(
      tier_share = sum(.data$share),
      .by = c("cell", "tier", "reach")
    ) |>
    dplyr::arrange(.data$cell, .data$tier) |>
    dplyr::mutate(
      filled = cumsum(.data$tier_share) - .data$tier_share,
      fraction = (.data$reach - .data$filled) / .data$tier_share,
      fraction = pmin(pmax(.data$fraction, 0), 1),
      .by = "cell"
    ) |>
    dplyr::select("cell", "tier", "fraction")

  reached <- joined |>
    dplyr::left_join(fraction, by = c("cell", "tier")) |>
    dplyr::mutate(moved = .data$share * .data$fraction)

  # the share not reached stays; the reached share either fails or ends up
  # protected, and both gain a dose
  stays <- reached |>
    dplyr::mutate(share = .data$share - .data$moved)
  fails <- reached |>
    dplyr::filter(!.data$protected) |>
    dplyr::mutate(share = .data$moved * (1 - .data$effectiveness)) |>
    .add_dose(max_doses)
  protects <- reached |>
    dplyr::mutate(
      share = dplyr::if_else(
        .data$protected, .data$moved, .data$moved * .data$effectiveness
      ),
      protected = TRUE
    ) |>
    .add_dose(max_doses)

  dplyr::bind_rows(untouched, stays, fails, protects) |>
    dplyr::summarise(
      share = sum(.data$share),
      .by = c("cell", "doses", "mcv1", "protected")
    ) |>
    dplyr::filter(.data$share != 0)
}

# run --------------------------------------------------------------------------

# internal helper: run every cohort's opportunities in order, carrying the
# state from step to step. returns the state summary after each step and
# each opportunity's contribution
#' @noRd
.run_engine <- function(cells, events, coupling, ve, max_doses) {
  tiers <- .tier_lookup(max_doses)
  if (is.null(events)) {
    events <- dplyr::tibble(
      cell = integer(0), event_id = character(0), type = character(0),
      month = as.Date(character(0)), eligible_share = numeric(0),
      timing_share = numeric(0), age_at_dose = numeric(0),
      coverage = numeric(0)
    )
  }
  if (!"coupling" %in% names(events)) {
    events$coupling <- NA_character_
  }

  default_coupling <- coupling
  events <- events |>
    dplyr::filter(.data$coverage > 0, .data$eligible_share > 0) |>
    dplyr::mutate(
      coupling = dplyr::coalesce(.data$coupling, default_coupling),
      reach = .data$coverage * .data$eligible_share,
      age_at_dose = pmax(.data$age_at_dose, min(ve$breaks)),
      type_order = match(.data$type, c("mcv1", "mcv2", "sia"))
    ) |>
    dplyr::left_join(
      .ve_table(ve),
      by = dplyr::join_by("type", closest("age_at_dose" >= "age_from"))
    ) |>
    dplyr::arrange(.data$cell, .data$month, .data$type_order) |>
    dplyr::mutate(step = dplyr::row_number(), .by = "cell")

  by_step <- events |>
    tidyr::nest(.by = "step") |>
    dplyr::arrange(.data$step)

  states <- purrr::accumulate(
    by_step$data,
    \(state, step_events) {
      .apply_opportunity(state, step_events, tiers, max_doses)
    },
    .init = .new_state(cells),
    .simplify = FALSE
  )

  step_keys <- dplyr::bind_rows(
    dplyr::tibble(
      cell = cells$cell, step = 0L, month = as.Date(NA), timing_share = 1
    ),
    dplyr::select(
      events,
      "cell", "step", "event_id", "type", "month", "timing_share", "reach"
    )
  )

  steps <- purrr::map2(
    states, c(0L, by_step$step),
    \(state, j) dplyr::mutate(.summarise_state(state), step = j)
  ) |>
    purrr::list_rbind() |>
    dplyr::inner_join(step_keys, by = c("cell", "step")) |>
    dplyr::arrange(.data$cell, .data$step)

  contributions <- steps |>
    dplyr::mutate(
      newly_protected = dplyr::lag(.data$susceptible_share) -
        .data$susceptible_share,
      .by = "cell"
    ) |>
    dplyr::filter(.data$step > 0) |>
    dplyr::select(
      "cell", "event_id", "type", "month",
      reached = "reach", "newly_protected"
    )

  list(steps = steps, contributions = contributions)
}

# monthly values ---------------------------------------------------------------

# internal helper: state of each cohort on the 15th of each month. a month
# takes the state at the end of the latest earlier month with opportunities;
# a month with opportunities takes its own mid-month state instead, which is
# the end-of-month state less the part of each change still to come
#' @noRd
.monthly_values <- function(panel, cells, steps, geography) {
  value_cols <- c(
    "susceptible_share", "dose_0", "dose_1", "dose_2", "dose_3_plus"
  )
  start <- list(
    susceptible_share = 1, dose_0 = 1, dose_1 = 0, dose_2 = 0,
    dose_3_plus = 0
  )

  by_month <- steps |>
    tidyr::pivot_longer(
      dplyr::all_of(value_cols),
      names_to = "measure", values_to = "value"
    ) |>
    dplyr::arrange(.data$cell, .data$measure, .data$step) |>
    dplyr::mutate(
      pending = (.data$value - dplyr::lag(.data$value)) *
        (1 - .data$timing_share),
      .by = c("cell", "measure")
    ) |>
    dplyr::filter(.data$step > 0) |>
    dplyr::summarise(
      end = dplyr::last(.data$value),
      mid = dplyr::last(.data$value) - sum(.data$pending),
      .by = c("cell", "month", "measure")
    )

  ends <- by_month |>
    tidyr::pivot_wider(
      id_cols = c("cell", "month"),
      names_from = "measure", values_from = "end"
    ) |>
    dplyr::rename(event_month = "month")
  mids <- by_month |>
    tidyr::pivot_wider(
      id_cols = c("cell", "month"),
      names_from = "measure", values_from = "mid"
    )

  panel |>
    dplyr::left_join(cells, by = c(geography, "birth_month")) |>
    dplyr::left_join(
      ends,
      by = dplyr::join_by("cell", closest("month" > "event_month"))
    ) |>
    dplyr::rows_update(mids, by = c("cell", "month"), unmatched = "ignore") |>
    tidyr::replace_na(start) |>
    dplyr::select(-"cell", -"event_month")
}

# uncertainty ------------------------------------------------------------------

#' Uncertainty Intervals for Immunity Estimates
#'
#' Repeats [estimate_immunity()] with coverage drawn from its uncertainty
#' intervals, and summarises the susceptible population across draws.
#'
#' @details
#' Coverage is drawn on the logit scale, with a normal distribution whose
#' 95% interval matches `coverage_lower` to `coverage_upper` (IHME bounds
#' for routine coverage, survey intervals for campaigns). Coverage without
#' an interval is kept fixed.
#'
#' Routine coverage draws are correlated across years within a geography
#' and dose (first-order autocorrelation `rho_years`) and between MCV1 and
#' MCV2 in the same year (`rho_doses`). Each campaign record is drawn on its
#' own.
#'
#' @inheritParams estimate_immunity
#' @param n_draws Number of draws. Default `100`.
#' @param seed Random seed, so the draws can be reproduced. Default `1`.
#' @param rho_years Correlation of routine coverage between consecutive
#'   years. Default `0.9`.
#' @param rho_doses Correlation between MCV1 and MCV2 coverage in the same
#'   year. Default `0.5`.
#' @param ... Passed to [summarise_immunity()], for example `geography`,
#'   `at` and `age_breaks`.
#'
#' @return A data frame with the summary columns of [summarise_immunity()]
#'   (`geography`, `month`, `age_band`, `population`) and the median,
#'   2.5% and 97.5% quantiles across draws of `susceptible` and
#'   `susceptible_share`.
#'
#' @examples
#' \dontrun{
#' draws <- immunity_draws(
#'   history, cohorts, n_draws = 200, at = "2025-06",
#'   geography = c("adm0", "adm1")
#' )
#' }
#' @export
immunity_draws <- function(
  history,
  cohorts,
  n_draws = 100,
  seed = 1,
  ve = ve_cdc(),
  coupling = c("dependent", "independent", "complementary"),
  maternal_months = 6,
  rho_years = 0.9,
  rho_doses = 0.5,
  ...
) {
  coupling <- match.arg(coupling)
  .check_history(history)
  geography <- setdiff(names(history$cells), c("cell", "birth_month"))
  panel <- .cohort_panel_input(cohorts, geography)

  set.seed(seed)
  draws <- purrr::map(seq_len(n_draws), \(d) {
    drawn <- history
    drawn$history <- .draw_coverage(history$history, geography,
                                     rho_years, rho_doses)
    run <- .run_engine(
      drawn$cells, drawn$history, coupling, ve, max_doses = 8L
    )
    .monthly_values(panel, drawn$cells, run$steps, geography) |>
      dplyr::mutate(
        susceptible = .data$susceptible_share * .data$population,
        maternal_protection = .data$age_months < maternal_months
      ) |>
      summarise_immunity(...) |>
      dplyr::mutate(draw = d)
  }) |>
    purrr::list_rbind()

  keys <- setdiff(
    names(draws),
    c(
      "draw", "susceptible", "susceptible_share", "immune_share", "dose_0",
      "dose_1", "dose_2", "dose_3_plus", "population"
    )
  )
  draws |>
    dplyr::summarise(
      population = dplyr::first(.data$population),
      susceptible_median = stats::median(.data$susceptible),
      susceptible_lower = stats::quantile(.data$susceptible, 0.025),
      susceptible_upper = stats::quantile(.data$susceptible, 0.975),
      share_median = stats::median(.data$susceptible_share),
      share_lower = stats::quantile(.data$susceptible_share, 0.025),
      share_upper = stats::quantile(.data$susceptible_share, 0.975),
      .by = dplyr::all_of(keys)
    )
}

# internal helper: one draw of coverage for every opportunity. routine draws
# follow an autocorrelated series by geography and dose; campaigns are drawn
# record by record
#' @noRd
.draw_coverage <- function(events, geography, rho_years, rho_doses) {
  logit <- function(p) stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  events <- events |>
    dplyr::mutate(
      year = as.integer(format(.data$month, "%Y")),
      centre = logit(.data$coverage),
      spread = (logit(.data$coverage_upper) - logit(.data$coverage_lower)) /
        (2 * stats::qnorm(0.975)),
      spread = dplyr::coalesce(.data$spread, 0)
    )

  # one standard normal per geography and year, shared by MCV1 and MCV2
  # through rho_doses, and carried between years through rho_years
  routine_z <- events |>
    dplyr::filter(.data$type != "sia") |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(geography, "year")))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(geography, "year")))) |>
    dplyr::mutate(
      shared = .ar1(stats::rnorm(dplyr::n()), rho_years),
      own = .ar1(stats::rnorm(dplyr::n()), rho_years),
      .by = dplyr::all_of(geography)
    ) |>
    tidyr::expand_grid(type = c("mcv1", "mcv2")) |>
    dplyr::mutate(
      z = dplyr::if_else(
        .data$type == "mcv1", .data$shared,
        rho_doses * .data$shared + sqrt(1 - rho_doses^2) * .data$own
      )
    ) |>
    dplyr::select(dplyr::all_of(c(geography, "year", "type", "z")))

  campaign_z <- events |>
    dplyr::filter(.data$type == "sia") |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(geography, "event_id")))) |>
    dplyr::mutate(z_campaign = stats::rnorm(dplyr::n()))

  events |>
    dplyr::left_join(routine_z, by = c(geography, "year", "type")) |>
    dplyr::left_join(campaign_z, by = c(geography, "event_id")) |>
    dplyr::mutate(
      z = dplyr::coalesce(.data$z, .data$z_campaign, 0),
      coverage = dplyr::if_else(
        .data$spread == 0,
        .data$coverage,
        stats::plogis(.data$centre + .data$spread * .data$z)
      )
    ) |>
    dplyr::select(-"year", -"centre", -"spread", -"z", -"z_campaign")
}

# internal helper: a first-order autoregressive series with unit variance
#' @noRd
.ar1 <- function(e, rho) {
  purrr::accumulate(
    e[-1],
    \(previous, innovation) rho * previous + sqrt(1 - rho^2) * innovation,
    .init = e[1]
  )
}
