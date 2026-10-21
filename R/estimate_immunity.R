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
#' **Birth days.** Children born on different days of a month receive
#' their doses on different dates. Each cohort is therefore followed by day
#' of birth: a child born on the 20th is due a 9-month dose on the 20th of
#' the month nine months later, and is in a campaign's target ages on the
#' campaign days that fall within that age range for that birth day. Birth
#' days with the same sequence of dated opportunities (the same
#' opportunities, in the same order, in the same reporting months and
#' effectiveness bands) are run together and weighted by their share of
#' the cohort, so a cohort usually needs a handful of runs, never more than
#' one per day of the month.
#'
#' **States.** Each run is split into shares by number of doses received
#' (0 to `max_doses`), whether MCV1 was received, and whether the child is
#' protected. Reached children who are not yet protected become protected
#' with the vaccine effectiveness for the dose type and the child's exact
#' age on the date of the dose, so the 9 and 12 month bands of [ve_cdc()]
#' apply to the children who are actually that age.
#'
#' **Routine doses** happen on the date the birth day reaches the schedule
#' age and reach `coverage` of the run.
#'
#' **Campaigns** are spread over the campaign days on which the birth day
#' is in the target ages. Each of those days reaches `coverage / n` of the
#' run, where `n` is the number of such days, among children not yet
#' reached by this campaign, so no child is vaccinated twice in one
#' campaign and the campaign reaches exactly `coverage` of the run by its
#' last eligible day. A campaign spanning several months therefore evolves
#' month by month, and a routine dose falling within the campaign is applied
#' on its own date between the campaign days.
#'
#' **Who a dose reaches** (`coupling`):
#' - `"dependent"` (default): children who already had a dose are reached
#'   before children with none, as in the CDC method.
#' - `"independent"`: every child is equally likely to be reached.
#' - `"complementary"`: children with fewer doses are reached first.
#'
#' MCV2 always reaches MCV1 recipients first. A `coupling` column in the
#' history overrides the default for an opportunity. With no opportunity
#' between a campaign's first and last day, the campaign's effect equals the
#' CDC one-step formula for its coverage.
#'
#' **Order.** Opportunities are applied in date order. On the same date,
#' routine MCV1 comes before MCV2, both before campaigns, and campaigns in
#' order of `event_id`. Monthly values are the state on the 15th of each
#' month: every opportunity dated on or before the 15th has been applied in
#' full and nothing later has, so every reported share lies between 0 and 1
#' and the shares add up to 1.
#'
#' **Maternal antibodies.** Infants younger than `maternal_months` are
#' marked in `maternal_protection` and left out of [summarise_immunity()] by
#' default, rather than modelled.
#'
#' Vaccination is the only source of immunity: infection is not modelled.
#' Each state carries the set of campaigns that have already reached it, so
#' two campaigns whose days overlap each reach a child at most once, and a
#' campaign's marker is dropped on its last eligible day.
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
#'     \item{opportunities}{As `contributions`, with `date_from`,
#'       `date_to`, `eligible_share` and `first_doses`, the share of the
#'       cohort whose first ever dose came from this opportunity}
#'     \item{campaign_targets}{One row per cohort and campaign: the share of
#'       the cohort the campaign can reach (`eligible_share`), the dose
#'       shares of those children on the 15th of the month `before` the
#'       campaign and of the month `after` it (`dose_0_before`,
#'       `dose_0_after` and so on) and `first_doses`, for
#'       [check_immunity()]}
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
  .check_ve(ve)
  max_doses <- .check_count(max_doses, "max_doses", lower = 3)
  .check_number(maternal_months, "maternal_months", lower = 0)
  .check_history(history)
  cells <- history$cells
  geography <- setdiff(names(cells), c("cell", "birth_month"))
  panel <- .cohort_panel_input(cohorts, geography)

  run <- .run_cohorts(cells, history$history, coupling, ve, max_doses)

  immunity <- .monthly_values(
    panel, cells, run$units, run$unit_values, geography
  ) |>
    dplyr::mutate(
      susceptible = .data$susceptible_share * .data$population,
      maternal_protection = .data$age_months < maternal_months
    )

  opportunities <- run$opportunities |>
    dplyr::left_join(cells, by = "cell") |>
    dplyr::select(
      dplyr::all_of(geography), "birth_month", "event_id", "type", "month",
      "date_from", "date_to", "eligible_share", "reached", "newly_protected",
      "first_doses"
    )
  contributions <- dplyr::select(
    opportunities,
    dplyr::all_of(geography), "birth_month", "event_id", "type", "month",
    "reached", "newly_protected"
  )
  campaign_targets <- run$campaign_targets |>
    dplyr::left_join(cells, by = "cell") |>
    dplyr::select(
      dplyr::all_of(geography), "birth_month", "event_id", "eligible_share",
      "before", "after", dplyr::ends_with("_before"),
      dplyr::ends_with("_after"), "first_doses"
    )

  list(
    immunity = immunity,
    contributions = contributions,
    opportunities = opportunities,
    campaign_targets = campaign_targets,
    metadata = list(
      method = "CDC immunity profile (Li, Goodson and Perry 2024, Figure 1)",
      coupling = coupling,
      ve = ve,
      maternal_months = maternal_months,
      max_doses = max_doses,
      reporting_date = "15th of each month",
      timing_rule = paste(
        "exact dates by day of birth; routine doses on the date the",
        "schedule age is reached; campaigns spread evenly over each birth",
        "day's eligible campaign days, reaching each child at most once per",
        "campaign; same-date order mcv1, mcv2, campaigns by event_id"
      ),
      effectiveness_rule = "age on the date of the dose",
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
#'   age bands set by `breaks`. A single value applies to every band.
#' @param breaks Lower bounds of the age bands in months, strictly
#'   increasing. Ages below the first break use the first band.
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
  n <- length(breaks)
  values <- list(mcv1 = mcv1, mcv2 = mcv2, sia = sia) |>
    purrr::map(\(v) if (length(v) == 1) rep_len(v, n) else v)
  ve <- c(list(breaks = breaks), values)
  .check_ve(ve)
  ve
}

# internal helper: a vaccine effectiveness list as made by ve_cdc()
#' @noRd
.check_ve <- function(ve) {
  parts <- c("breaks", "mcv1", "mcv2", "sia")
  if (!is.list(ve) || !all(parts %in% names(ve))) {
    cli::cli_abort("{.arg ve} must be a list from {.fn ve_cdc}.")
  }
  breaks <- ve$breaks
  if (!is.numeric(breaks) || length(breaks) == 0 || any(!is.finite(breaks)) ||
        is.unsorted(breaks, strictly = TRUE)) {
    cli::cli_abort(
      "{.arg ve} breaks must be finite, strictly increasing ages in months."
    )
  }
  values <- purrr::map(c("mcv1", "mcv2", "sia"), \(type) ve[[type]])
  right_length <- purrr::every(
    values, \(v) is.numeric(v) && length(v) == length(breaks)
  )
  if (!right_length) {
    cli::cli_abort(
      "Vaccine effectiveness needs one value per age band in {.arg ve}."
    )
  }
  all_values <- purrr::list_c(values)
  if (any(!is.finite(all_values)) || any(all_values < 0 | all_values > 1)) {
    cli::cli_abort("Vaccine effectiveness must be between 0 and 1.")
  }
  invisible(TRUE)
}

# internal helper: default number of parallel workers, the CPU cores less 4
#' @noRd
.default_workers <- function() {
  cores <- parallel::detectCores()
  if (is.na(cores)) 1L else max(1L, cores - 4L)
}

# internal helper: a single positive whole number
#' @noRd
.check_count <- function(x, arg, lower = 1) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < lower ||
        x %% 1 != 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number, {lower} or more."
    )
  }
  as.integer(x)
}

# internal helper: a single finite number within bounds
#' @noRd
.check_number <- function(x, arg, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < lower ||
        x > upper) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number between {lower} and {upper}."
    )
  }
  invisible(x)
}

#' @noRd
.check_history <- function(history) {
  if (!is.list(history) || !all(c("history", "cells") %in% names(history))) {
    cli::cli_abort(
      "{.arg history} must be the output of {.fn build_vaccination_history}."
    )
  }
  events <- history$history
  needed <- c(
    "cell", "birth_month", "event_id", "type", "record", "day_from",
    "day_to", "date_from", "date_to", "month", "eligible_share",
    "age_at_dose", "age_months", "start_date", "end_date", "age_min_months",
    "age_max_months", "coverage", "coverage_lower", "coverage_upper",
    "coverage_year"
  )
  if (!is.data.frame(events) || !all(needed %in% names(events)) ||
        !is.data.frame(history$cells) ||
        !all(c("cell", "birth_month") %in% names(history$cells))) {
    cli::cli_abort(c(
      "{.arg history} must be the output of {.fn build_vaccination_history}.",
      "i" = "Rebuild it with the current version of the package."
    ))
  }
  if (nrow(events) > 0) {
    if (any(!is.finite(events$coverage)) ||
          any(events$coverage < 0 | events$coverage > 1)) {
      cli::cli_abort("Opportunity coverage must be between 0 and 1.")
    }
    if (!all(events$type %in% c("mcv1", "mcv2", "sia"))) {
      cli::cli_abort("Opportunity types must be mcv1, mcv2 or sia.")
    }
    if (any(events$day_from < 1 | events$day_to < events$day_from) ||
          any(events$date_to < events$date_from)) {
      cli::cli_abort("Opportunity days and dates are out of order.")
    }
    couplings <- .optional_column(events, "coupling")
    if (!all(is.na(couplings) |
               couplings %in% c("dependent", "independent", "complementary"))) {
      cli::cli_abort(
        "A {.val coupling} column in the history must hold the coupling rules."
      )
    }
  }
  invisible(TRUE)
}

# state ------------------------------------------------------------------------

# internal helper: a new state, one row per cohort and occupied state. every
# child starts with no doses, no MCV1, no protection and no campaign marker
#' @noRd
.new_state <- function(cells) {
  dplyr::tibble(
    cell = cells$cell,
    doses = 0L,
    mcv1 = FALSE,
    protected = FALSE,
    markers = "",
    share = 1
  )
}

# internal helper: a text key for each set of campaign markers, so states
# can be grouped on it. each identifier is prefixed with its length, so
# identifiers containing the separator stay unambiguous
#' @noRd
.marker_key <- function(markers) {
  purrr::map_chr(markers, \(m) {
    if (length(m) == 0) {
      return("")
    }
    sorted <- sort(m)
    paste(paste0(nchar(sorted), ":", sorted), collapse = "|")
  })
}

# internal helper: the campaign identifiers in a marker key
#' @noRd
.marker_ids <- function(key) {
  ids <- character()
  while (nchar(key) > 0) {
    n <- as.integer(sub(":.*$", "", key))
    start <- nchar(n) + 2L
    ids <- c(ids, substr(key, start, start + n - 1L))
    key <- substr(key, start + n + 1L, nchar(key))
  }
  ids
}

# internal helper: apply `fun` to each marker key and identifier. states
# share few distinct keys, so each distinct pair is worked out only once
#' @noRd
.marker_apply <- function(key, id, fun, type = character()) {
  pairs <- dplyr::tibble(key = key, id = id)
  done <- dplyr::distinct(pairs) |>
    dplyr::mutate(
      out = purrr::map2_vec(
        key, id, \(k, i) fun(.marker_ids(k), i), .ptype = type
      )
    )
  dplyr::left_join(pairs, done, by = c("key", "id"))$out
}

# internal helper: whether each marker key holds the identifier
#' @noRd
.marker_has <- function(key, id) {
  .marker_apply(key, id, \(m, i) i %in% m, type = logical())
}

# internal helper: marker keys with the identifier added
#' @noRd
.marker_add <- function(key, id) {
  .marker_apply(key, id, \(m, i) .marker_key(list(union(m, i))))
}

# internal helper: marker keys with the identifier removed
#' @noRd
.marker_drop <- function(key, id) {
  .marker_apply(key, id, \(m, i) .marker_key(list(setdiff(m, i))))
}

# internal helper: shares of each cohort by vaccination state
#' @noRd
.summarise_state <- function(state) {
  state |>
    dplyr::summarise(
      susceptible_share = sum(share * !protected),
      dose_0 = sum(share * (doses == 0)),
      dose_1 = sum(share * (doses == 1)),
      dose_2 = sum(share * (doses == 2)),
      dose_3_plus = sum(share * (doses >= 3)),
      .by = "cell"
    )
}

# internal helper: effectiveness by dose type and age band, with the lower
# bound of each band in months
#' @noRd
.ve_table <- function(ve) {
  dplyr::tibble(
    band = seq_along(ve$breaks),
    age_from = ve$breaks,
    mcv1 = ve$mcv1,
    mcv2 = ve$mcv2,
    sia = ve$sia
  ) |>
    tidyr::pivot_longer(
      c("mcv1", "mcv2", "sia"),
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
# sets the mcv1 flag; a campaign day marks them as reached by the campaign
#' @noRd
.add_dose <- function(states, max_doses) {
  states |>
    dplyr::mutate(
      doses = pmin(.data$doses + 1L, max_doses),
      mcv1 = .data$mcv1 | .data$type == "mcv1",
      markers = dplyr::if_else(
        .data$set_marker,
        .marker_add(.data$markers, .data$event_id),
        .data$markers
      )
    )
}

# internal helper: apply one opportunity per cohort. the opportunity reaches
# `reach` of the eligible share (everyone, or children not yet marked by
# this campaign when `restrict`); each tier takes what is left of that
# mass in proportion to its share; reached children who are not protected
# become protected with the effectiveness of the dose. markers of a
# campaign are cleared on its last day (`clear_marker`)
#' @noRd
.apply_opportunity <- function(state, events, tiers, max_doses) {
  state_cols <- c("cell", "doses", "mcv1", "protected", "markers", "share")
  untouched <- dplyr::anti_join(state, events, by = "cell")

  joined <- state |>
    dplyr::inner_join(events, by = "cell") |>
    dplyr::mutate(
      eligible = !.data$restrict |
        !.marker_has(.data$markers, .data$event_id)
    )
  passive <- joined |>
    dplyr::filter(!.data$eligible) |>
    dplyr::select(dplyr::all_of(state_cols))
  active <- joined |>
    dplyr::filter(.data$eligible) |>
    dplyr::inner_join(tiers, by = c("type", "coupling", "doses", "mcv1"))

  fraction <- active |>
    dplyr::summarise(
      tier_share = sum(share),
      .by = c("cell", "tier", "reach")
    ) |>
    dplyr::arrange(.data$cell, .data$tier) |>
    dplyr::mutate(
      mass = reach * sum(tier_share),
      filled = cumsum(tier_share) - tier_share,
      fraction = (mass - filled) / tier_share,
      fraction = pmin(pmax(fraction, 0), 1),
      .by = "cell"
    ) |>
    dplyr::select("cell", "tier", "fraction")

  reached <- active |>
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

  # a campaign's own marker is dropped on its last eligible day, so states
  # merge again once the campaign is over
  clearing <- events |>
    dplyr::filter(.data$clear_marker) |>
    dplyr::select("cell", ended = "event_id")

  dplyr::bind_rows(
    passive,
    dplyr::select(stays, dplyr::all_of(state_cols)),
    dplyr::select(fails, dplyr::all_of(state_cols)),
    dplyr::select(protects, dplyr::all_of(state_cols))
  ) |>
    dplyr::left_join(clearing, by = "cell") |>
    dplyr::mutate(
      markers = dplyr::if_else(
        is.na(.data$ended),
        .data$markers,
        .marker_drop(.data$markers, dplyr::coalesce(.data$ended, ""))
      )
    ) |>
    dplyr::summarise(
      share = sum(share),
      .by = c("cell", "doses", "mcv1", "protected", "markers")
    ) |>
    dplyr::filter(.data$share != 0) |>
    dplyr::select(dplyr::all_of(state_cols)) |>
    dplyr::bind_rows(untouched)
}

# run --------------------------------------------------------------------------

# internal helper: a typed, empty table of engine events
#' @noRd
.engine_empty_events <- function() {
  dplyr::tibble(
    cell = integer(0), event_id = character(0), type = character(0),
    month = as.Date(character(0)), eligible_share = numeric(0),
    timing_share = numeric(0), age_at_dose = numeric(0),
    coverage = numeric(0)
  )
}

# internal helper: run whole-cohort opportunities in month order. each event
# reaches coverage x eligible_share of its cohort with the effectiveness for
# `age_at_dose`, as in the CDC one-step formulas. returns the state summary
# after each step and each opportunity's contribution
#' @noRd
.run_engine <- function(cells, events, coupling, ve, max_doses) {
  if (is.null(events)) {
    events <- .engine_empty_events()
  }
  events <- events |>
    dplyr::mutate(
      reach = .data$coverage * .data$eligible_share,
      age_at_dose = pmax(.data$age_at_dose, min(ve$breaks)),
      type_order = match(.data$type, c("mcv1", "mcv2", "sia")),
      restrict = FALSE,
      set_marker = FALSE,
      clear_marker = FALSE
    ) |>
    dplyr::left_join(
      .ve_table(ve),
      by = dplyr::join_by("type", closest("age_at_dose" >= "age_from"))
    ) |>
    dplyr::arrange(.data$cell, .data$month, .data$type_order, .data$event_id)
  .run_steps(cells, events, coupling, max_doses)
}

# internal helper: run every cohort's events in the order given, carrying
# the state from step to step. events need cell, event_id, type, reach,
# effectiveness, restrict, set_marker and clear_marker, and may carry a
# coupling column. returns the state summary after each step (with the
# event columns in key_cols), each event's contribution, the full states
# and the events with their step numbers
#' @noRd
.run_steps <- function(cells, events, coupling, max_doses) {
  tiers <- .tier_lookup(max_doses)
  default_coupling <- coupling
  events <- events |>
    dplyr::mutate(coupling = .optional_column(events, "coupling")) |>
    dplyr::filter(.data$reach > 0) |>
    dplyr::mutate(
      coupling = dplyr::coalesce(coupling, default_coupling)
    ) |>
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

  key_cols <- intersect(
    c(
      "cell", "step", "event_id", "type", "month", "report_month", "reach",
      "history_row"
    ),
    names(events)
  )
  step_keys <- dplyr::bind_rows(
    dplyr::tibble(cell = cells$cell, step = 0L),
    dplyr::select(events, dplyr::all_of(key_cols))
  )

  # after step 0, only the cohorts with an event at that step are kept
  step_cells <- c(list(cells$cell), purrr::map(by_step$data, "cell"))
  steps <- purrr::pmap(
    list(states, c(0L, by_step$step), step_cells),
    \(state, j, step_cell) {
      state |>
        dplyr::filter(.data$cell %in% step_cell) |>
        .summarise_state() |>
        dplyr::mutate(step = j)
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::inner_join(step_keys, by = c("cell", "step")) |>
    dplyr::arrange(.data$cell, .data$step)

  contributions <- steps |>
    dplyr::mutate(
      newly_protected = dplyr::lag(susceptible_share) -
        susceptible_share,
      first_doses = dplyr::lag(dose_0) - dose_0,
      .by = "cell"
    ) |>
    dplyr::filter(.data$step > 0) |>
    dplyr::select(
      dplyr::any_of(c(
        "cell", "step", "event_id", "type", "month", "report_month",
        "history_row"
      )),
      reached = "reach", "newly_protected", "first_doses"
    )

  list(steps = steps, contributions = contributions, states = states,
       events = events)
}

# birth days -------------------------------------------------------------------

# internal helper: run the opportunities of every cohort by day of birth,
# with exact dates. returns the runs of birth days (`units`, with their
# cohort and weight), the state of each run at the 15th of every month in
# which it changes, and each opportunity's contribution to its cohort
#' @noRd
.run_cohorts <- function(cells, events, coupling, ve, max_doses) {
  # a campaign may share its name with a dose type, and a dose may be due
  # at different ages in different periods, so opportunities are keyed by
  # type, identifier and schedule age together
  events <- events |>
    dplyr::mutate(
      history_row = dplyr::row_number(),
      event_key = .record_key(
        events, c("type", "event_id", "age_months"), "event"
      ),
      coupling = as.character(.optional_column(events, "coupling"))
    )
  patterns <- .cohort_patterns(cells, events)
  dated <- .pattern_subevents(patterns$patterns, patterns$definitions, ve)

  units <- patterns$cells |>
    dplyr::inner_join(
      dated$groups, by = "pattern", relationship = "many-to-many"
    ) |>
    dplyr::arrange(.data$cell, .data$rep_day) |>
    dplyr::mutate(unit = dplyr::row_number()) |>
    dplyr::select("unit", "cell", "pattern", "group", "rep_day", "weight")

  # each run's sub-events take the coverage of its cohort's history row
  unit_events <- units |>
    dplyr::inner_join(
      dated$subevents, by = c("pattern", "group"),
      relationship = "many-to-many"
    ) |>
    dplyr::inner_join(
      dplyr::select(
        events, "cell", "event_key", "history_row", "day_from", "day_to",
        "coverage", "coupling"
      ),
      by = dplyr::join_by(
        "cell", "event_key", "rep_day" >= "day_from", "rep_day" <= "day_to"
      )
    ) |>
    dplyr::mutate(
      reach = dplyr::if_else(
        .data$type == "sia",
        (.data$coverage / .data$n) /
          (1 - .data$coverage * (.data$k - 1) / .data$n),
        .data$coverage
      ),
      restrict = .data$type == "sia",
      set_marker = .data$type == "sia",
      clear_marker = .data$type == "sia" & .data$k == .data$n
    ) |>
    dplyr::left_join(
      dplyr::select(.ve_table(ve), "band", "type", "effectiveness"),
      by = c("band", "type")
    ) |>
    dplyr::arrange(
      .data$unit, .data$date, .data$type_order, .data$event_id, .data$k
    ) |>
    dplyr::transmute(
      cell = .data$unit, .data$event_id, .data$type, .data$reach,
      .data$effectiveness, .data$restrict, .data$set_marker,
      .data$clear_marker, .data$report_month, .data$history_row,
      .data$coupling
    )

  run <- .run_steps(
    dplyr::transmute(units, cell = .data$unit), unit_events, coupling,
    max_doses
  )

  value_cols <- c(
    "susceptible_share", "dose_0", "dose_1", "dose_2", "dose_3_plus"
  )
  unit_values <- run$steps |>
    dplyr::filter(.data$step > 0) |>
    dplyr::slice_max(
      .data$step, n = 1, by = c("cell", "report_month"), with_ties = FALSE
    ) |>
    dplyr::transmute(
      unit = .data$cell,
      month = .month_from_index(.data$report_month),
      dplyr::across(dplyr::all_of(value_cols))
    )

  opportunities <- .cohort_opportunities(events, run$contributions, units)
  membership <- dplyr::distinct(
    unit_events, unit = .data$cell, .data$history_row
  )

  list(
    units = units,
    unit_values = unit_values,
    opportunities = opportunities,
    campaign_targets = .campaign_targets(
      events, membership, units, unit_values, opportunities
    )
  )
}

# internal helper: the vaccination state of each campaign's target children
# (the birth days it can reach) on the 15th of the month before it starts
# and of the month after it ends, from the exact run states. one row per
# cohort and campaign, with the share of the cohort that is targeted
#' @noRd
.campaign_targets <- function(events, membership, units, unit_values,
                              opportunities) {
  value_cols <- c("dose_0", "dose_1", "dose_2", "dose_3_plus")
  start <- list(dose_0 = 1, dose_1 = 0, dose_2 = 0, dose_3_plus = 0)
  campaigns <- events |>
    dplyr::filter(.data$type == "sia") |>
    dplyr::transmute(
      .data$history_row, .data$cell, .data$event_id,
      before = .month_from_index(.month_index(.data$start_date) - 1L),
      after = .month_from_index(.month_index(.data$end_date) + 1L)
    )
  changes <- dplyr::select(
    unit_values, "unit", value_month = "month", dplyr::all_of(value_cols)
  )

  .state_at <- function(targets, when) {
    targets |>
      dplyr::mutate(at_month = .data[[when]]) |>
      dplyr::left_join(
        changes,
        by = dplyr::join_by("unit", closest("at_month" >= "value_month"))
      ) |>
      tidyr::replace_na(start) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(value_cols),
          \(x) sum(x * weight) / sum(weight),
          .names = paste0("{.col}_", when)
        ),
        .by = "history_row"
      )
  }

  targets <- campaigns |>
    dplyr::inner_join(membership, by = "history_row") |>
    dplyr::inner_join(
      dplyr::select(units, "unit", "weight"), by = "unit"
    )
  by_row <- targets |>
    dplyr::summarise(
      eligible_share = sum(weight),
      .by = c("history_row", "cell", "event_id", "before", "after")
    ) |>
    dplyr::inner_join(.state_at(targets, "before"), by = "history_row") |>
    dplyr::inner_join(.state_at(targets, "after"), by = "history_row")

  # cohorts with several runs of eligible days for one campaign are added
  # up by their eligible shares
  by_row |>
    dplyr::summarise(
      before = dplyr::first(before),
      after = dplyr::first(after),
      dplyr::across(
        dplyr::all_of(c(
          paste0(value_cols, "_before"), paste0(value_cols, "_after")
        )),
        \(x) sum(x * eligible_share) / sum(eligible_share)
      ),
      eligible_share = sum(eligible_share),
      .by = c("cell", "event_id")
    ) |>
    dplyr::inner_join(
      dplyr::select(opportunities, "cell", "event_id", "first_doses"),
      by = c("cell", "event_id")
    )
}

# internal helper: group cohorts that share a birth month and the same set
# of opportunity definitions (dates, ages and eligible days, not coverage),
# so the dated sub-events are worked out once per group
#' @noRd
.cohort_patterns <- function(cells, events) {
  definition_cols <- c(
    "event_key", "event_id", "type", "age_months", "start_date", "end_date",
    "age_min_months", "age_max_months", "day_from", "day_to"
  )
  keyed <- events |>
    dplyr::arrange(
      .data$cell, dplyr::across(dplyr::all_of(definition_cols))
    )
  keyed <- keyed |>
    dplyr::mutate(definition = .record_key(keyed, definition_cols, "")) |>
    dplyr::summarise(
      def_key = paste(.data$definition, collapse = ";"), .by = "cell"
    )
  cells_keyed <- cells |>
    dplyr::select("cell", "birth_month") |>
    dplyr::left_join(keyed, by = "cell") |>
    dplyr::mutate(def_key = dplyr::coalesce(.data$def_key, ""))
  patterns <- cells_keyed |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c("birth_month", "def_key")))
    ) |>
    dplyr::mutate(pattern = dplyr::row_number())
  cells_keyed <- cells_keyed |>
    dplyr::inner_join(patterns, by = c("birth_month", "def_key")) |>
    dplyr::select("cell", "pattern")

  # one representative cohort per pattern gives its definitions
  representative <- cells_keyed |>
    dplyr::slice_min(.data$cell, n = 1, by = "pattern", with_ties = FALSE)
  definitions <- events |>
    dplyr::inner_join(representative, by = "cell") |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c("pattern", definition_cols)))
    )

  list(
    cells = cells_keyed,
    patterns = dplyr::select(patterns, "pattern", "birth_month"),
    definitions = definitions
  )
}

# internal helper: the dated sub-events of every birth day of each pattern:
# one per routine dose, and one per eligible campaign day. birth days with
# the same sequence of sub-events (same opportunities in the same order,
# reporting months and effectiveness bands) form one group, represented by
# its first day. returns the groups with their weights and the sub-events
# of each group's representative day
#' @noRd
.pattern_subevents <- function(patterns, definitions, ve) {
  days <- patterns |>
    dplyr::mutate(
      n_days = .days_in_month(.month_index(.data$birth_month)),
      days_in_month = .data$n_days
    ) |>
    tidyr::uncount(.data$n_days, .id = "day") |>
    dplyr::mutate(birth_date = .data$birth_month + .data$day - 1L)
  on_days <- definitions |>
    dplyr::inner_join(
      dplyr::select(days, "pattern", "day", "birth_date"),
      by = dplyr::join_by("pattern", "day_from" <= "day", "day_to" >= "day")
    )

  routine <- on_days |>
    dplyr::filter(.data$type != "sia") |>
    dplyr::mutate(
      date = .add_months(.data$birth_date, .data$age_months),
      k = 1L, n = 1L
    )
  campaign <- on_days |>
    dplyr::filter(.data$type == "sia") |>
    dplyr::mutate(
      from = pmax(
        .data$start_date, .add_months(.data$birth_date, .data$age_min_months)
      ),
      to = pmin(
        .data$end_date,
        .add_months(.data$birth_date, .data$age_max_months) - 1L
      ),
      n = as.integer(.data$to - .data$from) + 1L
    ) |>
    tidyr::uncount(.data$n, .id = "k", .remove = FALSE) |>
    dplyr::mutate(date = .data$from + .data$k - 1L) |>
    dplyr::select(-"from", -"to")

  bands <- dplyr::tibble(age_from = ve$breaks) |>
    dplyr::mutate(band = dplyr::row_number())
  subevents <- dplyr::bind_rows(routine, campaign) |>
    dplyr::mutate(
      age = pmax(.age_in_months(.data$birth_date, .data$date), min(ve$breaks)),
      report_month = .report_month_index(.data$date),
      type_order = match(.data$type, c("mcv1", "mcv2", "sia"))
    ) |>
    dplyr::left_join(
      bands, by = dplyr::join_by(closest("age" >= "age_from"))
    ) |>
    dplyr::arrange(
      .data$pattern, .data$day, .data$date, .data$type_order,
      .data$event_id, .data$k
    ) |>
    dplyr::select(
      "pattern", "day", "event_key", "event_id", "type", "type_order", "k",
      "n", "date", "report_month", "band"
    )

  signatures <- subevents |>
    dplyr::summarise(
      signature = paste(
        event_key, k, n, report_month,
        band, sep = ":", collapse = ";"
      ),
      .by = c("pattern", "day")
    )
  grouped_days <- days |>
    dplyr::left_join(signatures, by = c("pattern", "day")) |>
    dplyr::mutate(signature = dplyr::coalesce(.data$signature, ""))
  groups <- grouped_days |>
    dplyr::summarise(
      rep_day = min(day),
      weight = dplyr::n() / dplyr::first(days_in_month),
      .by = c("pattern", "signature")
    ) |>
    dplyr::arrange(.data$pattern, .data$rep_day) |>
    dplyr::mutate(group = dplyr::row_number()) |>
    dplyr::select("pattern", "group", "rep_day", "weight")

  list(
    groups = groups,
    subevents = subevents |>
      dplyr::inner_join(
        dplyr::select(groups, "pattern", "group", day = "rep_day"),
        by = c("pattern", "day")
      ) |>
      dplyr::select(-"day")
  )
}

# internal helper: the first month whose reporting date (the 15th) is on or
# after each date
#' @noRd
.report_month_index <- function(date) {
  .month_index(date) + as.integer(as.POSIXlt(date)$mday > 15L)
}

# internal helper: each opportunity's contribution to its cohort, adding up
# the runs of birth days it covers. one row per cohort and opportunity
#' @noRd
.cohort_opportunities <- function(events, contributions, units) {
  if (nrow(events) == 0) {
    return(dplyr::tibble(
      cell = integer(), event_id = character(), type = character(),
      month = as.Date(character()), date_from = as.Date(character()),
      date_to = as.Date(character()), eligible_share = numeric(),
      reached = numeric(), newly_protected = numeric(),
      first_doses = numeric()
    ))
  }
  by_row <- contributions |>
    dplyr::inner_join(
      dplyr::select(units, "unit", "weight"), by = c(cell = "unit")
    ) |>
    dplyr::summarise(
      newly_protected = sum(newly_protected * weight),
      first_doses = sum(first_doses * weight),
      .by = "history_row"
    )

  events |>
    dplyr::left_join(by_row, by = "history_row") |>
    dplyr::mutate(
      newly_protected = dplyr::coalesce(.data$newly_protected, 0),
      first_doses = dplyr::coalesce(.data$first_doses, 0)
    ) |>
    dplyr::summarise(
      month = min(month),
      date_from = min(date_from),
      date_to = max(date_to),
      eligible_share = sum(eligible_share),
      reached = sum(coverage * eligible_share),
      newly_protected = sum(newly_protected),
      first_doses = sum(first_doses),
      .by = c("cell", "event_id", "type")
    ) |>
    dplyr::arrange(.data$cell, .data$date_from, .data$type)
}

# monthly values ---------------------------------------------------------------

# internal helper: state of each cohort on the 15th of each month, adding up
# its runs of birth days by their share of the cohort. a run keeps the
# state of its latest change month; before any change every child is
# unvaccinated. extra columns of `cells`, such as a draw number, are kept
#' @noRd
.monthly_values <- function(panel, cells, units, unit_values, geography) {
  value_cols <- c(
    "susceptible_share", "dose_0", "dose_1", "dose_2", "dose_3_plus"
  )
  start <- list(
    susceptible_share = 1, dose_0 = 1, dose_1 = 0, dose_2 = 0,
    dose_3_plus = 0
  )
  changes <- dplyr::rename(unit_values, value_month = "month")

  joined <- panel |>
    dplyr::left_join(cells, by = c(geography, "birth_month")) |>
    dplyr::inner_join(
      dplyr::select(units, "unit", "cell", "weight"),
      by = "cell", relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      changes,
      by = dplyr::join_by("unit", closest("month" >= "value_month"))
    ) |>
    tidyr::replace_na(start)
  keys <- setdiff(
    names(joined),
    c(value_cols, "cell", "unit", "weight", "value_month")
  )
  joined |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), \(x) sum(x * weight)),
      .by = dplyr::all_of(keys)
    )
}

# uncertainty ------------------------------------------------------------------

#' Uncertainty Intervals for Immunity Estimates
#'
#' Repeats [estimate_immunity()] with coverage drawn from its uncertainty
#' intervals, and summarises the susceptible population across draws.
#'
#' @details
#' Coverage is drawn on the logit scale from a normal distribution centred
#' on the logit of the coverage, with a standard deviation set so that the
#' width of its 95% interval matches the width of `coverage_lower` to
#' `coverage_upper` on the logit scale (IHME bounds for routine coverage,
#' survey intervals for campaigns). The draws are symmetric on the logit
#' scale, so an interval that is not symmetric around the coverage on that
#' scale is matched in width only, not bound by bound. Coverage without an
#' interval is kept fixed, and the draws never move it.
#'
#' One value is drawn per coverage record (`record` in the history), so a
#' campaign record given for a whole country or region is drawn once and
#' applied to every cohort inside it. Routine coverage draws are correlated
#' across coverage years within a geography and dose with first-order
#' autocorrelation `rho_years` per year elapsed (so years two apart have
#' correlation `rho_years^2`, whether or not the year between has data),
#' and between MCV1 and MCV2 in the same year (`rho_doses`).
#'
#' The random number generator state is restored after the call, so the
#' draws do not disturb other random work in the session.
#'
#' **Parallel runs.** With `workers` above 1, batches of draws run in
#' parallel on that many mirai daemons, which are started for the call and
#' shut down afterwards. Batches are made small enough to give every worker
#' at least one. Daemons already set with [mirai::daemons()] are
#' used instead. Coverage is drawn before the batches are sent out and each
#' draw's engine run is deterministic, so the result is the same for any
#' `workers` and `batch_size`. The daemons load the installed mipR.
#'
#' @inheritParams estimate_immunity
#' @param n_draws Number of draws. Default `100`.
#' @param seed Random seed, so the draws can be reproduced. Default `1`.
#' @param rho_years Correlation of routine coverage between consecutive
#'   years, between -1 and 1. Default `0.9`.
#' @param rho_doses Correlation between MCV1 and MCV2 coverage in the same
#'   year, between -1 and 1. Default `0.5`.
#' @param batch_size Number of draws run through the engine together.
#'   Larger batches use more memory. Default `10`.
#' @param workers Number of parallel workers. Default: the number of CPU
#'   cores less 4, and at least 1. Above 1 needs the mirai package; without
#'   it the draws run one batch at a time.
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
  batch_size = 10,
  workers = .default_workers(),
  max_doses = 8L,
  ...
) {
  coupling <- match.arg(coupling)
  .check_ve(ve)
  n_draws <- .check_count(n_draws, "n_draws")
  batch_size <- .check_count(batch_size, "batch_size")
  workers <- .check_count(workers, "workers")
  max_doses <- .check_count(max_doses, "max_doses", lower = 3)
  .check_number(seed, "seed")
  .check_number(rho_years, "rho_years", lower = -1, upper = 1)
  .check_number(rho_doses, "rho_doses", lower = -1, upper = 1)
  .check_number(maternal_months, "maternal_months", lower = 0)
  .check_history(history)
  geography <- setdiff(names(history$cells), c("cell", "birth_month"))
  panel <- .cohort_panel_input(cohorts, geography)
  cells <- history$cells

  # with `at`, only the cohorts alive in those months and the opportunities
  # that have started by the last of them affect the summary, so the engine
  # runs on those alone
  at <- list(...)$at
  last_report <- NULL
  if (!is.null(at)) {
    months <- .month_from_index(.month_index(.as_date(at, "at")))
    last_report <- max(months) + 14L
    panel <- dplyr::filter(panel, .data$month %in% months)
    cells <- dplyr::semi_join(
      cells, panel, by = c(geography, "birth_month")
    )
  }

  # coverage is drawn for every opportunity before trimming, so the draws
  # do not depend on `at`
  drawn <- .with_seed(seed, {
    purrr::map(seq_len(n_draws), \(d) {
      drawn_events <- .draw_coverage(
        history$history, geography, rho_years, rho_doses
      )
      if (!is.null(last_report)) {
        drawn_events <- drawn_events |>
          dplyr::filter(
            .data$cell %in% cells$cell,
            .data$date_from <= last_report
          )
      }
      drawn_events
    })
  })

  # the summary is grouped by draw as well as geography
  summary_args <- list(...)
  if (!is.null(summary_args$geography)) {
    summary_args$geography <- c(summary_args$geography, "draw")
  }

  parallel <- requireNamespace("mirai", quietly = TRUE) &&
    mirai::daemons_set()
  n_workers <- if (parallel) mirai::status()$connections else 1L
  if (!parallel && workers > 1) {
    if (requireNamespace("mirai", quietly = TRUE)) {
      mirai::daemons(workers)
      n_workers <- workers
      on.exit(mirai::daemons(0), add = TRUE)
      parallel <- TRUE
    } else {
      cli::cli_inform(
        "Install {.pkg mirai} to run the draws in parallel."
      )
    }
  }

  # in parallel, batches are capped so that every worker gets one
  if (parallel) {
    batch_size <- min(batch_size, ceiling(n_draws / max(1L, n_workers)))
  }

  # batches of draws, each run through the engine in one pass; batches run
  # in parallel on mirai daemons
  batches <- dplyr::tibble(draw = seq_len(n_draws), events = drawn) |>
    dplyr::mutate(batch = (.data$draw - 1L) %/% batch_size) |>
    tidyr::nest(.by = "batch") |>
    dplyr::pull("data")
  batch_args <- list(
    cells = cells, cell_offset = max(c(0L, history$cells$cell)),
    panel = panel, geography = geography, coupling = coupling, ve = ve,
    maternal_months = maternal_months, max_doses = max_doses,
    summary_args = summary_args
  )
  draws <- if (parallel) {
    mirai::mirai_map(batches, .draw_batch, .args = batch_args) |>
      mirai::collect_mirai(options = c(".stop", ".progress"))
  } else {
    purrr::map(batches, \(batch) {
      do.call(.draw_batch, c(list(batch), batch_args))
    })
  }
  draws <- draws |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$draw)

  keys <- setdiff(
    names(draws),
    c(
      "draw", "susceptible", "susceptible_share", "immune_share", "dose_0",
      "dose_1", "dose_2", "dose_3_plus", "population"
    )
  )
  draws |>
    dplyr::summarise(
      population = dplyr::first(population),
      susceptible_median = stats::median(susceptible),
      susceptible_lower = stats::quantile(susceptible, 0.025),
      susceptible_upper = stats::quantile(susceptible, 0.975),
      share_median = stats::median(susceptible_share),
      share_lower = stats::quantile(susceptible_share, 0.025),
      share_upper = stats::quantile(susceptible_share, 0.975),
      .by = dplyr::all_of(keys)
    )
}

# internal helper: run code with a seed, then put the random number
# generator back as it was
#' @noRd
.with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit(
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    },
    add = TRUE
  )
  set.seed(seed)
  force(code)
}

# internal helper: run one batch of draws through the engine and summarise
# it. every cohort is copied once per draw under its own cell number. batch
# has one row per draw, with the draw number and its drawn events
#' @noRd
.draw_batch <- function(batch, cells, cell_offset, panel, geography,
                        coupling, ve, maternal_months, max_doses,
                        summary_args) {
  batch_cells <- purrr::map(batch$draw, \(d) {
    dplyr::mutate(
      cells,
      cell = .data$cell + (d - 1L) * cell_offset,
      draw = d
    )
  }) |>
    purrr::list_rbind()
  batch_events <- purrr::map2(batch$draw, batch$events, \(d, events) {
    dplyr::mutate(events, cell = .data$cell + (d - 1L) * cell_offset)
  }) |>
    purrr::list_rbind()

  run <- .run_cohorts(batch_cells, batch_events, coupling, ve, max_doses)
  values <- .monthly_values(
    panel, batch_cells, run$units, run$unit_values, geography
  ) |>
    dplyr::mutate(
      susceptible = .data$susceptible_share * .data$population,
      maternal_protection = .data$age_months < maternal_months
    )
  do.call(summarise_immunity, c(list(values), summary_args))
}

# internal helper: one draw of coverage for every opportunity. routine draws
# follow an autocorrelated series over coverage years by geography and
# dose; campaigns are drawn once per coverage record. opportunities without
# an interval keep their coverage
#' @noRd
.draw_coverage <- function(events, geography, rho_years, rho_doses) {
  logit <- function(p) stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  events <- events |>
    dplyr::mutate(
      centre = logit(.data$coverage),
      spread = (logit(.data$coverage_upper) - logit(.data$coverage_lower)) /
        (2 * stats::qnorm(0.975)),
      spread = dplyr::coalesce(.data$spread, 0)
    )

  # one standard normal per geography and coverage year, shared by MCV1 and
  # MCV2 through rho_doses, and carried between years through rho_years per
  # year elapsed
  routine_z <- events |>
    dplyr::filter(.data$type != "sia") |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(geography, "coverage_year")))
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(c(geography, "coverage_year")))
    ) |>
    dplyr::mutate(
      gap = coverage_year - dplyr::lag(coverage_year),
      shared = .ar1(stats::rnorm(dplyr::n()), rho_years, gap),
      own = .ar1(stats::rnorm(dplyr::n()), rho_years, gap),
      .by = dplyr::all_of(geography)
    ) |>
    tidyr::expand_grid(type = c("mcv1", "mcv2")) |>
    dplyr::mutate(
      z = dplyr::if_else(
        .data$type == "mcv1", .data$shared,
        rho_doses * .data$shared + sqrt(1 - rho_doses^2) * .data$own
      )
    ) |>
    dplyr::select(dplyr::all_of(c(geography, "coverage_year", "type", "z")))

  campaign_z <- events |>
    dplyr::filter(.data$type == "sia") |>
    dplyr::distinct(dplyr::across(dplyr::all_of("record"))) |>
    dplyr::arrange(.data$record) |>
    dplyr::mutate(z_campaign = stats::rnorm(dplyr::n()))

  events |>
    dplyr::left_join(routine_z, by = c(geography, "coverage_year", "type")) |>
    dplyr::left_join(campaign_z, by = "record") |>
    dplyr::mutate(
      z = dplyr::coalesce(.data$z, .data$z_campaign, 0),
      coverage = dplyr::if_else(
        .data$spread == 0,
        .data$coverage,
        stats::plogis(.data$centre + .data$spread * .data$z)
      )
    ) |>
    dplyr::select(-"centre", -"spread", -"z", -"z_campaign")
}

# internal helper: a first-order autoregressive series with unit variance.
# gaps gives the time elapsed before each value (the first is ignored), so
# the correlation between neighbours is rho^gap
#' @noRd
.ar1 <- function(e, rho, gaps = NULL) {
  if (length(e) == 0) {
    return(numeric())
  }
  steps <- dplyr::tibble(
    e = e,
    gap = if (is.null(gaps)) 1 else as.numeric(gaps)
  ) |>
    dplyr::slice(-1)
  purrr::accumulate2(
    steps$e, steps$gap,
    \(previous, innovation, gap) {
      rho^gap * previous + sqrt(1 - rho^(2 * gap)) * innovation
    },
    .init = dplyr::first(e),
    .simplify = FALSE
  ) |>
    purrr::list_c(ptype = numeric())
}
