# vaccination opportunities ----------------------------------------------------

#' Build the Vaccination History of Monthly Birth Cohorts
#'
#' Lists every routine and campaign vaccination opportunity that applies to
#' each monthly birth cohort, with the share of the cohort that is eligible,
#' the coverage to apply and the age at the dose. The result is the input to
#' [estimate_immunity()].
#'
#' @details
#' **Cohorts.** Each cohort is a geography and birth month from
#' [reconstruct_birth_cohorts()]. Births are spread evenly over the days of
#' the birth month, so eligibility is worked out for each day of birth.
#'
#' **Routine doses.** Each row of `schedule` gives a dose, the age in months
#' at which it is due, and the period in which the schedule applied. A child
#' is offered the dose on the date they reach that age, if the date falls in
#' the schedule period; `eligible_share` is the share of the cohort for whom
#' it does. Coverage is the coverage of the calendar year of the dose. When
#' that year is outside the coverage data, the nearest year is used and the
#' substitution is listed in `diagnostics`.
#'
#' **Campaigns.** Each record from [prepare_campaigns()] reaches the share
#' of each cohort whose age is in the target range at some time during the
#' campaign, from [cohort_eligibility()]. Records without dates, coverage or
#' target ages are skipped and listed in `diagnostics`. Campaign records are
#' joined to cohorts on the `geography` columns they share, so a national or
#' admin 1 campaign applies to every cohort inside it.
#'
#' **Timing.** `month` is the calendar month of the opportunity: the month
#' the cohort reaches the schedule age, or the month of the campaign
#' midpoint. `timing_share` is the share of the cohort's opportunity that
#' has happened by the 15th of that month, the date at which monthly results
#' are reported. `age_at_dose` is the age in months of the middle of the
#' cohort at the dose, used for vaccine effectiveness.
#'
#' @param cohorts The output of [reconstruct_birth_cohorts()], or its
#'   `cohorts` data frame.
#' @param routine_coverage Data frame of routine coverage with the
#'   `geography` columns, `year`, `dose` (as in `schedule`) and `coverage`
#'   (a proportion), and optionally `coverage_lower` and `coverage_upper`.
#' @param campaigns The output of [prepare_campaigns()], or its `campaigns`
#'   data frame. Optional.
#' @param schedule Data frame with `dose` (`"mcv1"` or `"mcv2"`),
#'   `age_months`, and optionally `start_date` and `end_date`, the period in
#'   which the schedule applied (open ended when missing).
#' @param geography Columns that identify the geography of the cohorts.
#'
#' @return A list:
#'   \describe{
#'     \item{history}{One row per cohort and opportunity: `cell`, the
#'       `geography` columns, `birth_month`, `event_id`, `type` (`"mcv1"`,
#'       `"mcv2"` or `"sia"`), `month`, `eligible_share`, `timing_share`,
#'       `age_at_dose`, `coverage`, `coverage_lower`, `coverage_upper` and
#'       `coverage_source`}
#'     \item{cells}{One row per cohort: `cell`, the `geography` columns and
#'       `birth_month`}
#'     \item{diagnostics}{One row per problem: `check`, `detail` and the
#'       records concerned}
#'     \item{metadata}{The rules used}
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
#' schedule <- data.frame(dose = "mcv1", age_months = 9)
#' history <- build_vaccination_history(
#'   cohorts, coverage, schedule = schedule, geography = "adm1"
#' )
#' head(history$history)
#' @export
build_vaccination_history <- function(
  cohorts,
  routine_coverage,
  campaigns = NULL,
  schedule,
  geography
) {
  panel <- .cohort_panel_input(cohorts, geography)
  schedule <- .check_schedule(schedule)
  routine_coverage <- .check_routine_coverage(
    routine_coverage, geography, schedule
  )

  cell_cols <- c(geography, "birth_month")
  cells <- panel |>
    dplyr::distinct(dplyr::across(dplyr::all_of(cell_cols))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(cell_cols))) |>
    dplyr::mutate(cell = dplyr::row_number(), .before = 1)

  routine <- .routine_events(cells, routine_coverage, schedule, geography)
  campaign <- .campaign_events(cells, campaigns, geography)

  history <- dplyr::bind_rows(routine$events, campaign$events) |>
    dplyr::arrange(.data$cell, .data$month, .data$type)
  diagnostics <- dplyr::bind_rows(routine$diagnostics, campaign$diagnostics)

  list(
    history = history,
    cells = cells,
    diagnostics = diagnostics,
    metadata = list(
      births_within_month = "uniform by day",
      routine_rule = paste(
        "offered on the date the schedule age is reached, within the",
        "schedule period; coverage of the calendar year of the dose"
      ),
      campaign_rule = paste(
        "eligible when in the target ages at some time during the",
        "campaign; records without dates, coverage or ages are skipped"
      ),
      reporting_date = "15th of each month"
    )
  )
}

# internal helper: the cohorts data frame, checked
#' @noRd
.cohort_panel_input <- function(cohorts, geography) {
  if (is.list(cohorts) && !is.data.frame(cohorts)) {
    cohorts <- cohorts$cohorts
  }
  needed <- c(geography, "birth_month", "month", "age_months", "population")
  if (!is.data.frame(cohorts) || !all(needed %in% names(cohorts))) {
    cli::cli_abort(c(
      "{.arg cohorts} must be the output of {.fn reconstruct_birth_cohorts}.",
      "i" = "It needs columns {.val {needed}}."
    ))
  }
  cohorts
}

#' @noRd
.check_schedule <- function(schedule) {
  if (!is.data.frame(schedule) ||
        !all(c("dose", "age_months") %in% names(schedule))) {
    cli::cli_abort(
      "{.arg schedule} must have columns {.val dose} and {.val age_months}."
    )
  }
  schedule <- dplyr::as_tibble(schedule) |>
    dplyr::mutate(dose = tolower(.data$dose))
  if (!all(schedule$dose %in% c("mcv1", "mcv2"))) {
    cli::cli_abort("{.arg schedule} doses must be {.val mcv1} or {.val mcv2}.")
  }
  schedule |>
    dplyr::mutate(
      start_date = as.Date(.optional_column(schedule, "start_date")),
      end_date = as.Date(.optional_column(schedule, "end_date"))
    ) |>
    dplyr::mutate(
      start_date = dplyr::coalesce(.data$start_date, as.Date("1900-01-01")),
      end_date = dplyr::coalesce(.data$end_date, as.Date("2999-12-31"))
    )
}

#' @noRd
.check_routine_coverage <- function(coverage, geography, schedule) {
  needed <- c(geography, "year", "dose", "coverage")
  if (!is.data.frame(coverage) || !all(needed %in% names(coverage))) {
    cli::cli_abort(
      "{.arg routine_coverage} needs columns {.val {needed}}."
    )
  }
  coverage <- dplyr::as_tibble(coverage) |>
    dplyr::mutate(dose = tolower(.data$dose), year = as.integer(.data$year))
  if (any(!is.finite(coverage$coverage)) ||
        any(coverage$coverage < 0 | coverage$coverage > 1)) {
    cli::cli_abort(c(
      "Routine coverage must be a proportion between 0 and 1.",
      "i" = "Missing coverage is not zero; drop those rows instead."
    ))
  }
  repeated <- coverage |>
    dplyr::count(dplyr::across(dplyr::all_of(c(geography, "year", "dose")))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(repeated) > 0) {
    cli::cli_abort(
      paste(
        "{.arg routine_coverage} has more than one row per geography,",
        "year and dose."
      )
    )
  }
  routine <- coverage
  routine |>
    dplyr::mutate(
      coverage_lower = .optional_column(routine, "coverage_lower"),
      coverage_upper = .optional_column(routine, "coverage_upper")
    )
}

# internal helper: a column if present, otherwise NA
#' @noRd
.optional_column <- function(data, column) {
  if (column %in% names(data)) data[[column]] else NA
}

# internal helper: routine opportunities. the day-of-birth arithmetic is the
# same for every cohort born in a month, so it is done once per birth month
#' @noRd
.routine_events <- function(cells, coverage, schedule, geography) {
  timing <- tidyr::expand_grid(
    schedule,
    birth_month = unique(cells$birth_month)
  ) |>
    dplyr::mutate(
      birth_date = purrr::map(
        .data$birth_month,
        \(bm) bm + seq_len(.days_in_month(.month_index(bm))) - 1L
      )
    ) |>
    tidyr::unnest("birth_date") |>
    dplyr::mutate(
      dose_date = .add_months(.data$birth_date, .data$age_months),
      in_period = .data$dose_date >= .data$start_date &
        .data$dose_date <= .data$end_date
    ) |>
    dplyr::summarise(
      eligible_share = mean(.data$in_period),
      month = .month_from_index(.month_index(min(.data$dose_date))),
      timing_share = mean(
        .data$dose_date[.data$in_period] <= .data$month[1] + 14L
      ),
      .by = c("birth_month", "dose", "age_months")
    ) |>
    dplyr::filter(.data$eligible_share > 0) |>
    dplyr::rename(age_at_dose = "age_months")

  if (nrow(timing) == 0) {
    return(list(events = NULL, diagnostics = NULL))
  }

  events <- cells |>
    dplyr::inner_join(
      timing,
      by = "birth_month", relationship = "many-to-many"
    ) |>
    dplyr::mutate(year = as.integer(format(.data$month, "%Y")))

  # nearest year of coverage for each geography and dose
  available <- coverage |>
    dplyr::summarise(
      first_year = min(.data$year), last_year = max(.data$year),
      .by = dplyr::all_of(c(geography, "dose"))
    )
  events <- events |>
    dplyr::left_join(available, by = c(geography, "dose")) |>
    dplyr::mutate(
      coverage_year = pmin(pmax(.data$year, .data$first_year), .data$last_year)
    )

  no_coverage <- events |>
    dplyr::filter(is.na(.data$coverage_year)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(geography, "dose"))))
  substituted <- events |>
    dplyr::filter(
      !is.na(.data$coverage_year),
      .data$year != .data$coverage_year
    ) |>
    dplyr::distinct(dplyr::across(
      dplyr::all_of(c(geography, "dose", "year", "coverage_year"))
    ))

  events <- events |>
    dplyr::filter(!is.na(.data$coverage_year)) |>
    dplyr::left_join(
      coverage |>
        dplyr::select(
          dplyr::all_of(geography), "dose", coverage_year = "year",
          "coverage", "coverage_lower", "coverage_upper"
        ),
      by = c(geography, "dose", "coverage_year")
    ) |>
    dplyr::filter(!is.na(.data$coverage)) |>
    dplyr::transmute(
      .data$cell,
      dplyr::across(dplyr::all_of(geography)),
      .data$birth_month,
      event_id = .data$dose,
      type = .data$dose,
      .data$month,
      .data$eligible_share,
      .data$timing_share,
      .data$age_at_dose,
      .data$coverage,
      .data$coverage_lower,
      .data$coverage_upper,
      coverage_source = paste("routine coverage", .data$coverage_year)
    )

  diagnostics <- dplyr::bind_rows(
    if (nrow(no_coverage) > 0) {
      no_coverage |>
        dplyr::mutate(
          check = "missing_routine_coverage",
          detail = paste("no", .data$dose, "coverage for this geography")
        )
    },
    if (nrow(substituted) > 0) {
      substituted |>
        dplyr::summarise(
          detail = paste0(
            "years ", min(.data$year), "-", max(.data$year),
            " use the nearest coverage year"
          ),
          .by = dplyr::all_of(c(geography, "dose"))
        ) |>
        dplyr::mutate(check = "coverage_year_substituted")
    }
  )

  list(events = events, diagnostics = diagnostics)
}

# internal helper: campaign opportunities from prepare_campaigns() records
#' @noRd
.campaign_events <- function(cells, campaigns, geography) {
  if (is.null(campaigns)) {
    return(list(events = NULL, diagnostics = NULL))
  }
  if (is.list(campaigns) && !is.data.frame(campaigns)) {
    campaigns <- campaigns$campaigns
  }
  needed <- c(
    "campaign_id", "start_date", "end_date", "age_min_months",
    "age_max_months", "coverage"
  )
  if (!is.data.frame(campaigns) || !all(needed %in% names(campaigns))) {
    cli::cli_abort(
      "{.arg campaigns} must be the output of {.fn prepare_campaigns}."
    )
  }
  join_cols <- intersect(geography, names(campaigns))
  if (length(join_cols) == 0) {
    cli::cli_abort(
      "{.arg campaigns} shares no geography columns with the cohorts."
    )
  }
  campaigns <- campaigns |>
    dplyr::mutate(
      coverage_lower = .optional_column(campaigns, "coverage_lower"),
      coverage_upper = .optional_column(campaigns, "coverage_upper"),
      coverage_source = .optional_column(campaigns, "coverage_source")
    )

  campaigns <- campaigns |>
    dplyr::mutate(
      usable = !is.na(.data$start_date) & !is.na(.data$end_date) &
        !is.na(.data$coverage) & !is.na(.data$age_min_months) &
        !is.na(.data$age_max_months)
    )
  skipped <- campaigns |>
    dplyr::filter(!.data$usable) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      .by = "campaign_id"
    ) |>
    dplyr::mutate(
      check = "campaign_skipped",
      detail = paste(
        .data$n_records,
        "record(s) without dates, coverage or target ages were not applied"
      )
    ) |>
    dplyr::select(-"n_records")

  campaigns <- dplyr::filter(campaigns, .data$usable)
  if (nrow(campaigns) == 0) {
    return(list(events = NULL, diagnostics = skipped))
  }

  campaigns <- campaigns |>
    dplyr::mutate(
      midpoint = .data$start_date +
        as.integer((.data$end_date - .data$start_date) / 2),
      month = .month_from_index(.month_index(.data$midpoint)),
      timing_share = pmin(pmax(
        as.numeric(.data$month + 14L - .data$start_date + 1) /
          as.numeric(.data$end_date - .data$start_date + 1),
        0
      ), 1)
    )

  events <- cells |>
    dplyr::inner_join(
      campaigns |>
        dplyr::select(
          dplyr::all_of(join_cols), "campaign_id", "start_date", "end_date",
          "midpoint", "month", "timing_share", "age_min_months",
          "age_max_months", "coverage", "coverage_lower", "coverage_upper",
          "coverage_source"
        ),
      by = join_cols,
      relationship = "many-to-many"
    )

  events <- events |>
    dplyr::mutate(
      eligible_share = purrr::pmap_dbl(
        list(
          .data$birth_month, .data$start_date, .data$end_date,
          .data$age_min_months, .data$age_max_months
        ),
        \(bm, start, end, lo, hi) cohort_eligibility(bm, start, end, lo, hi)
      )
    ) |>
    dplyr::filter(.data$eligible_share > 0) |>
    dplyr::transmute(
      .data$cell,
      dplyr::across(dplyr::all_of(geography)),
      .data$birth_month,
      event_id = .data$campaign_id,
      type = "sia",
      .data$month,
      .data$eligible_share,
      .data$timing_share,
      age_at_dose = as.numeric(.data$midpoint - (.data$birth_month + 14L)) /
        (365.25 / 12),
      .data$coverage,
      .data$coverage_lower,
      .data$coverage_upper,
      .data$coverage_source
    )

  list(events = events, diagnostics = skipped)
}
