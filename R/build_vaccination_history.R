# vaccination opportunities ----------------------------------------------------

#' Build the Vaccination History of Monthly Birth Cohorts
#'
#' Lists every routine and campaign vaccination opportunity that applies to
#' each monthly birth cohort, with the birth days of the cohort that are
#' eligible, the dates over which the opportunity happens, the coverage to
#' apply and the age at the dose. The result is the input to
#' [estimate_immunity()].
#'
#' @details
#' **Cohorts.** Each cohort is a geography and birth month from
#' [reconstruct_birth_cohorts()]. Births are spread evenly over the days of
#' the birth month, so eligibility is worked out for each day of birth. An
#' opportunity row covers a run of consecutive eligible birth days,
#' `day_from` to `day_to`; `eligible_share` is the share of the cohort born
#' on those days.
#'
#' **Routine doses.** Each row of `schedule` gives a dose, the age in whole
#' months at which it is due, and the period in which the schedule applied.
#' A child is offered the dose on the date they reach that age, if the date
#' falls in any period of the schedule for that dose and age. Periods for
#' the same dose and age are merged, so a schedule split into consecutive
#' periods gives the same history as one open-ended period. Periods for the
#' same dose with different ages must not overlap. Coverage is the coverage
#' of the calendar year of the dose. When that year is not in the coverage
#' data, the nearest available year is used (the earlier year on ties); the
#' year used is kept in `coverage_year` and every substitution is listed in
#' `diagnostics`. A geography and dose with no coverage at all is listed
#' there too, and its doses are not applied.
#'
#' **Campaigns.** Each record from [prepare_campaigns()] reaches the children
#' whose age is in the target range at some time during the campaign, as in
#' [cohort_eligibility()]. Records without dates, coverage or target ages
#' are skipped and listed in `diagnostics`. Records with coverage outside 0
#' to 1, an end before the start or an invalid age range stop the function
#' with the campaign named, so they are never applied by mistake. Campaign
#' records are joined to cohorts on the `geography` columns they share, so a
#' national or admin 1 campaign applies to every cohort inside it.
#'
#' **Coverage intervals.** `coverage_lower` and `coverage_upper` are
#' optional. When given, both bounds must be finite proportions with
#' `coverage_lower <= coverage <= coverage_upper`; a row with one bound
#' only is an error.
#'
#' **Timing.** `date_from` and `date_to` are the first and last dates on
#' which children born on `day_from` to `day_to` receive the dose: the dates
#' they reach the schedule age, or the campaign days on which they are in
#' the target ages. Doses are taken as spread evenly over those dates.
#' `month` is the calendar month of the midpoint of those dates and
#' `timing_share` is the share of the dates up to the 15th of that month,
#' the date at which monthly results are reported. `age_at_dose` is the age
#' in months at the dose of the child born in the middle of the eligible
#' days, at the middle of that child's own dose dates, so it is always
#' within the target ages. [estimate_immunity()] refines the ages and the
#' timing further for each run of birth days.
#'
#' `record` identifies the coverage record behind each opportunity (a
#' geography, dose and coverage year, or a campaign record), so that
#' [immunity_draws()] draws one coverage value per record.
#'
#' @param cohorts The output of [reconstruct_birth_cohorts()], or its
#'   `cohorts` data frame.
#' @param routine_coverage Data frame of routine coverage with the
#'   `geography` columns, `year`, `dose` (as in `schedule`) and `coverage`
#'   (a proportion), and optionally `coverage_lower` and `coverage_upper`.
#' @param campaigns The output of [prepare_campaigns()], or its `campaigns`
#'   data frame. Optional.
#' @param schedule Data frame with `dose` (`"mcv1"` or `"mcv2"`),
#'   `age_months` (whole months), and optionally `start_date` and
#'   `end_date`, the period in which the schedule applied (open ended when
#'   missing).
#' @param geography Columns that identify the geography of the cohorts.
#'
#' @return A list:
#'   \describe{
#'     \item{history}{One row per cohort, opportunity and run of eligible
#'       birth days: `cell`, the `geography` columns, `birth_month`,
#'       `event_id`, `type` (`"mcv1"`, `"mcv2"` or `"sia"`), `record`,
#'       `day_from`, `day_to`, `date_from`, `date_to`, `month`,
#'       `eligible_share`, `timing_share`, `age_at_dose`, `age_months`
#'       (routine), `start_date`, `end_date`, `age_min_months` and
#'       `age_max_months` (campaigns), `coverage`, `coverage_lower`,
#'       `coverage_upper`, `coverage_year` and `coverage_source`. Typed and
#'       empty when there are no opportunities}
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
  routine_coverage <- .check_routine_coverage(routine_coverage, geography)

  cell_cols <- c(geography, "birth_month")
  cells <- panel |>
    dplyr::distinct(dplyr::across(dplyr::all_of(cell_cols))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(cell_cols))) |>
    dplyr::mutate(cell = dplyr::row_number(), .before = 1)

  routine <- .routine_events(cells, routine_coverage, schedule, geography)
  campaign <- .campaign_events(cells, campaigns, geography)

  history <- dplyr::bind_rows(
    .empty_history(cells), routine$events, campaign$events
  ) |>
    dplyr::arrange(
      .data$cell, .data$date_from, .data$type, .data$day_from
    )
  diagnostics <- dplyr::bind_rows(
    dplyr::tibble(check = character(), detail = character()),
    routine$diagnostics,
    campaign$diagnostics
  )

  list(
    history = history,
    cells = cells,
    diagnostics = diagnostics,
    metadata = list(
      births_within_month = "uniform by day",
      routine_rule = paste(
        "offered on the date the schedule age is reached, within any",
        "schedule period for that dose and age; coverage of the calendar",
        "year of the dose, or of the nearest available year (earlier on",
        "ties)"
      ),
      campaign_rule = paste(
        "eligible when in the target ages at some time during the",
        "campaign; doses spread evenly over each child's eligible campaign",
        "days; records without dates, coverage or ages are skipped"
      ),
      coverage_intervals = paste(
        "both bounds or none; bounds are finite proportions that contain",
        "the coverage"
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

# internal helper: a typed history with no rows, with the geography columns
# of the cells
#' @noRd
.empty_history <- function(cells) {
  dplyr::slice(cells, 0) |>
    dplyr::mutate(
      event_id = character(),
      type = character(),
      record = character(),
      day_from = integer(),
      day_to = integer(),
      date_from = as.Date(character()),
      date_to = as.Date(character()),
      month = as.Date(character()),
      eligible_share = numeric(),
      timing_share = numeric(),
      age_at_dose = numeric(),
      age_months = numeric(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      age_min_months = numeric(),
      age_max_months = numeric(),
      coverage = numeric(),
      coverage_lower = numeric(),
      coverage_upper = numeric(),
      coverage_year = integer(),
      coverage_source = character()
    )
}

#' @noRd
.check_schedule <- function(schedule) {
  if (!is.data.frame(schedule) ||
        !all(c("dose", "age_months") %in% names(schedule))) {
    cli::cli_abort(
      "{.arg schedule} must have columns {.val dose} and {.val age_months}."
    )
  }
  if (nrow(schedule) == 0) {
    cli::cli_abort("{.arg schedule} must have at least one dose.")
  }
  .check_raw_types(
    schedule, "schedule",
    list(dose = is.character, age_months = is.numeric)
  )
  schedule <- dplyr::as_tibble(schedule) |>
    dplyr::mutate(
      dose = tolower(as.character(.data$dose)),
      age_months = as.numeric(.data$age_months)
    )
  if (!all(schedule$dose %in% c("mcv1", "mcv2"))) {
    cli::cli_abort("{.arg schedule} doses must be {.val mcv1} or {.val mcv2}.")
  }
  if (any(!is.finite(schedule$age_months) | schedule$age_months < 0 |
            schedule$age_months %% 1 != 0)) {
    cli::cli_abort(
      "{.arg schedule} {.val age_months} must be whole months, 0 or more."
    )
  }
  schedule <- schedule |>
    dplyr::mutate(
      start_date = as.Date(.optional_column(schedule, "start_date")),
      end_date = as.Date(.optional_column(schedule, "end_date"))
    ) |>
    dplyr::mutate(
      start_date = dplyr::coalesce(.data$start_date, as.Date("1900-01-01")),
      end_date = dplyr::coalesce(.data$end_date, as.Date("2999-12-31"))
    )
  if (any(schedule$end_date < schedule$start_date)) {
    cli::cli_abort(
      "{.arg schedule} periods must not end before they start."
    )
  }

  # the same dose cannot be due at two ages in the same period
  periods <- dplyr::mutate(schedule, .row = dplyr::row_number())
  conflicts <- periods |>
    dplyr::inner_join(
      dplyr::select(
        periods, "dose", .row_b = ".row", age_b = "age_months",
        start_b = "start_date", end_b = "end_date"
      ),
      by = "dose", relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$.row < .data$.row_b,
      .data$age_months != .data$age_b,
      .data$start_date <= .data$end_b,
      .data$start_b <= .data$end_date
    )
  if (nrow(conflicts) > 0) {
    doses <- unique(conflicts$dose)
    cli::cli_abort(c(
      "{.arg schedule} has overlapping periods with different ages.",
      "x" = "Dose{?s} {.val {doses}} would be due at two ages at once."
    ))
  }
  dplyr::select(schedule, "dose", "age_months", "start_date", "end_date")
}

#' @noRd
.check_routine_coverage <- function(coverage, geography) {
  needed <- c(geography, "year", "dose", "coverage")
  if (!is.data.frame(coverage) || !all(needed %in% names(coverage))) {
    cli::cli_abort(
      "{.arg routine_coverage} needs columns {.val {needed}}."
    )
  }
  .check_raw_types(
    coverage, "routine_coverage",
    list(dose = is.character, year = is.numeric, coverage = is.numeric)
  )
  if (any(!is.finite(coverage$year) | coverage$year %% 1 != 0)) {
    cli::cli_abort("{.arg routine_coverage} {.val year} must be whole years.")
  }
  coverage <- dplyr::as_tibble(coverage) |>
    dplyr::mutate(
      dose = tolower(.data$dose),
      year = as.integer(.data$year),
      coverage = as.numeric(.data$coverage)
    )
  if (!all(coverage$dose %in% c("mcv1", "mcv2"))) {
    cli::cli_abort(
      "{.arg routine_coverage} doses must be {.val mcv1} or {.val mcv2}."
    )
  }
  if (any(!is.finite(coverage$coverage) | coverage$coverage < 0 |
            coverage$coverage > 1)) {
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
  # bounds are read from the table itself, not the masked coverage column
  bound_lower <- as.numeric(.optional_column(coverage, "coverage_lower"))
  bound_upper <- as.numeric(.optional_column(coverage, "coverage_upper"))
  routine <- coverage |>
    dplyr::mutate(
      coverage_lower = bound_lower, coverage_upper = bound_upper
    )
  routine |>
    dplyr::mutate(
      label = paste0(
        .record_key(routine, c(geography, "dose"), "routine"), " ",
        .data$year
      )
    ) |>
    .check_coverage_bounds("routine_coverage")
  routine
}

# internal helper: coverage bounds must be given together, be finite
# proportions and contain the coverage. data needs a label column naming
# each row
#' @noRd
.check_coverage_bounds <- function(data, arg) {
  invalid <- data |>
    dplyr::mutate(
      has_lower = !is.na(.data$coverage_lower),
      has_upper = !is.na(.data$coverage_upper),
      bad_values = .data$has_lower & .data$has_upper & (
        !is.finite(.data$coverage_lower) | !is.finite(.data$coverage_upper) |
          .data$coverage_lower < 0 | .data$coverage_upper > 1 |
          .data$coverage_lower > .data$coverage |
          .data$coverage_upper < .data$coverage
      )
    ) |>
    dplyr::filter(.data$has_lower != .data$has_upper | .data$bad_values)
  if (nrow(invalid) > 0) {
    labels <- utils::head(unique(invalid$label), 5)
    cli::cli_abort(c(
      paste(
        "{.arg {arg}} has invalid coverage intervals: give both bounds or",
        "none, as finite proportions with lower <= coverage <= upper."
      ),
      "x" = "{.val {labels}}"
    ))
  }
  invisible(data)
}

# internal helper: columns must have the expected raw type before any
# coercion, so factors are never turned into their codes. checks is a named
# list of predicate functions; absent columns are skipped
#' @noRd
.check_raw_types <- function(data, arg, checks) {
  wrong <- dplyr::tibble(column = names(checks), check = checks) |>
    dplyr::filter(.data$column %in% names(data)) |>
    dplyr::filter(
      !purrr::map2_lgl(
        .data$column, .data$check,
        \(column, check) check(data[[column]])
      )
    ) |>
    dplyr::pull("column")
  if (length(wrong) > 0) {
    cli::cli_abort(c(
      "{.arg {arg}} has column{?s} {.val {wrong}} of the wrong type.",
      "i" = "Use plain character or numeric columns, not factors."
    ))
  }
  invisible(data)
}

# internal helper: a column if present, otherwise NA
#' @noRd
.optional_column <- function(data, column) {
  if (column %in% names(data)) data[[column]] else NA
}

# internal helper: one text key per row from the given columns. each value
# is prefixed by its length, so values that contain the separator cannot
# collide with other combinations
#' @noRd
.record_key <- function(data, cols, prefix) {
  data |>
    dplyr::select(dplyr::all_of(cols)) |>
    purrr::pmap_chr(\(...) {
      values <- as.character(c(...))
      paste(c(prefix, paste0(nchar(values), ":", values)), collapse = "|")
    })
}

# internal helper: runs of consecutive eligible birth days, one row per run
# with day_from and day_to. days has one row per eligible day
#' @noRd
.day_runs <- function(days, keys) {
  days |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "day")))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(keys, "day")))) |>
    dplyr::mutate(
      run = cumsum(
        is.na(dplyr::lag(day)) | day != dplyr::lag(day) + 1L
      ),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::summarise(
      day_from = min(day),
      day_to = max(day),
      .by = dplyr::all_of(c(keys, "run"))
    ) |>
    dplyr::select(-"run")
}

# internal helper: birth months of the cells with their number of days, one
# row per day of birth
#' @noRd
.birth_days <- function(cells) {
  dplyr::tibble(birth_month = unique(cells$birth_month)) |>
    dplyr::mutate(n_days = .days_in_month(.month_index(.data$birth_month))) |>
    tidyr::uncount(.data$n_days, .id = "day") |>
    dplyr::mutate(birth_date = .data$birth_month + .data$day - 1L)
}

# internal helper: share of the dates from `from` to `to` (inclusive) that
# fall on or before `at`, with doses spread evenly over the dates
#' @noRd
.elapsed_share <- function(from, to, at) {
  pmin(pmax(
    as.numeric(at - from + 1) / as.numeric(to - from + 1), 0
  ), 1)
}

# internal helper: the calendar month of the midpoint of each opportunity's
# dates, and the share of the dates up to the 15th of that month
#' @noRd
.opportunity_timing <- function(data) {
  data |>
    dplyr::mutate(
      midpoint = .data$date_from +
        as.numeric(.data$date_to - .data$date_from) %/% 2,
      month = .month_from_index(.month_index(.data$midpoint)),
      timing_share = .elapsed_share(
        .data$date_from, .data$date_to, .data$month + 14L
      )
    ) |>
    dplyr::select(-"midpoint")
}

# internal helper: exact age in months, as completed months plus the
# fraction of the current month of age. a child born on the 31st reaches
# one month on the last day of the next month
#' @noRd
.age_in_months <- function(birth_date, date) {
  whole <- .month_index(date) - .month_index(birth_date)
  completed <- dplyr::if_else(
    .add_months(birth_date, whole) <= date, whole, whole - 1L
  )
  lower <- .add_months(birth_date, completed)
  upper <- .add_months(birth_date, completed + 1L)
  completed + as.numeric(date - lower) / as.numeric(upper - lower)
}

# internal helper: age at a campaign dose of a child born on birth_date,
# taken at the middle of the campaign days on which the child is in the
# target ages, so it is always within the target ages
#' @noRd
.campaign_age <- function(birth_date, start, end, age_min, age_max) {
  first <- pmax(start, .add_months(birth_date, age_min))
  last <- pmin(end, .add_months(birth_date, age_max) - 1L)
  .age_in_months(birth_date, first + as.numeric(last - first) %/% 2)
}

# internal helper: routine opportunities. eligibility is the same for every
# cohort born in a month, so it is worked out once per birth month
#' @noRd
.routine_events <- function(cells, coverage, schedule, geography) {
  ranges <- tidyr::expand_grid(schedule, .birth_days(cells)) |>
    dplyr::mutate(
      dose_date = .add_months(.data$birth_date, .data$age_months)
    ) |>
    dplyr::filter(
      .data$dose_date >= .data$start_date, .data$dose_date <= .data$end_date
    ) |>
    .day_runs(c("birth_month", "dose", "age_months"))
  if (nrow(ranges) == 0) {
    return(list(events = NULL, diagnostics = NULL))
  }
  ranges <- ranges |>
    dplyr::mutate(
      n_days = .days_in_month(.month_index(.data$birth_month)),
      date_from = .add_months(
        .data$birth_month + .data$day_from - 1L, .data$age_months
      ),
      date_to = .add_months(
        .data$birth_month + .data$day_to - 1L, .data$age_months
      )
    ) |>
    .opportunity_timing()

  events <- cells |>
    dplyr::inner_join(
      ranges, by = "birth_month", relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      eligible_share = (.data$day_to - .data$day_from + 1) / .data$n_days,
      year = as.integer(format(.data$month, "%Y"))
    )

  # the nearest coverage year for each geography, dose and year of the dose
  needed <- events |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(geography, "dose", "year"))))
  available <- coverage |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(geography, "dose"))),
      coverage_year = .data$year
    )
  matched <- needed |>
    dplyr::left_join(
      available, by = c(geography, "dose"), relationship = "many-to-many"
    ) |>
    dplyr::mutate(gap = abs(.data$coverage_year - .data$year)) |>
    dplyr::arrange(.data$gap, .data$coverage_year) |>
    dplyr::slice_head(
      n = 1, by = dplyr::all_of(c(geography, "dose", "year"))
    ) |>
    dplyr::select(-"gap")

  no_coverage <- matched |>
    dplyr::filter(is.na(.data$coverage_year)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(geography, "dose")))) |>
    dplyr::mutate(
      check = "missing_routine_coverage",
      detail = paste(
        "no", .data$dose, "coverage for this geography; doses not applied"
      )
    )
  substituted <- matched |>
    dplyr::filter(
      !is.na(.data$coverage_year), .data$coverage_year != .data$year
    ) |>
    dplyr::mutate(
      check = "coverage_year_substituted",
      detail = paste0(
        "no ", .data$dose, " coverage for ", .data$year, "; used ",
        .data$coverage_year
      )
    )

  events <- events |>
    dplyr::inner_join(
      dplyr::filter(matched, !is.na(.data$coverage_year)),
      by = c(geography, "dose", "year")
    ) |>
    dplyr::left_join(
      coverage |>
        dplyr::select(
          dplyr::all_of(geography), "dose", coverage_year = "year",
          "coverage", "coverage_lower", "coverage_upper"
        ),
      by = c(geography, "dose", "coverage_year")
    )
  events <- events |>
    dplyr::mutate(
      record = .record_key(
        events, c("dose", "coverage_year", geography), "routine"
      )
    ) |>
    dplyr::transmute(
      .data$cell,
      dplyr::across(dplyr::all_of(geography)),
      .data$birth_month,
      event_id = .data$dose,
      type = .data$dose,
      .data$record,
      .data$day_from,
      .data$day_to,
      .data$date_from,
      .data$date_to,
      .data$month,
      .data$eligible_share,
      .data$timing_share,
      age_at_dose = .data$age_months,
      .data$age_months,
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      age_min_months = NA_real_,
      age_max_months = NA_real_,
      .data$coverage,
      .data$coverage_lower,
      .data$coverage_upper,
      .data$coverage_year,
      coverage_source = paste("routine coverage", .data$coverage_year)
    )

  list(
    events = events,
    diagnostics = dplyr::bind_rows(no_coverage, substituted)
  )
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
  .check_raw_types(
    campaigns, "campaigns",
    list(
      campaign_id = is.character, age_min_months = is.numeric,
      age_max_months = is.numeric, coverage = is.numeric,
      coverage_lower = is.numeric, coverage_upper = is.numeric,
      start_date = \(x) inherits(x, "Date"),
      end_date = \(x) inherits(x, "Date")
    )
  )
  campaigns <- dplyr::as_tibble(campaigns)
  repeated <- campaigns |>
    dplyr::count(dplyr::across(dplyr::all_of(c(join_cols, "campaign_id")))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(repeated) > 0) {
    cli::cli_abort(c(
      "{.arg campaigns} has more than one record per campaign and geography.",
      "x" = "Campaign{?s} {.val {unique(repeated$campaign_id)}}",
      "i" = "Each record would be applied as a separate campaign."
    ))
  }
  campaigns <- campaigns |>
    dplyr::mutate(
      campaign_id = as.character(.data$campaign_id),
      start_date = as.Date(.data$start_date),
      end_date = as.Date(.data$end_date),
      age_min_months = as.numeric(.data$age_min_months),
      age_max_months = as.numeric(.data$age_max_months),
      coverage = as.numeric(.data$coverage),
      coverage_lower = as.numeric(
        .optional_column(campaigns, "coverage_lower")
      ),
      coverage_upper = as.numeric(
        .optional_column(campaigns, "coverage_upper")
      ),
      coverage_source = as.character(
        .optional_column(campaigns, "coverage_source")
      )
    )

  campaigns <- campaigns |>
    dplyr::mutate(
      usable = !is.na(start_date) & !is.na(end_date) &
        !is.na(coverage) & !is.na(age_min_months) &
        !is.na(age_max_months)
    )
  skipped <- campaigns |>
    dplyr::filter(!usable) |>
    dplyr::summarise(n_records = dplyr::n(), .by = "campaign_id") |>
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

  # records that cannot be applied stop here, named, rather than being
  # dropped or reaching the engine
  campaigns <- campaigns |>
    dplyr::mutate(
      label = paste0(
        .data$campaign_id, " (", .record_key(campaigns, join_cols, "sia"), ")"
      )
    )
  invalid <- campaigns |>
    dplyr::mutate(
      reason = dplyr::case_when(
        !is.finite(.data$coverage) | .data$coverage < 0 |
          .data$coverage > 1 ~ "coverage is not a proportion",
        .data$end_date < .data$start_date ~ "end date before start date",
        !is.finite(.data$age_min_months) | !is.finite(.data$age_max_months) |
          .data$age_min_months < 0 |
          .data$age_min_months %% 1 != 0 | .data$age_max_months %% 1 != 0 |
          .data$age_max_months <= .data$age_min_months ~
          "target ages must be whole months with max above min",
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$reason))
  if (nrow(invalid) > 0) {
    problems <- utils::head(paste(invalid$label, invalid$reason, sep = ": "), 5)
    cli::cli_abort(c(
      "Campaign records cannot be applied; fix them before building the
       history.",
      "x" = "{.val {problems}}"
    ))
  }
  .check_coverage_bounds(campaigns, "campaigns")

  # eligible birth days depend only on the campaign dates and target ages
  definition_cols <- c(
    "start_date", "end_date", "age_min_months", "age_max_months"
  )
  definitions <- dplyr::distinct(
    campaigns, dplyr::across(dplyr::all_of(definition_cols))
  )
  ranges <- tidyr::expand_grid(definitions, .birth_days(cells)) |>
    dplyr::filter(
      .add_months(.data$birth_date, .data$age_min_months) <= .data$end_date,
      .add_months(.data$birth_date, .data$age_max_months) > .data$start_date
    ) |>
    .day_runs(c(definition_cols, "birth_month"))

  events <- cells |>
    dplyr::inner_join(
      campaigns |>
        dplyr::select(
          dplyr::all_of(c(join_cols, definition_cols)), "campaign_id",
          "coverage", "coverage_lower", "coverage_upper", "coverage_source"
        ),
      by = join_cols,
      relationship = "many-to-many"
    ) |>
    dplyr::inner_join(ranges, by = c("birth_month", definition_cols)) |>
    dplyr::mutate(
      n_days = .days_in_month(.month_index(.data$birth_month)),
      eligible_share = (.data$day_to - .data$day_from + 1) / .data$n_days,
      date_from = pmax(
        .data$start_date,
        .add_months(
          .data$birth_month + .data$day_from - 1L, .data$age_min_months
        )
      ),
      date_to = pmin(
        .data$end_date,
        .add_months(
          .data$birth_month + .data$day_to - 1L, .data$age_max_months
        ) - 1L
      ),
      age_at_dose = .campaign_age(
        .data$birth_month + (.data$day_from + .data$day_to) %/% 2L - 1L,
        .data$start_date, .data$end_date, .data$age_min_months,
        .data$age_max_months
      )
    ) |>
    .opportunity_timing()
  events <- events |>
    dplyr::mutate(
      record = .record_key(events, c("campaign_id", join_cols), "sia")
    ) |>
    dplyr::transmute(
      .data$cell,
      dplyr::across(dplyr::all_of(geography)),
      .data$birth_month,
      event_id = .data$campaign_id,
      type = "sia",
      .data$record,
      .data$day_from,
      .data$day_to,
      .data$date_from,
      .data$date_to,
      .data$month,
      .data$eligible_share,
      .data$timing_share,
      .data$age_at_dose,
      age_months = NA_real_,
      .data$start_date,
      .data$end_date,
      .data$age_min_months,
      .data$age_max_months,
      .data$coverage,
      .data$coverage_lower,
      .data$coverage_upper,
      coverage_year = NA_integer_,
      .data$coverage_source
    )

  list(events = events, diagnostics = skipped)
}
