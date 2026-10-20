# campaign records -------------------------------------------------------------

#' Prepare Vaccination Campaign Records
#'
#' Brings campaign calendars, post-campaign survey results and coverage
#' reported by the campaign into one standard set of campaign records, one
#' per geography and campaign, ready to be applied to birth cohorts. Survey
#' indicators are kept in a separate evaluation table, and every problem
#' found is listed in a diagnostics table.
#'
#' @details
#' **Records.** A record is made for every geography and campaign that
#' appears in `calendar`, `evaluation` or `reported_coverage`. Dates and
#' target ages come from `calendar`. Records without dates are kept, with
#' `needs_dates = TRUE`, so they can be completed by hand; dates are never
#' inferred.
#'
#' **Target ages.** Ages are converted to whole months, with the upper bound
#' exclusive: `"9-59 months"` becomes 9 to 60 and `"9 months-14 years"`
#' becomes 9 to 180. Text such as `"9 to 59 months"` or `"9 and 59 months"`
#' is also read. Alternatively, `calendar` can give `age_min_months` and
#' `age_max_months` directly.
#'
#' **Coverage.** Survey coverage (the `coverage_indicator` rows of
#' `evaluation`) is used where available. Otherwise, coverage reported by
#' the campaign is used and labelled as such in `coverage_source`. Records
#' with neither keep `NA` coverage and `needs_coverage = TRUE`; no value is
#' imputed. Coverage is returned as a proportion.
#'
#' **Evaluation.** All survey indicators, including coverage, are returned
#' in `evaluation` as proportions with their intervals and sample sizes, for
#' example first-dose receipt, MCV before the campaign and the distribution
#' of lifetime doses. They are kept for calibrating and validating
#' assumptions about how campaign doses overlap with earlier doses.
#'
#' **Diagnostics.** Each row is one problem: `missing_dates`,
#' `end_before_start`, `missing_age`, `invalid_age`, `missing_coverage`,
#' `coverage_out_of_range` (outside 0-100%), `unmatched_geography` (missing
#' identifiers, or units not in `boundaries`), `duplicate_record` and
#' `overlapping_campaigns` (two campaigns in the same geography whose dates
#' overlap).
#'
#' @param calendar Data frame with the `geography` columns, `campaign_id`,
#'   `start_date`, `end_date` (Dates, `NA` when unknown) and either
#'   `target_age` (text) or `age_min_months` and `age_max_months` (upper
#'   bound exclusive). Optional columns `source` and `note` are kept.
#' @param evaluation Optional survey results in long form, with the
#'   `geography` columns, `campaign_id`, `indicator`, `value_pct`, and
#'   optionally `lower_pct`, `upper_pct`, `n` and `survey` (the survey name,
#'   kept as the source reference).
#' @param reported_coverage Optional coverage reported by the campaign, with
#'   the `geography` columns, `campaign_id`, `coverage_pct` and optionally
#'   `source`.
#' @param geography Columns that identify the geography in every input.
#' @param boundaries Optional data frame or `sf` object with the
#'   `geography` columns, such as the output of [get_boundaries()], used to
#'   flag units that are not in the boundaries.
#' @param coverage_indicator Name of the indicator in `evaluation` that
#'   holds campaign coverage. Default `"sia_coverage"`.
#'
#' @return A list:
#'   \describe{
#'     \item{campaigns}{One row per geography and campaign: the `geography`
#'       columns, `campaign_id`, `start_date`, `end_date`, `target_age`,
#'       `age_min_months`, `age_max_months`, `coverage`, `coverage_lower`,
#'       `coverage_upper`, `coverage_source`, `coverage_reference`,
#'       `dates_source`, `note`, `needs_dates` and `needs_coverage`}
#'     \item{evaluation}{Survey indicators as proportions: the `geography`
#'       columns, `campaign_id`, `survey`, `indicator`, `value`, `lower`,
#'       `upper` and `n`}
#'     \item{diagnostics}{One row per problem: `check`, `campaign_id`, the
#'       `geography` columns and `detail`}
#'     \item{metadata}{The rules and assumptions used}
#'   }
#'
#' @examples
#' calendar <- data.frame(
#'   adm1 = c("Kano", "Lagos"),
#'   campaign_id = "NGA-MSL-2024",
#'   start_date = as.Date(c("2024-11-25", "2024-10-26")),
#'   end_date = as.Date(c("2024-12-03", "2024-11-03")),
#'   target_age = "9-59 months"
#' )
#' evaluation <- data.frame(
#'   adm1 = c("Kano", "Lagos", "Kano"),
#'   campaign_id = "NGA-MSL-2024",
#'   survey = "IPCCS",
#'   indicator = c("sia_coverage", "sia_coverage", "sia_first_dose"),
#'   value_pct = c(88.1, 79.4, 12.3)
#' )
#' out <- prepare_campaigns(calendar, evaluation, geography = "adm1")
#' out$campaigns
#' out$diagnostics
#' @export
prepare_campaigns <- function(
  calendar,
  evaluation = NULL,
  reported_coverage = NULL,
  geography,
  boundaries = NULL,
  coverage_indicator = "sia_coverage"
) {
  calendar <- .check_campaign_input(
    calendar, "calendar", c(geography, "campaign_id", "start_date", "end_date")
  )
  if (!is.null(evaluation)) {
    evaluation <- .check_campaign_input(
      evaluation, "evaluation",
      c(geography, "campaign_id", "indicator", "value_pct")
    )
  }
  if (!is.null(reported_coverage)) {
    reported_coverage <- .check_campaign_input(
      reported_coverage, "reported_coverage",
      c(geography, "campaign_id", "coverage_pct")
    )
  }

  standardised <- .standardise_calendar(calendar, geography)
  calendar <- standardised$calendar
  evaluation_out <- .standardise_evaluation(evaluation, geography)

  key_cols <- c(geography, "campaign_id")
  duplicate_issues <- dplyr::bind_rows(
    .flag_duplicates(calendar, key_cols, "calendar", geography),
    if (!is.null(evaluation)) {
      .flag_duplicates(
        evaluation_out, c(key_cols, "survey", "indicator"), "evaluation",
        geography
      )
    },
    if (!is.null(reported_coverage)) {
      .flag_duplicates(
        reported_coverage, key_cols, "reported_coverage", geography
      )
    }
  )

  campaigns <- .campaign_skeleton(
    calendar, evaluation_out, reported_coverage, key_cols
  ) |>
    .attach_calendar(calendar, key_cols) |>
    .attach_coverage(
      evaluation_out, reported_coverage, key_cols, coverage_indicator
    ) |>
    dplyr::mutate(
      needs_dates = is.na(.data$start_date) | is.na(.data$end_date),
      needs_coverage = is.na(.data$coverage)
    )

  diagnostics <- dplyr::bind_rows(
    standardised$issues,
    duplicate_issues,
    .flag_campaign_gaps(campaigns, geography),
    .flag_coverage_range(campaigns, evaluation_out, geography),
    .flag_geography(campaigns, geography, boundaries),
    .flag_overlaps(campaigns, geography)
  )

  campaigns <- campaigns |>
    dplyr::arrange(
      .data$campaign_id, dplyr::pick(dplyr::all_of(geography))
    ) |>
    dplyr::select(dplyr::all_of(c(
      geography, "campaign_id", "start_date", "end_date", "target_age",
      "age_min_months", "age_max_months", "coverage", "coverage_lower",
      "coverage_upper", "coverage_source", "coverage_reference",
      "dates_source", "note", "needs_dates", "needs_coverage"
    )))

  list(
    campaigns = campaigns,
    evaluation = evaluation_out,
    diagnostics = diagnostics,
    metadata = list(
      coverage_indicator = coverage_indicator,
      coverage_rule = paste(
        "survey coverage where available, otherwise coverage reported by",
        "the campaign; missing coverage is not imputed"
      ),
      dates_rule = "dates from the calendar only; missing dates are flagged",
      age_rule = "whole months, upper bound exclusive",
      scale = "coverage and evaluation values are proportions"
    )
  )
}

# internal helper: check an input is a data frame with the needed columns
#' @noRd
.check_campaign_input <- function(x, arg, needed) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame.")
  }
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  x <- dplyr::as_tibble(x)
  missing_cols <- setdiff(needed, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "{.arg {arg}} needs {cli::qty(missing_cols)}column{?s} ",
      "{.val {missing_cols}}."
    ))
  }
  x
}

# internal helper: rows with a detail column as diagnostics rows
#' @noRd
.as_issues <- function(rows, check, geography) {
  check_name <- check
  rows |>
    dplyr::mutate(check = check_name) |>
    dplyr::select(dplyr::all_of(c("check", "campaign_id", geography, "detail")))
}

# internal helper: dates as Date, target ages as month bounds. returns the
# calendar and its issues
#' @noRd
.standardise_calendar <- function(calendar, geography) {
  missing_text <- setdiff(c("target_age", "source", "note"), names(calendar))
  text_cols <- purrr::map(purrr::set_names(missing_text), \(col) NA_character_)
  has_bounds <- all(c("age_min_months", "age_max_months") %in% names(calendar))

  calendar <- calendar |>
    dplyr::mutate(
      !!!text_cols,
      start_date = as.Date(.data$start_date),
      end_date = as.Date(.data$end_date)
    )
  parsed <- .parse_target_age(calendar$target_age)
  calendar <- dplyr::bind_cols(
    calendar,
    dplyr::tibble(.min_parsed = parsed$min, .max_parsed = parsed$max)
  )

  # month bounds given in the calendar take precedence over the text
  if (has_bounds) {
    calendar <- calendar |>
      dplyr::mutate(
        .min_given = as.numeric(.data$age_min_months),
        .max_given = as.numeric(.data$age_max_months)
      )
  } else {
    calendar <- calendar |>
      dplyr::mutate(.min_given = NA_real_, .max_given = NA_real_)
  }

  calendar <- calendar |>
    dplyr::mutate(
      .given = !is.na(.data$.min_given) & !is.na(.data$.max_given),
      age_min_months = dplyr::if_else(
        .data$.given, .data$.min_given, .data$.min_parsed
      ),
      age_max_months = dplyr::if_else(
        .data$.given, .data$.max_given, .data$.max_parsed
      ),
      .unreadable = !is.na(.data$target_age) & is.na(.data$.min_parsed) &
        !.data$.given,
      .bad_range = !is.na(.data$age_min_months) &
        !is.na(.data$age_max_months) &
        (.data$age_min_months < 0 |
           .data$age_max_months <= .data$age_min_months),
      .backwards = !is.na(.data$start_date) & !is.na(.data$end_date) &
        .data$end_date < .data$start_date
    )

  issues <- dplyr::bind_rows(
    calendar |>
      dplyr::filter(.data$.unreadable) |>
      dplyr::mutate(
        detail = paste0("target age not read: ", .data$target_age)
      ) |>
      .as_issues("invalid_age", geography),
    calendar |>
      dplyr::filter(.data$.bad_range) |>
      dplyr::mutate(
        detail = paste0(
          "age range ", .data$age_min_months, " to ", .data$age_max_months,
          " months"
        )
      ) |>
      .as_issues("invalid_age", geography),
    calendar |>
      dplyr::filter(.data$.backwards) |>
      dplyr::mutate(
        detail = paste(.data$start_date, "to", .data$end_date)
      ) |>
      .as_issues("end_before_start", geography)
  )

  calendar <- calendar |>
    dplyr::mutate(
      age_min_months = dplyr::if_else(
        .data$.bad_range, NA_real_, .data$age_min_months
      ),
      age_max_months = dplyr::if_else(
        .data$.bad_range, NA_real_, .data$age_max_months
      )
    ) |>
    dplyr::select(-dplyr::all_of(c(
      ".min_parsed", ".max_parsed", ".min_given", ".max_given", ".given",
      ".unreadable", ".bad_range", ".backwards"
    )))

  list(calendar = calendar, issues = issues)
}

# internal helper: read target ages such as "9-59 months" or "9 months to
# 14 years" into whole months, upper bound exclusive. a unit on the upper
# bound only applies to both
#' @noRd
.parse_target_age <- function(x) {
  pattern <- paste0(
    "^\\s*(\\d+)\\s*(months?|mo|m|years?|yrs?|y)?\\s*",
    "(?:-|\u2013|\u2014|to|and)\\s*",
    "(\\d+)\\s*(months?|mo|m|years?|yrs?|y)\\s*$"
  )
  text <- tolower(as.character(x))
  matches <- regmatches(text, regexec(pattern, text, perl = TRUE))

  bounds <- purrr::map(matches, \(parts) {
    if (length(parts) == 0) {
      return(c(NA_real_, NA_real_))
    }
    upper_unit <- parts[5]
    lower_unit <- if (nzchar(parts[3])) parts[3] else upper_unit
    upper_step <- if (grepl("^y", upper_unit)) 12 else 1
    c(
      .in_months(as.numeric(parts[2]), lower_unit),
      .in_months(as.numeric(parts[4]), upper_unit) + upper_step
    )
  })
  list(
    min = purrr::map_dbl(bounds, 1),
    max = purrr::map_dbl(bounds, 2)
  )
}

# internal helper: a value in months, given its unit
#' @noRd
.in_months <- function(value, unit) {
  if (grepl("^y", unit)) value * 12 else value
}

# internal helper: survey results as proportions in a standard layout
#' @noRd
.standardise_evaluation <- function(evaluation, geography) {
  if (is.null(evaluation)) {
    return(NULL)
  }
  optional <- c("survey", "lower_pct", "upper_pct", "n")
  missing_cols <- setdiff(optional, names(evaluation))
  empty_cols <- purrr::map(purrr::set_names(missing_cols), \(col) NA)

  evaluation |>
    dplyr::mutate(
      !!!empty_cols,
      survey = as.character(.data$survey),
      value = as.numeric(.data$value_pct) / 100,
      lower = as.numeric(.data$lower_pct) / 100,
      upper = as.numeric(.data$upper_pct) / 100,
      n = as.numeric(.data$n)
    ) |>
    dplyr::select(dplyr::all_of(c(
      geography, "campaign_id", "survey", "indicator", "value", "lower",
      "upper", "n"
    )))
}

# internal helper: flag the first row of each key that repeats
#' @noRd
.flag_duplicates <- function(x, key_cols, arg, geography) {
  detail_text <- paste0("repeated in ", arg, "; the first row is used")
  x |>
    dplyr::mutate(.key_rows = dplyr::n(), .by = dplyr::all_of(key_cols)) |>
    dplyr::filter(.data$.key_rows > 1) |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(key_cols)), .keep_all = TRUE) |>
    dplyr::mutate(detail = detail_text) |>
    .as_issues("duplicate_record", geography)
}

# internal helper: one row per geography and campaign found in any input
#' @noRd
.campaign_skeleton <- function(calendar, evaluation, reported, key_cols) {
  list(calendar, evaluation, reported) |>
    purrr::compact() |>
    purrr::map(\(x) dplyr::select(x, dplyr::all_of(key_cols))) |>
    dplyr::bind_rows() |>
    dplyr::distinct()
}

# internal helper: dates, ages, source and note from the first calendar row
# of each key
#' @noRd
.attach_calendar <- function(campaigns, calendar, key_cols) {
  dates <- calendar |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(key_cols)), .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(
      key_cols, "start_date", "end_date", "target_age", "age_min_months",
      "age_max_months",
      dates_source = "source", "note"
    )))
  dplyr::left_join(
    campaigns, dates,
    by = key_cols, relationship = "many-to-one"
  )
}

# internal helper: survey coverage first, then coverage reported by the
# campaign, labelled by source; nothing is imputed
#' @noRd
.attach_coverage <- function(campaigns, evaluation, reported, key_cols,
                             coverage_indicator) {
  campaigns <- campaigns |>
    dplyr::mutate(
      coverage = NA_real_,
      coverage_lower = NA_real_,
      coverage_upper = NA_real_,
      coverage_source = NA_character_,
      coverage_reference = NA_character_
    )

  if (!is.null(evaluation)) {
    wanted_indicator <- coverage_indicator
    survey <- evaluation |>
      dplyr::filter(.data$indicator == wanted_indicator) |>
      dplyr::distinct(
        dplyr::pick(dplyr::all_of(key_cols)), .keep_all = TRUE
      ) |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::select(dplyr::all_of(c(
        key_cols,
        .survey_value = "value", .survey_lower = "lower",
        .survey_upper = "upper", .survey_name = "survey"
      )))
    campaigns <- campaigns |>
      dplyr::left_join(survey, by = key_cols, relationship = "many-to-one") |>
      dplyr::mutate(
        coverage = .data$.survey_value,
        coverage_lower = .data$.survey_lower,
        coverage_upper = .data$.survey_upper,
        coverage_source = dplyr::if_else(
          is.na(.data$.survey_value), NA_character_, "survey"
        ),
        coverage_reference = .data$.survey_name
      ) |>
      dplyr::select(-dplyr::starts_with(".survey_"))
  }

  if (!is.null(reported)) {
    if (!"source" %in% names(reported)) {
      reported <- dplyr::mutate(reported, source = NA_character_)
    }
    reported <- reported |>
      dplyr::distinct(
        dplyr::pick(dplyr::all_of(key_cols)), .keep_all = TRUE
      ) |>
      dplyr::mutate(
        .reported_value = .data$coverage_pct / 100,
        .reported_name = as.character(.data$source)
      ) |>
      dplyr::select(dplyr::all_of(
        c(key_cols, ".reported_value", ".reported_name")
      ))
    campaigns <- campaigns |>
      dplyr::left_join(
        reported,
        by = key_cols, relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        .fill = is.na(.data$coverage) & !is.na(.data$.reported_value),
        coverage = dplyr::if_else(
          .data$.fill, .data$.reported_value, .data$coverage
        ),
        coverage_source = dplyr::if_else(
          .data$.fill, "coverage reported by the campaign",
          .data$coverage_source
        ),
        coverage_reference = dplyr::if_else(
          .data$.fill, .data$.reported_name, .data$coverage_reference
        )
      ) |>
      dplyr::select(
        -dplyr::all_of(c(".fill", ".reported_value", ".reported_name"))
      )
  }
  campaigns
}

# internal helper: records without dates, coverage or a target age
#' @noRd
.flag_campaign_gaps <- function(campaigns, geography) {
  dplyr::bind_rows(
    campaigns |>
      dplyr::filter(.data$needs_dates) |>
      dplyr::mutate(detail = "start or end date missing; complete by hand") |>
      .as_issues("missing_dates", geography),
    campaigns |>
      dplyr::filter(.data$needs_coverage) |>
      dplyr::mutate(
        detail = "no survey coverage or coverage reported by the campaign"
      ) |>
      .as_issues("missing_coverage", geography),
    campaigns |>
      dplyr::filter(is.na(.data$age_min_months), is.na(.data$target_age)) |>
      dplyr::mutate(detail = "no target age in the calendar") |>
      .as_issues("missing_age", geography)
  )
}

# internal helper: coverage reported by the campaign and survey values
# outside 0 to 1
#' @noRd
.flag_coverage_range <- function(campaigns, evaluation, geography) {
  reported_issues <- campaigns |>
    dplyr::filter(
      !is.na(.data$coverage),
      .data$coverage_source %in% "coverage reported by the campaign",
      .data$coverage < 0 | .data$coverage > 1
    ) |>
    dplyr::mutate(
      detail = paste0(
        "coverage of ", round(100 * .data$coverage, 1), "% (",
        .data$coverage_source, ")"
      )
    ) |>
    .as_issues("coverage_out_of_range", geography)

  survey_issues <- if (!is.null(evaluation)) {
    evaluation |>
      dplyr::filter(
        !is.na(.data$value),
        .data$value < 0 | .data$value > 1
      ) |>
      dplyr::mutate(
        detail = paste0(
          .data$indicator, " of ", round(100 * .data$value, 1), "%"
        )
      ) |>
      .as_issues("coverage_out_of_range", geography)
  }
  dplyr::bind_rows(reported_issues, survey_issues)
}

# internal helper: geography with missing identifiers, or not in the
# boundaries
#' @noRd
.flag_geography <- function(campaigns, geography, boundaries) {
  missing_id <- campaigns |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(geography), is.na))
  missing_issues <- missing_id |>
    dplyr::mutate(detail = "geography identifier missing") |>
    .as_issues("unmatched_geography", geography)
  if (is.null(boundaries)) {
    return(missing_issues)
  }
  if (inherits(boundaries, "sf")) {
    boundaries <- sf::st_drop_geometry(boundaries)
  }
  missing_cols <- setdiff(geography, names(boundaries))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.val {missing_cols}} not in {.arg boundaries}."
    )
  }
  known <- boundaries |>
    dplyr::select(dplyr::all_of(geography)) |>
    dplyr::distinct()
  outside_issues <- campaigns |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(geography), is.na)) |>
    dplyr::anti_join(known, by = geography) |>
    dplyr::mutate(detail = "not in the boundaries") |>
    .as_issues("unmatched_geography", geography)
  dplyr::bind_rows(missing_issues, outside_issues)
}

# internal helper: pairs of campaigns in the same geography with
# overlapping dates
#' @noRd
.flag_overlaps <- function(campaigns, geography) {
  dated <- campaigns |>
    dplyr::filter(!.data$needs_dates) |>
    dplyr::mutate(.row = dplyr::row_number())
  later <- dated |>
    dplyr::select(dplyr::all_of(c(
      geography,
      .row_b = ".row", .id_b = "campaign_id", .start_b = "start_date",
      .end_b = "end_date"
    )))
  dated |>
    dplyr::inner_join(later, by = geography, relationship = "many-to-many") |>
    dplyr::filter(
      .data$.row < .data$.row_b,
      .data$campaign_id != .data$.id_b,
      .data$start_date <= .data$.end_b,
      .data$.start_b <= .data$end_date
    ) |>
    dplyr::mutate(detail = paste("overlaps", .data$.id_b)) |>
    .as_issues("overlapping_campaigns", geography)
}
