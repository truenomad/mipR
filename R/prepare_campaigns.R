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

  diagnostics <- list()
  add_issue <- function(check, rows, detail) {
    if (nrow(rows) == 0) {
      return(invisible(NULL))
    }
    diagnostics[[length(diagnostics) + 1]] <<- data.frame(
      check = check,
      rows[, c("campaign_id", geography), drop = FALSE],
      detail = detail
    )
  }

  calendar <- .standardise_calendar(calendar, add_issue)
  evaluation_out <- .standardise_evaluation(evaluation, geography)

  key_cols <- c(geography, "campaign_id")
  .flag_duplicates(calendar, key_cols, "calendar", add_issue)
  if (!is.null(evaluation)) {
    .flag_duplicates(
      evaluation_out, c(key_cols, "survey", "indicator"), "evaluation",
      add_issue
    )
  }
  if (!is.null(reported_coverage)) {
    .flag_duplicates(
      reported_coverage, key_cols, "reported_coverage", add_issue
    )
  }

  campaigns <- .campaign_skeleton(
    calendar, evaluation_out, reported_coverage, key_cols
  )
  campaigns <- .attach_calendar(campaigns, calendar, key_cols)
  campaigns <- .attach_coverage(
    campaigns, evaluation_out, reported_coverage, key_cols,
    coverage_indicator
  )

  campaigns$needs_dates <- is.na(campaigns$start_date) |
    is.na(campaigns$end_date)
  campaigns$needs_coverage <- is.na(campaigns$coverage)

  add_issue(
    "missing_dates", campaigns[campaigns$needs_dates, ],
    "start or end date missing; complete by hand"
  )
  add_issue(
    "missing_coverage", campaigns[campaigns$needs_coverage, ],
    "no survey coverage or coverage reported by the campaign"
  )
  add_issue(
    "missing_age",
    campaigns[is.na(campaigns$age_min_months) &
                is.na(campaigns$target_age), ],
    "no target age in the calendar"
  )
  # survey values are checked in the evaluation table below
  out_of_range <- !is.na(campaigns$coverage) &
    campaigns$coverage_source %in% "coverage reported by the campaign" &
    (campaigns$coverage < 0 | campaigns$coverage > 1)
  add_issue(
    "coverage_out_of_range", campaigns[out_of_range, ],
    paste0(
      "coverage of ", round(100 * campaigns$coverage[out_of_range], 1),
      "% (", campaigns$coverage_source[out_of_range], ")"
    )
  )
  if (!is.null(evaluation)) {
    bad_eval <- evaluation_out[
      !is.na(evaluation_out$value) &
        (evaluation_out$value < 0 | evaluation_out$value > 1),
    ]
    add_issue(
      "coverage_out_of_range", bad_eval,
      paste0(bad_eval$indicator, " of ", round(100 * bad_eval$value, 1), "%")
    )
  }
  .flag_geography(campaigns, geography, boundaries, add_issue)
  .flag_overlaps(campaigns, geography, add_issue)

  diagnostics <- if (length(diagnostics) == 0) {
    data.frame(
      check = character(0), campaign_id = character(0),
      campaigns[0, geography, drop = FALSE], detail = character(0)
    )
  } else {
    do.call(rbind, diagnostics)
  }
  rownames(diagnostics) <- NULL

  campaigns <- campaigns[do.call(order, unname(as.list(
    campaigns[, c("campaign_id", geography), drop = FALSE]
  ))), ]
  rownames(campaigns) <- NULL
  campaigns <- campaigns[, c(
    geography, "campaign_id", "start_date", "end_date", "target_age",
    "age_min_months", "age_max_months", "coverage", "coverage_lower",
    "coverage_upper", "coverage_source", "coverage_reference",
    "dates_source", "note", "needs_dates", "needs_coverage"
  )]

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
  x <- as.data.frame(x)
  missing_cols <- setdiff(needed, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "{.arg {arg}} needs {cli::qty(missing_cols)}column{?s} ",
      "{.val {missing_cols}}."
    ))
  }
  x
}

# internal helper: dates as Date, target ages as month bounds
#' @noRd
.standardise_calendar <- function(calendar, add_issue) {
  calendar$start_date <- as.Date(calendar$start_date)
  calendar$end_date <- as.Date(calendar$end_date)
  for (col in c("target_age", "source", "note")) {
    if (!col %in% names(calendar)) {
      calendar[[col]] <- NA_character_
    }
  }

  has_bounds <- all(c("age_min_months", "age_max_months") %in% names(calendar))
  parsed <- .parse_target_age(calendar$target_age)
  if (has_bounds) {
    given <- !is.na(calendar$age_min_months) & !is.na(calendar$age_max_months)
    parsed$min[given] <- calendar$age_min_months[given]
    parsed$max[given] <- calendar$age_max_months[given]
  }
  calendar$age_min_months <- parsed$min
  calendar$age_max_months <- parsed$max

  unreadable <- !is.na(calendar$target_age) & is.na(parsed$min) &
    !(has_bounds && !is.na(calendar$age_min_months))
  add_issue(
    "invalid_age", calendar[unreadable, ],
    paste0("target age not read: ", calendar$target_age[unreadable])
  )
  bad_range <- !is.na(parsed$min) & !is.na(parsed$max) &
    (parsed$min < 0 | parsed$max <= parsed$min)
  add_issue(
    "invalid_age", calendar[bad_range, ],
    paste0(
      "age range ", parsed$min[bad_range], " to ", parsed$max[bad_range],
      " months"
    )
  )
  calendar$age_min_months[bad_range] <- NA
  calendar$age_max_months[bad_range] <- NA

  backwards <- !is.na(calendar$start_date) & !is.na(calendar$end_date) &
    calendar$end_date < calendar$start_date
  add_issue(
    "end_before_start", calendar[backwards, ],
    paste(calendar$start_date[backwards], "to", calendar$end_date[backwards])
  )
  calendar
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
  m <- regmatches(
    tolower(as.character(x)),
    regexec(pattern, tolower(as.character(x)), perl = TRUE)
  )
  out <- list(
    min = rep(NA_real_, length(x)),
    max = rep(NA_real_, length(x))
  )
  in_months <- function(value, unit) {
    ifelse(grepl("^y", unit), value * 12, value)
  }
  for (i in seq_along(m)) {
    parts <- m[[i]]
    if (length(parts) == 0) {
      next
    }
    upper_unit <- parts[5]
    lower_unit <- if (nzchar(parts[3])) parts[3] else upper_unit
    upper_step <- if (grepl("^y", upper_unit)) 12 else 1
    out$min[i] <- in_months(as.numeric(parts[2]), lower_unit)
    out$max[i] <- in_months(as.numeric(parts[4]), upper_unit) + upper_step
  }
  out
}

# internal helper: survey results as proportions in a standard layout
#' @noRd
.standardise_evaluation <- function(evaluation, geography) {
  if (is.null(evaluation)) {
    return(NULL)
  }
  column <- function(name) {
    if (name %in% names(evaluation)) evaluation[[name]] else NA
  }
  out <- data.frame(
    evaluation[, geography, drop = FALSE],
    campaign_id = evaluation$campaign_id,
    survey = as.character(column("survey")),
    indicator = evaluation$indicator,
    value = as.numeric(evaluation$value_pct) / 100,
    lower = as.numeric(column("lower_pct")) / 100,
    upper = as.numeric(column("upper_pct")) / 100,
    n = as.numeric(column("n"))
  )
  rownames(out) <- NULL
  out
}

#' @noRd
.campaign_key <- function(x, key_cols) {
  do.call(paste, c(unname(as.list(x[, key_cols, drop = FALSE])), sep = "\r"))
}

# internal helper: flag rows that repeat a key
#' @noRd
.flag_duplicates <- function(x, key_cols, arg, add_issue) {
  key <- .campaign_key(x, key_cols)
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  rows <- x[dup & !duplicated(key), , drop = FALSE]
  add_issue(
    "duplicate_record", rows,
    paste0("repeated in ", arg, "; the first row is used")
  )
}

# internal helper: one row per geography and campaign found in any input
#' @noRd
.campaign_skeleton <- function(calendar, evaluation, reported, key_cols) {
  parts <- list(calendar[, key_cols, drop = FALSE])
  if (!is.null(evaluation)) {
    parts[[length(parts) + 1]] <- evaluation[, key_cols, drop = FALSE]
  }
  if (!is.null(reported)) {
    parts[[length(parts) + 1]] <- reported[, key_cols, drop = FALSE]
  }
  all_keys <- do.call(rbind, parts)
  all_keys[!duplicated(.campaign_key(all_keys, key_cols)), , drop = FALSE]
}

#' @noRd
.attach_calendar <- function(campaigns, calendar, key_cols) {
  calendar <- calendar[!duplicated(.campaign_key(calendar, key_cols)), ]
  i <- match(
    .campaign_key(campaigns, key_cols),
    .campaign_key(calendar, key_cols)
  )
  campaigns$start_date <- calendar$start_date[i]
  campaigns$end_date <- calendar$end_date[i]
  campaigns$target_age <- calendar$target_age[i]
  campaigns$age_min_months <- calendar$age_min_months[i]
  campaigns$age_max_months <- calendar$age_max_months[i]
  campaigns$dates_source <- calendar$source[i]
  campaigns$note <- calendar$note[i]
  campaigns
}

# internal helper: survey coverage first, then coverage reported by the
# campaign, labelled by source; nothing is imputed
#' @noRd
.attach_coverage <- function(campaigns, evaluation, reported, key_cols,
                             coverage_indicator) {
  campaigns$coverage <- NA_real_
  campaigns$coverage_lower <- NA_real_
  campaigns$coverage_upper <- NA_real_
  campaigns$coverage_source <- NA_character_
  campaigns$coverage_reference <- NA_character_
  key <- .campaign_key(campaigns, key_cols)

  if (!is.null(evaluation)) {
    survey <- evaluation[evaluation$indicator == coverage_indicator, ]
    survey <- survey[!duplicated(.campaign_key(survey, key_cols)), ]
    i <- match(key, .campaign_key(survey, key_cols))
    found <- !is.na(i) & !is.na(survey$value[i])
    campaigns$coverage[found] <- survey$value[i[found]]
    campaigns$coverage_lower[found] <- survey$lower[i[found]]
    campaigns$coverage_upper[found] <- survey$upper[i[found]]
    campaigns$coverage_source[found] <- "survey"
    campaigns$coverage_reference[found] <- survey$survey[i[found]]
  }

  if (!is.null(reported)) {
    reported <- reported[!duplicated(.campaign_key(reported, key_cols)), ]
    i <- match(key, .campaign_key(reported, key_cols))
    fill <- is.na(campaigns$coverage) & !is.na(i) &
      !is.na(reported$coverage_pct[i])
    campaigns$coverage[fill] <- reported$coverage_pct[i[fill]] / 100
    campaigns$coverage_source[fill] <- "coverage reported by the campaign"
    if ("source" %in% names(reported)) {
      campaigns$coverage_reference[fill] <- reported$source[i[fill]]
    }
  }
  campaigns
}

# internal helper: geography with missing identifiers, or not in the
# boundaries
#' @noRd
.flag_geography <- function(campaigns, geography, boundaries, add_issue) {
  missing_id <- !stats::complete.cases(campaigns[, geography, drop = FALSE])
  add_issue(
    "unmatched_geography", campaigns[missing_id, ],
    "geography identifier missing"
  )
  if (is.null(boundaries)) {
    return(invisible(NULL))
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
  known <- .campaign_key(boundaries, geography)
  outside <- !missing_id & !.campaign_key(campaigns, geography) %in% known
  add_issue(
    "unmatched_geography", campaigns[outside, ],
    "not in the boundaries"
  )
}

# internal helper: pairs of campaigns in the same geography with
# overlapping dates
#' @noRd
.flag_overlaps <- function(campaigns, geography, add_issue) {
  dated <- campaigns[!campaigns$needs_dates, , drop = FALSE]
  if (nrow(dated) < 2) {
    return(invisible(NULL))
  }
  geo <- .campaign_key(dated, geography)
  pairs <- merge(
    data.frame(geo = geo, a = seq_len(nrow(dated))),
    data.frame(geo = geo, b = seq_len(nrow(dated))),
    by = "geo"
  )
  pairs <- pairs[pairs$a < pairs$b, ]
  pairs <- pairs[
    dated$campaign_id[pairs$a] != dated$campaign_id[pairs$b] &
      dated$start_date[pairs$a] <= dated$end_date[pairs$b] &
      dated$start_date[pairs$b] <= dated$end_date[pairs$a],
  ]
  add_issue(
    "overlapping_campaigns", dated[pairs$a, , drop = FALSE],
    paste("overlaps", dated$campaign_id[pairs$b])
  )
}
