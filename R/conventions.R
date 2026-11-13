# date and age conventions -----------------------------------------------------

#' Date and Age Conventions
#'
#' Sets the date and age rules used to build vaccination histories and to
#' estimate immunity. The defaults are the rules mipR has always used. The
#' other options let mipR reproduce analyses that place births, doses and
#' reporting dates differently.
#'
#' @details
#' **Births** (`birth`). With `"uniform"`, births are spread evenly over
#' every day of the birth month and eligibility is worked out for each day
#' of birth. With `"fixed_day"`, each monthly cohort is represented by one
#' birth date, day `birth_day` of the birth month, and every child in the
#' cohort is eligible for the same opportunities on the same dates.
#'
#' **Age** (`age`). With `"calendar"`, age is the exact calendar age: a
#' child born on 20 January is 9 months old on 20 October. With `"days"`,
#' age in months is the number of days since birth divided by
#' `days_per_month`. The age rule sets campaign eligibility (a child is
#' eligible on a campaign day when their age is at least `age_min_months`
#' and below `age_max_months`) and the age at each dose used for vaccine
#' effectiveness.
#'
#' **Routine dose dates** (`dose_timing`). With `"calendar_months"`, a dose
#' due at `age_months` is given on the date the child reaches that many whole
#' calendar months. With `"days"`, it is given `round(age_months *
#' days_per_month)` days after birth.
#'
#' **Reporting date** (`report_day`, `report_before_events`). Monthly
#' values are the vaccination state on day `report_day` of each month. With
#' `report_before_events = FALSE`, opportunities dated on the reporting day
#' are counted in that month; with `TRUE`, they are counted in the next
#' month.
#'
#' **Same-date order** (`same_date_order`). Opportunities on the same date
#' are applied with routine MCV1 first, then MCV2, then campaigns. With
#' `"event_id"`, campaigns on the same date are applied in order of
#' `event_id`. With `"input"`, opportunities on the same date are applied in
#' the order of their rows in the inputs to [build_vaccination_history()]:
#' the routine doses in the order of `schedule`, then the campaigns in the
#' order of the rows of `campaigns`.
#'
#' [mip_conventions_preset()] returns named sets of conventions.
#'
#' @param birth How births are placed within the birth month: `"uniform"`
#'   (default) or `"fixed_day"`.
#' @param birth_day Day of the month used as the birth date with
#'   `birth = "fixed_day"`, from 1 to 28. Default `15`.
#' @param age How age in months is measured: `"calendar"` (default) or
#'   `"days"`.
#' @param days_per_month Days in a month for `age = "days"` and
#'   `dose_timing = "days"`, from 28 to 31. Default `365.25 / 12`.
#' @param dose_timing How routine dose dates are set: `"calendar_months"`
#'   (default) or `"days"`.
#' @param report_day Day of the month on which monthly values are
#'   reported, from 1 to 28. Default `15`.
#' @param report_before_events Logical. Whether the monthly value is taken
#'   before the opportunities dated on the reporting day. Default `FALSE`.
#' @param same_date_order Order of opportunities on the same date:
#'   `"event_id"` (default) or `"input"`.
#'
#' @return A list of class `mip_conventions`, for the `conventions`
#'   argument of [build_vaccination_history()], [estimate_immunity()],
#'   [immunity_draws()] and [cohort_eligibility()].
#'
#' @examples
#' mip_conventions()
#'
#' # one birth date per cohort, on the 1st of the month
#' mip_conventions(birth = "fixed_day", birth_day = 1)
#' @export
mip_conventions <- function(
  birth = c("uniform", "fixed_day"),
  birth_day = 15L,
  age = c("calendar", "days"),
  days_per_month = 365.25 / 12,
  dose_timing = c("calendar_months", "days"),
  report_day = 15L,
  report_before_events = FALSE,
  same_date_order = c("event_id", "input")
) {
  birth <- match.arg(birth)
  age <- match.arg(age)
  dose_timing <- match.arg(dose_timing)
  same_date_order <- match.arg(same_date_order)
  birth_day <- .check_count(birth_day, "birth_day")
  report_day <- .check_count(report_day, "report_day")
  if (birth_day > 28L) {
    cli::cli_abort("{.arg birth_day} must be between 1 and 28.")
  }
  if (report_day > 28L) {
    cli::cli_abort("{.arg report_day} must be between 1 and 28.")
  }
  .check_number(days_per_month, "days_per_month", lower = 28, upper = 31)
  if (!is.logical(report_before_events) ||
        length(report_before_events) != 1 || is.na(report_before_events)) {
    cli::cli_abort("{.arg report_before_events} must be TRUE or FALSE.")
  }
  structure(
    list(
      birth = birth,
      birth_day = birth_day,
      age = age,
      days_per_month = as.numeric(days_per_month),
      dose_timing = dose_timing,
      report_day = report_day,
      report_before_events = report_before_events,
      same_date_order = same_date_order
    ),
    class = "mip_conventions"
  )
}

#' Named Sets of Date and Age Conventions
#'
#' Returns a named set of [mip_conventions()].
#'
#' @details
#' `"default"` is [mip_conventions()] with its defaults.
#'
#' `"v2_pipeline"` reproduces the vaccination-only runs of the measles
#' campaign pipeline (`v2_run`): one birth date per cohort on the 15th
#' (`birth = "fixed_day"`, `birth_day = 15`), age as days since birth
#' divided by 365.25 / 12 (`age = "days"`), routine doses `round(age_months
#' * 365.25 / 12)` days after birth (`dose_timing = "days"`), monthly values
#' on the 1st before that day's opportunities (`report_day = 1`,
#' `report_before_events = TRUE`) and same-date opportunities in input order
#' (`same_date_order = "input"`).
#'
#' @param name Name of the set: `"v2_pipeline"` or `"default"`.
#'
#' @return A list of class `mip_conventions`.
#'
#' @examples
#' mip_conventions_preset("v2_pipeline")
#' @export
mip_conventions_preset <- function(name = c("v2_pipeline", "default")) {
  name <- match.arg(name)
  switch(
    name,
    default = mip_conventions(),
    v2_pipeline = mip_conventions(
      birth = "fixed_day",
      birth_day = 15L,
      age = "days",
      days_per_month = 365.25 / 12,
      dose_timing = "days",
      report_day = 1L,
      report_before_events = TRUE,
      same_date_order = "input"
    )
  )
}

#' @export
print.mip_conventions <- function(x, ...) {
  cli::cli_text("mipR date and age conventions")
  values <- purrr::map_chr(unclass(x), \(v) format(v, digits = 10))
  cli::cli_bullets(purrr::set_names(
    paste0(names(values), ": ", values), rep("*", length(values))
  ))
  invisible(x)
}

# internal helper: a conventions object, checked
#' @noRd
.check_conventions <- function(conventions) {
  if (!inherits(conventions, "mip_conventions")) {
    cli::cli_abort(
      "{.arg conventions} must be made with {.fn mip_conventions}."
    )
  }
  invisible(conventions)
}

# internal helper: the conventions given, or else those stored in the
# history; both known must agree
#' @noRd
.resolve_conventions <- function(history, conventions, given) {
  stored <- history$metadata$conventions
  if (!given) {
    return(if (is.null(stored)) conventions else stored)
  }
  .check_conventions(conventions)
  if (!is.null(stored) && !identical(unclass(stored), unclass(conventions))) {
    cli::cli_abort(c(
      "{.arg conventions} differ from those used to build {.arg history}.",
      "i" = paste(
        "Leave {.arg conventions} out to use the ones stored in the",
        "history, or rebuild the history with the same conventions."
      )
    ))
  }
  conventions
}

# date and age rules -----------------------------------------------------------

# internal helper: the birth days that represent each birth month, with the
# number of birth days per cohort (cohort_days) and the birth date
#' @noRd
.birth_days <- function(cells, conventions = mip_conventions()) {
  months <- dplyr::tibble(birth_month = unique(cells$birth_month))
  if (conventions$birth == "fixed_day") {
    return(
      months |>
        dplyr::mutate(
          cohort_days = 1L,
          day = conventions$birth_day,
          birth_date = .data$birth_month + .data$day - 1L
        )
    )
  }
  months |>
    dplyr::mutate(
      n_days = .days_in_month(.month_index(.data$birth_month)),
      cohort_days = .data$n_days
    ) |>
    tidyr::uncount(.data$n_days, .id = "day") |>
    dplyr::mutate(birth_date = .data$birth_month + .data$day - 1L)
}

# internal helper: number of birth days that represent each birth month
#' @noRd
.cohort_days <- function(birth_month, conventions = mip_conventions()) {
  if (conventions$birth == "fixed_day") {
    return(rep(1L, length(birth_month)))
  }
  .days_in_month(.month_index(birth_month))
}

# internal helper: age in months on `date` of a child born on birth_date
#' @noRd
.age_at <- function(birth_date, date, conventions = mip_conventions()) {
  if (conventions$age == "days") {
    return(as.numeric(date - birth_date) / conventions$days_per_month)
  }
  .age_in_months(birth_date, date)
}

# internal helper: first date a child born on birth_date is `age` months
# old; in days, tested on the same expression as .age_at()
#' @noRd
.age_reached <- function(birth_date, age, conventions = mip_conventions()) {
  if (conventions$age == "calendar") {
    return(.add_months(birth_date, age))
  }
  per_month <- conventions$days_per_month
  days <- ceiling(age * per_month)
  days <- dplyr::case_when(
    (days - 1) / per_month >= age ~ days - 1,
    days / per_month < age ~ days + 1,
    .default = days
  )
  birth_date + days
}

# internal helper: date of a routine dose due at age_months
#' @noRd
.dose_date <- function(birth_date, age_months,
                       conventions = mip_conventions()) {
  if (conventions$dose_timing == "days") {
    return(birth_date + round(age_months * conventions$days_per_month))
  }
  .add_months(birth_date, age_months)
}

# internal helper: the reporting date of each month
#' @noRd
.report_date <- function(month, conventions = mip_conventions()) {
  month + conventions$report_day - 1L
}

# internal helper: the last date whose opportunities count in the value of
# each month
#' @noRd
.last_counted_date <- function(month, conventions = mip_conventions()) {
  .report_date(month, conventions) -
    as.integer(conventions$report_before_events)
}
