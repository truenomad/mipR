#' Share of a Monthly Birth Cohort Eligible by Age
#'
#' Calculates, for each monthly birth cohort, the share of children whose
#' age falls within an age range on a vaccination date or at any time
#' during a campaign.
#'
#' @details
#' Each cohort covers the birth dates from the first to the last day of its
#' month. With `convention = "fractional"` (the default), births are spread
#' evenly over those days, and eligibility is worked out for each day of
#' birth from the exact date or campaign interval. A child born on 20
#' January turns 9 months on 20 October, so on 15 October the January 2024
#' cohort's share aged 9 months or more is 15/31. With
#' `convention = "midmonth"`, every birth is placed on the 15th, so the
#' share is 0 or 1.
#'
#' A child is eligible when, at some time between `start` and `end`, they
#' are at least `age_min_months` old and younger than `age_max_months`. For
#' a campaign targeting 9-59 months, use `age_min_months = 9` and
#' `age_max_months = 60`.
#'
#' @param birth_month Birth month of each cohort, as Dates or `"YYYY-MM"`.
#' @param start Vaccination date, or first day of the campaign.
#' @param end Last day of the campaign. Defaults to `start`.
#' @param age_min_months Youngest eligible age in months. Default `0`.
#' @param age_max_months Age in months at which children stop being
#'   eligible (exclusive). Default `Inf`.
#' @param convention `"fractional"` (default) or `"midmonth"`.
#'
#' @return Numeric vector of eligible shares between 0 and 1, one per
#'   `birth_month`.
#'
#' @examples
#' # share of the January 2024 cohort aged 9 months or more on 15 October
#' cohort_eligibility("2024-01", start = "2024-10-15", age_min_months = 9)
#'
#' # campaign for 9-59 months, 1-10 March 2025
#' cohort_eligibility(
#'   c("2020-02", "2020-03", "2024-06"),
#'   start = "2025-03-01", end = "2025-03-10",
#'   age_min_months = 9, age_max_months = 60
#' )
#' @export
cohort_eligibility <- function(
  birth_month,
  start,
  end = start,
  age_min_months = 0,
  age_max_months = Inf,
  convention = c("fractional", "midmonth")
) {
  convention <- match.arg(convention)
  start <- .as_date(start, "start")
  end <- .as_date(end, "end")
  if (length(start) != 1 || length(end) != 1 || end < start) {
    cli::cli_abort(paste0(
      "{.arg start} and {.arg end} must be single dates, with {.arg end} ",
      "not before {.arg start}."
    ))
  }
  if (age_max_months <= age_min_months) {
    cli::cli_abort(
      "{.arg age_max_months} must be above {.arg age_min_months}."
    )
  }

  index <- .month_index(.as_date(birth_month, "birth_month"))
  unique_index <- unique(index)

  share <- vapply(unique_index, function(m) {
    days <- if (convention == "midmonth") {
      .month_from_index(m) + 14L
    } else {
      .month_from_index(m) + seq_len(.days_in_month(m)) - 1L
    }
    old_enough <- .add_months(days, age_min_months) <= end
    young_enough <- if (is.finite(age_max_months)) {
      .add_months(days, age_max_months) > start
    } else {
      TRUE
    }
    mean(old_enough & young_enough)
  }, numeric(1))

  share[match(index, unique_index)]
}
