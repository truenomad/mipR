# internal date helpers for monthly birth cohorts. a month is indexed as
# year * 12 + (month - 1), so consecutive months differ by one

#' @noRd
.month_index <- function(date) {
  lt <- as.POSIXlt(date)
  (lt$year + 1900L) * 12L + lt$mon
}

#' @noRd
.month_from_index <- function(index) {
  as.Date(sprintf("%04d-%02d-01", index %/% 12L, index %% 12L + 1L))
}

#' @noRd
.days_in_month <- function(index) {
  as.integer(.month_from_index(index + 1L) - .month_from_index(index))
}

# add whole months to dates, keeping the day of the month and moving to the
# last day when the target month is shorter (31 January + 1 = 28 February)
#' @noRd
.add_months <- function(date, n) {
  lt <- as.POSIXlt(date)
  target <- (lt$year + 1900L) * 12L + lt$mon + as.integer(n)
  day <- pmin(lt$mday, .days_in_month(target))
  .month_from_index(target) + (day - 1L)
}

# parse "YYYY-MM", "YYYY-MM-DD" or Date into a Date
#' @noRd
.as_date <- function(x, arg = "date") {
  if (inherits(x, "Date")) {
    return(x)
  }
  x <- as.character(x)
  x <- ifelse(grepl("^\\d{4}-\\d{2}$", x), paste0(x, "-01"), x)
  out <- as.Date(x, optional = TRUE)
  if (anyNA(out)) {
    cli::cli_abort(
      "{.arg {arg}} must be a Date or a string such as {.val 2025-01}."
    )
  }
  out
}
