# anchors and reconstruction ---------------------------------------------------

#' Prepare Population Anchors for Cohort Reconstruction
#'
#' Converts a table of population by single year of age, such as the output
#' of [extract_raster()] on rasters from [get_worldpop_single_age()], into
#' the anchor format used by [reconstruct_cohorts()].
#'
#' @param data A data frame with one row per geography, year and single year
#'   of age.
#' @param geography Columns of `data` that identify the geography.
#' @param year Name of the year column. Default `"year"`.
#' @param age Name of the column holding age in completed years. Default
#'   `"age"`.
#' @param population Name of the population column. Default `"sum"`, as
#'   written by [extract_raster()].
#' @param ref_date Reference date of the estimates within each year, as
#'   `"MM-DD"`. Default `"01-01"`, the WorldPop R2025A reference date.
#'
#' @return A data frame with the `geography` columns, `ref_date`,
#'   `age_lower` and `age_upper` (age in months, upper bound exclusive) and
#'   `population`.
#'
#' @examples
#' pop <- data.frame(
#'   adm1 = "Kano", year = 2025, age = 0:1, sum = c(1200, 1150)
#' )
#' make_anchors(pop, geography = "adm1")
#' @export
make_anchors <- function(
  data,
  geography,
  year = "year",
  age = "age",
  population = "sum",
  ref_date = "01-01"
) {
  needed <- c(geography, year, age, population)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} {.val {missing_cols}} not in {.arg data}.")
  }
  if (!grepl("^\\d{2}-\\d{2}$", ref_date)) {
    cli::cli_abort(
      "{.arg ref_date} must be {.val MM-DD}, such as {.val 01-01}."
    )
  }

  out <- data[, geography, drop = FALSE]
  out$ref_date <- .as_date(paste0(data[[year]], "-", ref_date), "ref_date")
  out$age_lower <- as.integer(data[[age]]) * 12L
  out$age_upper <- out$age_lower + 12L
  out$population <- as.numeric(data[[population]])
  rownames(out) <- NULL
  out
}

#' Reconstruct Monthly Birth Cohorts from Population Anchors
#'
#' Rebuilds the population by month of birth and calendar month from annual
#' population estimates by age (the anchors), keeping each birth cohort
#' consistent across years.
#'
#' @details
#' This is the uniform method (version 0.1). For each anchor, births are
#' assumed to be spread evenly by day over the birth-date window that the
#' anchor's age group covers at its reference date. For example, people aged
#' 0 on 1 January 2025 were born from 1 January to 31 December 2024. Each
#' monthly cohort's size at the anchor date is the daily density times the
#' days in the month, so longer months hold more births. When two anchors
#' observe the same cohort on the same date (because the reference date is
#' not the first of a month), the observations are averaged by overlap.
#'
#' Between anchor dates, each cohort's size is interpolated linearly along
#' the cohort, not by age, so a cohort is followed as it ages. Before its
#' first anchor and after its last, the nearest anchor value is carried. No
#' mortality is applied, so every change between anchors is reported in
#' `cohort_changes` as an unexplained cohort change. Cohorts born after the
#' last anchor date are not covered by any anchor and are left out.
#'
#' The population of a cohort in a calendar month is its size on the 15th of
#' that month. A cohort is present from its birth month, where `age_months`
#' is 0.
#'
#' @param anchors A data frame of population estimates with the
#'   `geography` columns, `ref_date` (Date), `age_lower` and `age_upper`
#'   (age in completed months, upper bound exclusive) and `population`, as
#'   returned by [make_anchors()].
#' @param geography Columns of `anchors` that identify the geography.
#'   Default `"geography"`.
#' @param start,end First and last calendar month of the output, as Dates or
#'   `"YYYY-MM"`. Default: the earliest birth month covered by the anchors,
#'   and the month of the latest anchor date.
#' @param max_age_months Oldest age in months to keep in the output. Default
#'   `179` (under 15 years).
#' @param method Reconstruction method. Only `"uniform"` is available.
#'
#' @return A list of four data frames and a list:
#'   \describe{
#'     \item{cohorts}{One row per geography, birth month and calendar month:
#'       the `geography` columns, `birth_month`, `month` (first day of each
#'       month), `age_months` and `population`}
#'     \item{cohort_changes}{One row per cohort and pair of consecutive
#'       anchor dates: sizes at each date and `change`, the proportional
#'       change between them}
#'     \item{anchor_check}{Each anchor with the population rebuilt from the
#'       cohorts at its reference date, and the difference}
#'     \item{metadata}{The method and the assumptions used}
#'   }
#'
#' @examples
#' pop <- data.frame(
#'   adm1 = "Kano",
#'   year = rep(c(2024, 2025), each = 2),
#'   age = rep(0:1, 2),
#'   sum = c(1180, 1130, 1200, 1150)
#' )
#' anchors <- make_anchors(pop, geography = "adm1")
#' cohorts <- reconstruct_cohorts(anchors, geography = "adm1")
#' head(cohorts$cohorts)
#' cohorts$cohort_changes
#' @export
reconstruct_cohorts <- function(
  anchors,
  geography = "geography",
  start = NULL,
  end = NULL,
  max_age_months = 179,
  method = "uniform"
) {
  method <- match.arg(method, "uniform")
  anchors <- .check_anchors(anchors, geography)

  key <- .geography_key(anchors, geography)
  geo_lookup <- anchors[!duplicated(key), geography, drop = FALSE]
  geo_lookup$.key <- key[!duplicated(key)]
  anchors$.key <- key
  anchors$.anchor <- seq_len(nrow(anchors))

  pieces <- .anchor_pieces(anchors)
  obs <- .cohort_observations(pieces)

  start_index <- if (is.null(start)) {
    min(obs$birth_index)
  } else {
    .month_index(.as_date(start, "start"))
  }
  end_index <- if (is.null(end)) {
    .month_index(max(anchors$ref_date))
  } else {
    .month_index(.as_date(end, "end"))
  }
  if (end_index < start_index) {
    cli::cli_abort("{.arg end} must not be before {.arg start}.")
  }

  cohorts <- .cohort_panel(obs, start_index, end_index, max_age_months)
  cohorts <- merge(geo_lookup, cohorts, by = ".key")
  cohorts <- cohorts[
    order(cohorts$.key, cohorts$birth_index, cohorts$month_index),
  ]
  cohorts <- data.frame(
    cohorts[, geography, drop = FALSE],
    birth_month = .month_from_index(cohorts$birth_index),
    month = .month_from_index(cohorts$month_index),
    age_months = cohorts$month_index - cohorts$birth_index,
    population = cohorts$population
  )
  rownames(cohorts) <- NULL

  changes <- .cohort_changes(obs)
  changes <- merge(geo_lookup, changes, by = ".key")
  changes <- data.frame(
    changes[, geography, drop = FALSE],
    birth_month = .month_from_index(changes$birth_index),
    changes[, c("from_date", "to_date", "from_population",
                "to_population", "change")]
  )

  list(
    cohorts = cohorts,
    cohort_changes = changes,
    anchor_check = .anchor_check(anchors, pieces, obs, geography),
    metadata = list(
      method = method,
      births_within_month = "uniform by day",
      population_date = "15th of each month",
      mortality = "none; changes between anchors are unexplained",
      between_anchors = "linear along each cohort"
    )
  )
}

# internal helper: validate the anchor table
#' @noRd
.check_anchors <- function(anchors, geography) {
  needed <- c(geography, "ref_date", "age_lower", "age_upper", "population")
  missing_cols <- setdiff(needed, names(anchors))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Column{?s} {.val {missing_cols}} not in {.arg anchors}.",
      "i" = "Use {.fn make_anchors} to build the anchor table."
    ))
  }
  anchors$ref_date <- .as_date(anchors$ref_date, "ref_date")
  bad_age <- !is.finite(anchors$age_lower) | !is.finite(anchors$age_upper) |
    anchors$age_lower < 0 | anchors$age_upper <= anchors$age_lower
  if (any(bad_age)) {
    cli::cli_abort(paste0(
      "{.arg age_lower} and {.arg age_upper} must be finite, with ",
      "{.arg age_upper} above {.arg age_lower}."
    ))
  }
  if (any(is.na(anchors$population) | anchors$population < 0)) {
    cli::cli_abort("{.arg population} must be zero or more, without NA.")
  }
  anchors
}

#' @noRd
.geography_key <- function(data, geography) {
  do.call(paste, c(unname(as.list(data[geography])), sep = "\r"))
}

# internal helper: split each anchor across the birth months its window
# covers. births are in [from, to), with uniform density per day
#' @noRd
.anchor_pieces <- function(anchors) {
  from <- .add_months(anchors$ref_date, -anchors$age_upper)
  to <- .add_months(anchors$ref_date, -anchors$age_lower)
  first <- .month_index(from)
  last <- .month_index(to - 1L)
  n <- last - first + 1L

  i <- rep(seq_len(nrow(anchors)), n)
  birth_index <- first[i] + sequence(n) - 1L
  month_start <- .month_from_index(birth_index)
  month_end <- .month_from_index(birth_index + 1L)
  overlap <- as.numeric(pmin(month_end, to[i]) - pmax(month_start, from[i]))

  data.frame(
    .anchor = anchors$.anchor[i],
    .key = anchors$.key[i],
    ref_date = anchors$ref_date[i],
    birth_index = birth_index,
    overlap = overlap,
    density = anchors$population[i] / as.numeric(to - from)[i]
  )
}

# internal helper: size of each cohort on each anchor date, averaging
# anchors that share a cohort and date by days of overlap
#' @noRd
.cohort_observations <- function(pieces) {
  pieces$weighted <- pieces$density * pieces$overlap
  obs <- stats::aggregate(
    cbind(weighted, overlap) ~ .key + birth_index + ref_date,
    data = pieces,
    FUN = sum
  )
  obs$population <- obs$weighted / obs$overlap *
    .days_in_month(obs$birth_index)
  obs[order(obs$.key, obs$birth_index, obs$ref_date),
      c(".key", "birth_index", "ref_date", "population")]
}

# internal helper: cohort sizes by calendar month, interpolated along each
# cohort between its anchor dates
#' @noRd
.cohort_panel <- function(obs, start_index, end_index, max_age_months) {
  groups <- split(obs, list(obs$.key, obs$birth_index), drop = TRUE)
  rows <- lapply(groups, function(g) {
    b <- g$birth_index[1]
    first <- max(b, start_index)
    last <- min(end_index, b + max_age_months)
    if (first > last) {
      return(NULL)
    }
    months <- seq.int(first, last)
    mid <- .month_from_index(months) + 14L
    population <- if (nrow(g) == 1) {
      rep(g$population, length(months))
    } else {
      stats::approx(
        as.numeric(g$ref_date), g$population,
        xout = as.numeric(mid), rule = 2
      )$y
    }
    data.frame(
      .key = g$.key[1],
      birth_index = b,
      month_index = months,
      population = population
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    cli::cli_abort("No cohorts fall within {.arg start} and {.arg end}.")
  }
  out
}

# internal helper: change in each cohort between consecutive anchor dates
#' @noRd
.cohort_changes <- function(obs) {
  same <- c(FALSE, obs$.key[-1] == obs$.key[-nrow(obs)] &
    obs$birth_index[-1] == obs$birth_index[-nrow(obs)])
  to <- which(same)
  from <- to - 1L
  data.frame(
    .key = obs$.key[to],
    birth_index = obs$birth_index[to],
    from_date = obs$ref_date[from],
    to_date = obs$ref_date[to],
    from_population = obs$population[from],
    to_population = obs$population[to],
    change = obs$population[to] / obs$population[from] - 1
  )
}

# internal helper: rebuild each anchor from the cohorts on its date
#' @noRd
.anchor_check <- function(anchors, pieces, obs, geography) {
  matched <- merge(
    pieces, obs,
    by = c(".key", "birth_index", "ref_date")
  )
  matched$rebuilt <- matched$population * matched$overlap /
    .days_in_month(matched$birth_index)
  rebuilt <- stats::aggregate(rebuilt ~ .anchor, data = matched, FUN = sum)

  out <- anchors[, c(geography, "ref_date", "age_lower", "age_upper",
                     "population", ".anchor")]
  out$rebuilt <- rebuilt$rebuilt[match(out$.anchor, rebuilt$.anchor)]
  out$difference <- out$rebuilt - out$population
  out$.anchor <- NULL
  rownames(out) <- NULL
  out
}

# eligibility ------------------------------------------------------------------

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

# date helpers -----------------------------------------------------------------

# a month is indexed as year * 12 + (month - 1), so consecutive months
# differ by one

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
