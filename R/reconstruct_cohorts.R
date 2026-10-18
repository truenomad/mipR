# anchors and reconstruction ---------------------------------------------------

#' Reconstruct Monthly Birth Cohorts from Annual Population by Age
#'
#' Rebuilds the population by month of birth and calendar month from annual
#' population estimates by single year of age, keeping each birth cohort
#' consistent across years. Each estimate (a geography, year and age) is an
#' anchor for the cohorts it covers.
#'
#' @details
#' **Anchors.** People aged `a` years at a reference date were born in the
#' 12 months before their `a`-th birthday. For example, people aged 0 on 1
#' January 2025 were born from 1 January to 31 December 2024. Each anchor is
#' shared out across the birth months in that window by day of birth,
#' weighted by `seasonality` when given and, for the `survival` and
#' `constrained` methods, by the share of each month's births expected to
#' survive to the reference date.
#'
#' **Methods.**
#' - `"uniform"`: no births or mortality. Each cohort's size at each
#'   anchor date is taken from the anchors, interpolated linearly along the
#'   cohort between anchor dates and held constant before the first and
#'   after the last. Cohorts born after the last anchor date are left out.
#' - `"survival"`: births in each birth month are estimated by dividing each
#'   anchor's share by survival from birth to the reference date, averaged
#'   across the anchors that observe the cohort. The cohort is then carried
#'   forward from birth with monthly survival, so it changes only through
#'   mortality.
#' - `"constrained"`: starts from the `survival` births and reweights the
#'   anchors until the fit is stable. Each cohort's births are a weighted
#'   geometric mean of what its anchors imply. Anchors within `tolerance`
#'   of the fit keep full weight and anchors outside it are down-weighted
#'   in proportion to how far outside they are (Huber weights), so an
#'   outlying anchor pulls the fit less than in `"survival"`. Anchors still
#'   outside the tolerance are flagged in `anchor_check`.
#'
#' **Survival.** Monthly survival comes from `mortality`, single-age central
#' death rates by calendar year. From age 1 the monthly survival is
#' `exp(-mx / 12)`. In the first year, the infant death probability is
#' derived from `mx` at age 0 with the Coale-Demeny separation factor, half
#' of infant deaths fall in the first month of life, and the rest are spread
#' evenly over the remaining 11 months. Mortality is national, so every
#' geography uses the same survival.
#'
#' **Cohorts after the last anchor.** With `survival` or `constrained`,
#' cohorts born after a geography's last anchored birth month and up to
#' `end` are extended from the births of its last 12 anchored months. They
#' follow the national trend in `births` when given, and `seasonality`. These
#' cohorts have `basis = "extended"`; all others are `"anchored"`.
#'
#' **What is not modelled.** Migration is not modelled. A change in a cohort
#' between anchors that births and survival do not explain is reported as an
#' unexplained cohort change in `cohort_changes`. It can come from
#' migration or from differences between the annual estimates, and is not
#' labelled as migration.
#'
#' The population of a cohort in a calendar month is its size in that month;
#' a cohort is present from its birth month, where `age_months` is 0.
#'
#' @param data A data frame with one row per geography, year and single
#'   year of age, such as the output of [extract_raster()] on rasters from
#'   [get_worldpop_single_age()].
#' @param geography Columns of `data` that identify the geography.
#' @param year Name of the year column. Default `"year"`.
#' @param age Name of the column holding age in completed years. Default
#'   `"age"`.
#' @param population Name of the population column. Default `"pop"`.
#' @param ref_date Reference date of the estimates within each year, as
#'   `"MM-DD"`. Default `"01-01"`, the WorldPop R2025A reference date.
#' @param method `"constrained"` (default), `"survival"` or `"uniform"`.
#' @param mortality Data frame with `year`, `age` (single years) and `mx`
#'   (central death rate), such as `get_demog_wpp()$mortality`. Needed for
#'   `"survival"` and `"constrained"`. Years outside the table use the
#'   nearest year.
#' @param births Optional data frame with `year` and `births`, such as
#'   `get_demog_wpp()$rates`, giving the national trend used to extend
#'   cohorts past the last anchor.
#' @param seasonality Optional numeric vector of 12 relative birth
#'   frequencies, January to December. Default `NULL`, births spread evenly
#'   over the days of the year.
#' @param tolerance Relative difference allowed between an anchor and the
#'   population rebuilt from the cohorts. Used by `"constrained"` and to flag
#'   anchors in `anchor_check`. Default `0.05`.
#' @param start,end First and last calendar month of the output, as Dates or
#'   `"YYYY-MM"`. Default: the earliest birth month covered by the anchors,
#'   and the month of the latest anchor date.
#' @param max_age_months Oldest age in months to keep in the output. Default
#'   `179` (under 15 years).
#'
#' @return A list of four data frames and a list:
#'   \describe{
#'     \item{cohorts}{One row per geography, birth month and calendar month:
#'       the `geography` columns, `birth_month`, `month` (first day of each
#'       month), `age_months`, `population` and `basis`}
#'     \item{births}{One row per geography and birth month with the
#'       estimated `births` and `basis` (not returned by `"uniform"`)}
#'     \item{cohort_changes}{One row per cohort and pair of consecutive
#'       anchor dates: the cohort size implied by each anchor,
#'       `observed_change`, `expected_change` from mortality (0 for
#'       `"uniform"`), and `unexplained_change`}
#'     \item{anchor_check}{Each anchor (geography, `ref_date`, `age_lower`
#'       and `age_upper` in months, and `population`) with the population
#'       rebuilt from the cohorts at its reference date, the difference, the
#'       relative difference and `within_tolerance`}
#'     \item{metadata}{The method, the assumptions used and, for
#'       `"constrained"`, the number of iterations}
#'   }
#'
#' @examples
#' pop <- data.frame(
#'   adm1 = "Kano",
#'   year = rep(c(2024, 2025), each = 2),
#'   age = rep(0:1, 2),
#'   pop = c(1180, 1130, 1200, 1150)
#' )
#' cohorts <- reconstruct_cohorts(pop, geography = "adm1", method = "uniform")
#' head(cohorts$cohorts)
#'
#' # with mortality, so cohorts change only through deaths
#' mortality <- data.frame(year = 2023, age = 0:100, mx = 0.01)
#' cohorts <- reconstruct_cohorts(
#'   pop, geography = "adm1", mortality = mortality
#' )
#' cohorts$anchor_check
#' @export
reconstruct_cohorts <- function(
  data,
  geography,
  year = "year",
  age = "age",
  population = "pop",
  ref_date = "01-01",
  method = c("constrained", "survival", "uniform"),
  mortality = NULL,
  births = NULL,
  seasonality = NULL,
  tolerance = 0.05,
  start = NULL,
  end = NULL,
  max_age_months = 179
) {
  method <- match.arg(method)
  if (method != "uniform" && is.null(mortality)) {
    cli::cli_abort(c(
      "{.arg mortality} is needed for method {.val {method}}.",
      "i" = paste(
        "Use {.code get_demog_wpp()$mortality},",
        "or {.code method = \"uniform\"}."
      )
    ))
  }
  season <- .check_seasonality(seasonality)
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    cli::cli_abort("{.arg tolerance} must be a single number, 0 or more.")
  }

  anchors <- .make_anchors(data, geography, year, age, population, ref_date)
  anchors <- .check_anchors(anchors)
  key <- .geography_key(anchors, geography)
  geo_lookup <- anchors[!duplicated(key), geography, drop = FALSE]
  geo_lookup$.key <- key[!duplicated(key)]
  anchors$.key <- key
  anchors$.anchor <- seq_len(nrow(anchors))

  pieces <- .anchor_pieces(anchors)
  start_index <- if (is.null(start)) {
    min(pieces$birth_index)
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

  survival <- if (method == "uniform") {
    NULL
  } else {
    .survival_table(
      mortality,
      first_birth = min(pieces$birth_index),
      last_month = max(end_index, .month_index(max(anchors$ref_date)))
    )
  }
  pieces$survival <- .survival_at(
    survival, pieces$birth_index, pieces$ref_index - pieces$birth_index
  )
  pieces <- .share_anchors(pieces, season)
  obs <- .cohort_observations(pieces)
  obs$survival <- .survival_at(
    survival, obs$birth_index, obs$ref_index - obs$birth_index
  )

  iterations <- NA_integer_
  if (method == "uniform") {
    panel <- .uniform_panel(obs, start_index, end_index, max_age_months)
    fitted <- obs[, c(".key", "birth_index", "ref_index")]
    fitted$fitted <- obs$population
    birth_table <- NULL
  } else {
    births_est <- .births_from_pieces(pieces)
    if (method == "constrained") {
      fit <- .constrain_births(births_est, pieces, tolerance)
      births_est <- fit$births
      iterations <- fit$iterations
    }
    births_est$basis <- "anchored"
    births_est <- rbind(
      births_est,
      .extend_births(births_est, end_index, season, births)
    )
    panel <- .survival_panel(
      births_est, survival, start_index, end_index, max_age_months
    )
    fitted <- obs[, c(".key", "birth_index", "ref_index")]
    fitted$fitted <- births_est$births[
      match(
        paste(fitted$.key, fitted$birth_index),
        paste(births_est$.key, births_est$birth_index)
      )
    ] * obs$survival
    birth_table <- merge(geo_lookup, births_est, by = ".key")
    birth_table <- birth_table[
      order(birth_table$.key, birth_table$birth_index),
    ]
    birth_table <- data.frame(
      birth_table[, geography, drop = FALSE],
      birth_month = .month_from_index(birth_table$birth_index),
      births = birth_table$births,
      basis = birth_table$basis
    )
    rownames(birth_table) <- NULL
  }

  panel <- merge(geo_lookup, panel, by = ".key")
  panel <- panel[order(panel$.key, panel$birth_index, panel$month_index), ]
  cohorts <- data.frame(
    panel[, geography, drop = FALSE],
    birth_month = .month_from_index(panel$birth_index),
    month = .month_from_index(panel$month_index),
    age_months = panel$month_index - panel$birth_index,
    population = panel$population,
    basis = panel$basis
  )
  rownames(cohorts) <- NULL

  changes <- merge(geo_lookup, .cohort_changes(obs), by = ".key")
  changes <- changes[order(changes$.key, changes$birth_index), ]
  changes <- data.frame(
    changes[, geography, drop = FALSE],
    birth_month = .month_from_index(changes$birth_index),
    changes[, c(
      "from_date", "to_date", "from_population", "to_population",
      "observed_change", "expected_change", "unexplained_change"
    )]
  )
  rownames(changes) <- NULL

  out <- list(
    cohorts = cohorts,
    births = birth_table,
    cohort_changes = changes,
    anchor_check = .anchor_check(anchors, pieces, fitted, geography, tolerance),
    metadata = .cohort_metadata(
      method, seasonality, births, tolerance, iterations
    )
  )
  out[!vapply(out, is.null, logical(1))]
}

# internal helper: turn population by single year of age into anchors, with
# age in completed months (upper bound exclusive) and an exact reference date
#' @noRd
.make_anchors <- function(data, geography, year, age, population, ref_date) {
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

  ages <- data[[age]]
  if (!is.numeric(ages) || any(is.na(ages) | ages < 0 | ages %% 1 != 0)) {
    cli::cli_abort("{.arg age} must hold whole years of age, 0 or more.")
  }

  out <- data[, geography, drop = FALSE]
  out$ref_date <- .as_date(paste0(data[[year]], "-", ref_date), "ref_date")
  out$age_lower <- as.integer(data[[age]]) * 12L
  out$age_upper <- out$age_lower + 12L
  out$population <- as.numeric(data[[population]])
  rownames(out) <- NULL
  out
}

# internal helper: check anchor populations
#' @noRd
.check_anchors <- function(anchors) {
  if (any(is.na(anchors$population) | anchors$population < 0)) {
    cli::cli_abort("{.arg population} must be zero or more, without NA.")
  }
  anchors
}

# internal helper: relative birth frequency by calendar month, mean 1
#' @noRd
.check_seasonality <- function(seasonality) {
  if (is.null(seasonality)) {
    return(rep(1, 12))
  }
  if (!is.numeric(seasonality) || length(seasonality) != 12 ||
        anyNA(seasonality) || any(seasonality < 0) || sum(seasonality) == 0) {
    cli::cli_abort(
      "{.arg seasonality} must be 12 non-negative numbers, January first."
    )
  }
  seasonality / mean(seasonality)
}

#' @noRd
.geography_key <- function(data, geography) {
  do.call(paste, c(unname(as.list(data[geography])), sep = "\r"))
}

# internal helper: split each anchor across the birth months its window
# covers. births are in [from, to), and overlap is in days
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

  data.frame(
    .anchor = anchors$.anchor[i],
    .key = anchors$.key[i],
    ref_date = anchors$ref_date[i],
    ref_index = .month_index(anchors$ref_date[i]),
    birth_index = birth_index,
    overlap = as.numeric(pmin(month_end, to[i]) - pmax(month_start, from[i])),
    days = .days_in_month(birth_index),
    anchor_population = anchors$population[i]
  )
}

# internal helper: share each anchor across its birth months by days,
# seasonality and survival to the reference date. count is the anchor's
# population in the piece; cohort_size scales it to the whole birth month
#' @noRd
.share_anchors <- function(pieces, season) {
  weight <- pieces$overlap * season[pieces$birth_index %% 12L + 1L] *
    pieces$survival
  total <- rowsum(weight, pieces$.anchor, reorder = FALSE)
  total <- total[match(pieces$.anchor, rownames(total))]
  pieces$count <- ifelse(
    total > 0, pieces$anchor_population * weight / total, 0
  )
  pieces$cohort_size <- pieces$count * pieces$days / pieces$overlap
  pieces
}

# internal helper: size of each cohort on each anchor date, averaging
# anchors that share a cohort and date by days of overlap
#' @noRd
.cohort_observations <- function(pieces) {
  pieces$weighted <- pieces$cohort_size * pieces$overlap
  obs <- stats::aggregate(
    cbind(weighted, overlap) ~ .key + birth_index + ref_date + ref_index,
    data = pieces,
    FUN = sum
  )
  obs$population <- obs$weighted / obs$overlap
  obs <- obs[order(obs$.key, obs$birth_index, obs$ref_date), ]
  rownames(obs) <- NULL
  obs[, c(".key", "birth_index", "ref_date", "ref_index", "population")]
}

# internal helper: births per cohort implied by its anchors, averaged by
# days of overlap
#' @noRd
.births_from_pieces <- function(pieces) {
  pieces$implied <- ifelse(
    pieces$survival > 0, pieces$cohort_size / pieces$survival, 0
  )
  pieces$weighted <- pieces$implied * pieces$overlap
  births <- stats::aggregate(
    cbind(weighted, overlap) ~ .key + birth_index,
    data = pieces,
    FUN = sum
  )
  births$births <- births$weighted / births$overlap
  births <- births[order(births$.key, births$birth_index), ]
  rownames(births) <- NULL
  births[, c(".key", "birth_index", "births")]
}

# internal helper: births as a weighted geometric mean of what each anchor
# implies, reweighted until stable. anchors whose log error is within the
# tolerance keep full weight and the rest are down-weighted by tolerance /
# error (Huber weights), which limits the pull of outlying anchors while
# keeping a single solution
#' @noRd
.constrain_births <- function(births, pieces, tolerance, max_iter = 100L) {
  cohort <- match(
    paste(pieces$.key, pieces$birth_index),
    paste(births$.key, births$birth_index)
  )
  implied <- ifelse(
    pieces$survival > 0, pieces$cohort_size / pieces$survival, 0
  )
  log_implied <- log(pmax(implied, 1e-9))
  share <- pieces$survival * pieces$overlap / pieces$days
  anchor_ids <- unique(pieces$.anchor)
  anchor_of <- match(pieces$.anchor, anchor_ids)
  target <- pieces$anchor_population[match(anchor_ids, pieces$.anchor)]
  band <- log(1 + tolerance)
  b <- births$births
  iterations <- 0L

  for (iter in seq_len(max_iter)) {
    rebuilt <- as.vector(rowsum(b[cohort] * share, anchor_of))
    error <- abs(log((rebuilt + 1e-9) / (target + 1e-9)))
    weight <- ifelse(error <= band, 1, band / pmax(error, 1e-12))
    w <- weight[anchor_of] * pieces$overlap
    updated <- exp(
      as.vector(rowsum(w * log_implied, cohort)) /
        pmax(as.vector(rowsum(w, cohort)), 1e-12)
    )
    iterations <- iter
    change <- max(abs(updated - b) / pmax(b, 1e-9))
    b <- updated
    if (change < 1e-6) {
      break
    }
  }

  births$births <- b
  list(births = births, iterations = iterations)
}

# internal helper: births for months after each geography's last anchored
# birth month, from the per-day births of its last 12 anchored months,
# adjusted for seasonality and the national births trend
#' @noRd
.extend_births <- function(births, end_index, season, national) {
  pieces <- lapply(split(births, births$.key), function(g) {
    last <- max(g$birth_index)
    if (last >= end_index) {
      return(NULL)
    }
    base <- g[g$birth_index > last - 12L, ]
    per_day <- base$births /
      (season[base$birth_index %% 12L + 1L] * .days_in_month(base$birth_index))
    months <- seq.int(last + 1L, end_index)
    trend <- .births_trend(national, months, base$birth_index)
    data.frame(
      .key = g$.key[1],
      birth_index = months,
      births = mean(per_day) * season[months %% 12L + 1L] *
        .days_in_month(months) * trend,
      basis = "extended"
    )
  })
  out <- do.call(rbind, pieces)
  if (is.null(out)) {
    out <- births[0, ]
    out$basis <- character(0)
  }
  out
}

# internal helper: national births in each month's year relative to the
# mean over the base months' years; 1 when no births table is given
#' @noRd
.births_trend <- function(national, months, base_months) {
  if (is.null(national)) {
    return(rep(1, length(months)))
  }
  if (!all(c("year", "births") %in% names(national))) {
    cli::cli_abort(
      "{.arg births} must have columns {.val year} and {.val births}."
    )
  }
  lookup <- function(index) {
    yr <- index %/% 12L
    yr <- pmin(pmax(yr, min(national$year)), max(national$year))
    national$births[match(yr, national$year)]
  }
  lookup(months) / mean(lookup(base_months))
}

# internal helper: monthly survival from birth, as log cumulative survival
# by birth month (rows) and completed months of age (columns, from 0)
#' @noRd
.survival_table <- function(mortality, first_birth, last_month) {
  needed <- c("year", "age", "mx")
  if (!is.data.frame(mortality) || !all(needed %in% names(mortality))) {
    cli::cli_abort(
      "{.arg mortality} must be a data frame with {.val {needed}}."
    )
  }
  if (anyNA(mortality$mx) || any(mortality$mx < 0)) {
    cli::cli_abort("{.arg mortality} {.val mx} must be 0 or more, without NA.")
  }

  years <- sort(unique(mortality$year))
  ages <- 0:max(mortality$age)
  mx <- matrix(NA_real_, length(ages), length(years))
  mx[cbind(mortality$age + 1L, match(mortality$year, years))] <- mortality$mx
  if (anyNA(mx)) {
    cli::cli_abort(
      "{.arg mortality} must have every age from 0 for each year."
    )
  }

  births <- seq.int(first_birth, last_month)
  max_age <- last_month - first_birth
  log_s <- matrix(0, length(births), max_age + 1L)

  for (a in seq_len(max_age)) {
    month <- births + a - 1L
    yr <- pmin(pmax(month %/% 12L, min(years)), max(years))
    col <- match(yr, years)
    age_months <- a - 1L
    if (age_months < 12L) {
      step <- log(.infant_survival(mx[1, col], age_months))
    } else {
      age_years <- pmin(age_months %/% 12L, max(ages))
      step <- -mx[cbind(age_years + 1L, col)] / 12
    }
    log_s[, a + 1L] <- log_s[, a] + step
  }

  list(first_birth = first_birth, log_s = log_s)
}

# internal helper: monthly survival in the first year of life. the infant
# death probability comes from m0 with the Coale-Demeny separation factor;
# half of infant deaths fall in month 0 and the rest evenly over months 1-11
#' @noRd
.infant_survival <- function(m0, age_months) {
  a0 <- ifelse(m0 >= 0.107, 0.34, 0.049 + 2.742 * m0)
  q0 <- m0 / (1 + (1 - a0) * m0)
  first <- 1 - 0.5 * q0
  if (age_months == 0L) {
    return(first)
  }
  ((1 - q0) / first)^(1 / 11)
}

# internal helper: survival from birth to an age in completed months; 1
# when no survival table is used
#' @noRd
.survival_at <- function(survival, birth_index, age_months) {
  if (is.null(survival)) {
    return(rep(1, length(birth_index)))
  }
  row <- birth_index - survival$first_birth + 1L
  col <- pmax(age_months, 0L) + 1L
  exp(survival$log_s[cbind(row, col)])
}

# internal helper: rows for each cohort's months within the output range
#' @noRd
.panel_rows <- function(.key, birth_index, start_index, end_index,
                        max_age_months) {
  first <- pmax(birth_index, start_index)
  last <- pmin(end_index, birth_index + max_age_months)
  n <- pmax(last - first + 1L, 0L)
  i <- rep(seq_along(birth_index), n)
  data.frame(
    .cohort = i,
    .key = .key[i],
    birth_index = birth_index[i],
    month_index = first[i] + sequence(n) - 1L
  )
}

# internal helper: cohort sizes by month from births and survival
#' @noRd
.survival_panel <- function(births, survival, start_index, end_index,
                            max_age_months) {
  panel <- .panel_rows(
    births$.key, births$birth_index, start_index, end_index, max_age_months
  )
  if (nrow(panel) == 0) {
    cli::cli_abort("No cohorts fall within {.arg start} and {.arg end}.")
  }
  panel$population <- births$births[panel$.cohort] * .survival_at(
    survival, panel$birth_index, panel$month_index - panel$birth_index
  )
  panel$basis <- births$basis[panel$.cohort]
  panel$.cohort <- NULL
  panel
}

# internal helper: cohort sizes by month interpolated along each cohort
# between its anchor dates, holding the nearest value outside them
#' @noRd
.uniform_panel <- function(obs, start_index, end_index, max_age_months) {
  cohort_id <- paste(obs$.key, obs$birth_index)
  cohorts <- obs[!duplicated(cohort_id), c(".key", "birth_index")]
  cid <- match(cohort_id, paste(cohorts$.key, cohorts$birth_index))

  panel <- .panel_rows(
    cohorts$.key, cohorts$birth_index, start_index, end_index, max_age_months
  )
  if (nrow(panel) == 0) {
    cli::cli_abort("No cohorts fall within {.arg start} and {.arg end}.")
  }

  # place each cohort on its own stretch of the number line, then find the
  # anchor dates either side of each month
  offset <- 1e6
  obs_x <- cid * offset + as.numeric(obs$ref_date)
  mid <- as.numeric(.month_from_index(panel$month_index) + 14L)
  panel_x <- panel$.cohort * offset + mid
  ord <- order(obs_x)
  obs_x <- obs_x[ord]
  obs_y <- obs$population[ord]
  obs_c <- cid[ord]

  first_of <- match(seq_len(nrow(cohorts)), obs_c)
  last_of <- length(obs_c) + 1L - match(seq_len(nrow(cohorts)), rev(obs_c))
  lo <- findInterval(panel_x, obs_x)
  lo[lo == 0L | obs_c[pmax(lo, 1L)] != panel$.cohort] <- NA_integer_
  hi <- ifelse(!is.na(lo) & lo < last_of[panel$.cohort], lo + 1L, NA_integer_)

  population <- obs_y[last_of[panel$.cohort]]
  before <- is.na(lo)
  population[before] <- obs_y[first_of[panel$.cohort[before]]]
  between <- !is.na(lo) & !is.na(hi)
  w <- (panel_x[between] - obs_x[lo[between]]) /
    (obs_x[hi[between]] - obs_x[lo[between]])
  population[between] <- obs_y[lo[between]] +
    w * (obs_y[hi[between]] - obs_y[lo[between]])

  panel$population <- population
  panel$basis <- "anchored"
  panel$.cohort <- NULL
  panel
}

# internal helper: change in each cohort between consecutive anchor dates,
# split into the part survival explains and the unexplained remainder
#' @noRd
.cohort_changes <- function(obs) {
  same <- c(FALSE, obs$.key[-1] == obs$.key[-nrow(obs)] &
    obs$birth_index[-1] == obs$birth_index[-nrow(obs)])
  to <- which(same)
  from <- to - 1L
  observed <- obs$population[to] / obs$population[from]
  expected <- obs$survival[to] / obs$survival[from]
  data.frame(
    .key = obs$.key[to],
    birth_index = obs$birth_index[to],
    from_date = obs$ref_date[from],
    to_date = obs$ref_date[to],
    from_population = obs$population[from],
    to_population = obs$population[to],
    observed_change = observed - 1,
    expected_change = expected - 1,
    unexplained_change = observed / expected - 1
  )
}

# internal helper: rebuild each anchor from the fitted cohort sizes on its
# reference date
#' @noRd
.anchor_check <- function(anchors, pieces, fitted, geography, tolerance) {
  matched <- merge(
    pieces[, c(".anchor", ".key", "birth_index", "ref_index", "overlap",
               "days")],
    fitted,
    by = c(".key", "birth_index", "ref_index")
  )
  matched$rebuilt <- matched$fitted * matched$overlap / matched$days
  rebuilt <- rowsum(matched$rebuilt, matched$.anchor)

  out <- anchors[, c(geography, "ref_date", "age_lower", "age_upper",
                     "population")]
  out$rebuilt <- as.vector(rebuilt)[
    match(anchors$.anchor, as.integer(rownames(rebuilt)))
  ]
  out$difference <- out$rebuilt - out$population
  out$relative_difference <- ifelse(
    out$population > 0, out$difference / out$population, NA_real_
  )
  out$within_tolerance <- abs(out$relative_difference) <= tolerance + 1e-9
  rownames(out) <- NULL
  out
}

#' @noRd
.cohort_metadata <- function(method, seasonality, births, tolerance,
                             iterations) {
  list(
    method = method,
    births_within_year = if (is.null(seasonality)) {
      "uniform by day"
    } else {
      "by day, weighted by seasonality"
    },
    mortality = if (method == "uniform") {
      "none; all changes between anchors are unexplained"
    } else {
      paste(
        "national mx by single year of age; infant deaths half in month 0",
        "(Coale-Demeny a0), the rest evenly over months 1-11"
      )
    },
    migration = "not modelled; reported as unexplained cohort change",
    extended_cohorts = if (method == "uniform") {
      "none"
    } else if (is.null(births)) {
      "last 12 anchored months, held at their level"
    } else {
      "last 12 anchored months, following national births"
    },
    tolerance = tolerance,
    iterations = iterations
  )
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
