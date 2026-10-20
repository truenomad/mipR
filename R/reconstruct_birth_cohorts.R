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
#' @param ref_date Reference date of the estimates within each year: either
#'   one `"MM-DD"` for every row, or the name of a column of `data` holding
#'   each row's `"MM-DD"`. Use a column when sources with different
#'   reference dates are combined, for example WorldPop legacy years (1
#'   July) with R2025A years (1 January); [worldpop_reference_date()] fills
#'   it from the year. Default `"01-01"`, the WorldPop R2025A reference date.
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
#' cohorts <- reconstruct_birth_cohorts(
#'   pop, geography = "adm1", method = "uniform"
#' )
#' head(cohorts$cohorts)
#'
#' # with mortality, so cohorts change only through deaths
#' mortality <- data.frame(year = 2023, age = 0:100, mx = 0.01)
#' cohorts <- reconstruct_birth_cohorts(
#'   pop, geography = "adm1", mortality = mortality
#' )
#' cohorts$anchor_check
#' @export
reconstruct_birth_cohorts <- function(
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

  anchors <- .make_anchors(data, geography, year, age, population, ref_date) |>
    .check_anchors()

  # one integer key per geography, in sorted order
  geo_lookup <- anchors |>
    dplyr::distinct(dplyr::across(dplyr::all_of(geography))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(geography))) |>
    dplyr::mutate(.key = dplyr::row_number())
  anchors <- anchors |>
    dplyr::left_join(geo_lookup, by = geography) |>
    dplyr::mutate(.anchor = dplyr::row_number())

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
  pieces <- pieces |>
    dplyr::mutate(age_months = .data$ref_index - .data$birth_index) |>
    .add_survival(survival) |>
    .share_anchors(season)
  obs <- .cohort_observations(pieces) |>
    dplyr::mutate(age_months = .data$ref_index - .data$birth_index) |>
    .add_survival(survival) |>
    dplyr::select(-"age_months")

  iterations <- NA_integer_
  if (method == "uniform") {
    panel <- .uniform_panel(obs, start_index, end_index, max_age_months)
    fitted <- obs |>
      dplyr::select(".key", "birth_index", "ref_index", fitted = "population")
    birth_table <- NULL
  } else {
    births_est <- .births_from_pieces(pieces)
    if (method == "constrained") {
      fit <- .constrain_births(births_est, pieces, tolerance)
      births_est <- fit$births
      iterations <- fit$iterations
    }
    births_est <- dplyr::mutate(births_est, basis = "anchored")
    births_est <- dplyr::bind_rows(
      births_est,
      .extend_births(births_est, end_index, season, births)
    )
    panel <- .survival_panel(
      births_est, survival, start_index, end_index, max_age_months
    )
    fitted <- obs |>
      dplyr::left_join(
        dplyr::select(births_est, ".key", "birth_index", "births"),
        by = c(".key", "birth_index")
      ) |>
      dplyr::transmute(
        .data$.key, .data$birth_index, .data$ref_index,
        fitted = .data$births * .data$survival
      )
    birth_table <- births_est |>
      dplyr::inner_join(geo_lookup, by = ".key") |>
      dplyr::arrange(.data$.key, .data$birth_index) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(geography)),
        birth_month = .month_from_index(.data$birth_index),
        .data$births,
        .data$basis
      )
  }

  cohorts <- panel |>
    dplyr::inner_join(geo_lookup, by = ".key") |>
    dplyr::arrange(.data$.key, .data$birth_index, .data$month_index) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(geography)),
      birth_month = .month_from_index(.data$birth_index),
      month = .month_from_index(.data$month_index),
      age_months = .data$month_index - .data$birth_index,
      .data$population,
      .data$basis
    )

  changes <- .cohort_changes(obs) |>
    dplyr::inner_join(geo_lookup, by = ".key") |>
    dplyr::arrange(.data$.key, .data$birth_index) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(geography)),
      birth_month = .month_from_index(.data$birth_index),
      dplyr::across(c(
        "from_date", "to_date", "from_population", "to_population",
        "observed_change", "expected_change", "unexplained_change"
      ))
    )

  list(
    cohorts = cohorts,
    births = birth_table,
    cohort_changes = changes,
    anchor_check = .anchor_check(anchors, pieces, fitted, geography, tolerance),
    metadata = .cohort_metadata(
      method, seasonality, births, tolerance, iterations
    )
  ) |>
    purrr::compact()
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
  if (length(ref_date) != 1) {
    cli::cli_abort("{.arg ref_date} must be a single value.")
  }
  ref_values <- if (ref_date %in% names(data)) {
    as.character(data[[ref_date]])
  } else {
    rep(ref_date, nrow(data))
  }
  if (anyNA(ref_values) || !all(grepl("^\\d{2}-\\d{2}$", ref_values))) {
    cli::cli_abort(paste0(
      "{.arg ref_date} must be {.val MM-DD}, such as {.val 01-01}, or a ",
      "column of {.arg data} holding {.val MM-DD} values."
    ))
  }

  ages <- data[[age]]
  if (!is.numeric(ages) || any(is.na(ages) | ages < 0 | ages %% 1 != 0)) {
    cli::cli_abort("{.arg age} must hold whole years of age, 0 or more.")
  }

  # take the values out first so argument names cannot mask columns
  anchor_dates <- .as_date(paste0(data[[year]], "-", ref_values), "ref_date")
  anchor_pop <- as.numeric(data[[population]])

  # a new table of the geography columns, without the input's attributes
  purrr::set_names(geography) |>
    purrr::map(\(column) data[[column]]) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      ref_date = anchor_dates,
      age_lower = as.integer(ages) * 12L,
      age_upper = .data$age_lower + 12L,
      population = anchor_pop
    )
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

# internal helper: relative birth frequency of the calendar month of each
# month index
#' @noRd
.season_at <- function(season, index) {
  season[index %% 12L + 1L]
}

# internal helper: split each anchor across the birth months its window
# covers. births are in [from, to), and overlap is in days
#' @noRd
.anchor_pieces <- function(anchors) {
  windows <- anchors |>
    dplyr::mutate(
      from = .add_months(.data$ref_date, -.data$age_upper),
      to = .add_months(.data$ref_date, -.data$age_lower),
      first = .month_index(.data$from),
      last = .month_index(.data$to - 1L)
    )
  months <- dplyr::tibble(
    birth_index = seq.int(min(windows$first), max(windows$last))
  )

  windows |>
    dplyr::inner_join(
      months,
      by = dplyr::join_by("first" <= "birth_index", "last" >= "birth_index")
    ) |>
    dplyr::transmute(
      .data$.anchor,
      .data$.key,
      .data$ref_date,
      ref_index = .month_index(.data$ref_date),
      .data$birth_index,
      overlap = as.numeric(
        pmin(.month_from_index(.data$birth_index + 1L), .data$to) -
          pmax(.month_from_index(.data$birth_index), .data$from)
      ),
      days = .days_in_month(.data$birth_index),
      anchor_population = .data$population
    )
}

# internal helper: share each anchor across its birth months by days,
# seasonality and survival to the reference date. count is the anchor's
# population in the piece; cohort_size scales it to the whole birth month
#' @noRd
.share_anchors <- function(pieces, season) {
  pieces |>
    dplyr::mutate(
      weight = .data$overlap * .season_at(season, .data$birth_index) *
        .data$survival
    ) |>
    dplyr::mutate(
      dplyr::across("weight", sum, .names = "total"),
      .by = ".anchor"
    ) |>
    dplyr::mutate(
      count = dplyr::if_else(
        .data$total > 0,
        .data$anchor_population * .data$weight / .data$total,
        0
      ),
      cohort_size = .data$count * .data$days / .data$overlap
    ) |>
    dplyr::select(-"weight", -"total")
}

# internal helper: size of each cohort on each anchor date, averaging
# anchors that share a cohort and date by days of overlap
#' @noRd
.cohort_observations <- function(pieces) {
  pieces |>
    dplyr::mutate(weighted = .data$cohort_size * .data$overlap) |>
    dplyr::summarise(
      dplyr::across(c("weighted", "overlap"), sum),
      .by = c(".key", "birth_index", "ref_date", "ref_index")
    ) |>
    dplyr::mutate(population = .data$weighted / .data$overlap) |>
    dplyr::arrange(.data$.key, .data$birth_index, .data$ref_date) |>
    dplyr::select(".key", "birth_index", "ref_date", "ref_index", "population")
}

# internal helper: births a piece implies, its cohort size undone for
# survival to the reference date
#' @noRd
.implied_births <- function(pieces) {
  pieces |>
    dplyr::mutate(
      implied = dplyr::if_else(
        .data$survival > 0, .data$cohort_size / .data$survival, 0
      )
    )
}

# internal helper: births per cohort implied by its anchors, averaged by
# days of overlap
#' @noRd
.births_from_pieces <- function(pieces) {
  pieces |>
    .implied_births() |>
    dplyr::mutate(weighted = .data$implied * .data$overlap) |>
    dplyr::summarise(
      dplyr::across(c("weighted", "overlap"), sum),
      .by = c(".key", "birth_index")
    ) |>
    dplyr::mutate(births = .data$weighted / .data$overlap) |>
    dplyr::arrange(.data$.key, .data$birth_index) |>
    dplyr::select(".key", "birth_index", "births")
}

# internal helper: births as a weighted geometric mean of what each anchor
# implies, reweighted until stable. anchors whose log error is within the
# tolerance keep full weight and the rest are down-weighted by tolerance /
# error (Huber weights), which limits the pull of outlying anchors while
# keeping a single solution
#' @noRd
.constrain_births <- function(births, pieces, tolerance, max_iter = 100L) {
  band <- log(1 + tolerance)
  fit <- pieces |>
    .implied_births() |>
    dplyr::left_join(births, by = c(".key", "birth_index")) |>
    dplyr::transmute(
      .data$.anchor,
      .data$.key,
      .data$birth_index,
      .data$overlap,
      .data$births,
      log_implied = log(pmax(.data$implied, 1e-9)),
      share = .data$survival * .data$overlap / .data$days,
      target = .data$anchor_population
    )

  # stop early once births change by less than the convergence limit
  result <- purrr::reduce(
    seq_len(max_iter),
    \(state, iter) {
      fit <- .huber_step(state$fit, band)
      change <- max(abs(fit$updated - fit$births) / pmax(fit$births, 1e-9))
      fit <- fit |>
        dplyr::mutate(births = .data$updated) |>
        dplyr::select(-"updated")
      out <- list(fit = fit, iterations = iter)
      if (change < 1e-6) purrr::done(out) else out
    },
    .init = list(fit = fit, iterations = 0L)
  )

  fitted <- result$fit |>
    dplyr::select(".key", "birth_index", "births") |>
    dplyr::distinct()
  births <- births |>
    dplyr::select(".key", "birth_index") |>
    dplyr::left_join(fitted, by = c(".key", "birth_index"))
  list(births = births, iterations = result$iterations)
}

# internal helper: one reweighting step. rebuild each anchor from the
# current births, weight anchors by their log error, then refit each cohort
#' @noRd
.huber_step <- function(fit, band) {
  fit |>
    dplyr::mutate(contribution = .data$births * .data$share) |>
    dplyr::mutate(
      dplyr::across("contribution", sum, .names = "rebuilt"),
      .by = ".anchor"
    ) |>
    dplyr::mutate(
      error = abs(log((.data$rebuilt + 1e-9) / (.data$target + 1e-9))),
      w = dplyr::if_else(
        .data$error <= band, 1, band / pmax(.data$error, 1e-12)
      ) * .data$overlap,
      w_log = .data$w * .data$log_implied
    ) |>
    dplyr::mutate(
      dplyr::across(c("w_log", "w"), sum, .names = "{.col}_sum"),
      .by = c(".key", "birth_index")
    ) |>
    dplyr::mutate(
      updated = exp(.data$w_log_sum / pmax(.data$w_sum, 1e-12))
    ) |>
    dplyr::select(
      -"contribution", -"rebuilt", -"error", -"w", -"w_log",
      -"w_log_sum", -"w_sum"
    )
}

# internal helper: births for months after each geography's last anchored
# birth month, from the per-day births of its last 12 anchored months,
# adjusted for seasonality and the national births trend
#' @noRd
.extend_births <- function(births, end_index, season, national) {
  last_anchored <- births |>
    dplyr::summarise(
      dplyr::across("birth_index", max, .names = "last"),
      .by = ".key"
    ) |>
    dplyr::filter(.data$last < end_index)
  if (nrow(last_anchored) == 0) {
    return(dplyr::slice(births, 0))
  }

  # per-day births and national births over each geography's last year of
  # anchored months
  base <- births |>
    dplyr::inner_join(last_anchored, by = ".key") |>
    dplyr::filter(.data$birth_index > .data$last - 12L) |>
    dplyr::mutate(
      per_day = .data$births / (
        .season_at(season, .data$birth_index) *
          .days_in_month(.data$birth_index)
      ),
      national_births = .national_births(national, .data$birth_index)
    ) |>
    dplyr::summarise(
      dplyr::across(
        c("per_day", "national_births"), mean, .names = "base_{.col}"
      ),
      .by = c(".key", "last")
    )

  months <- dplyr::tibble(
    birth_index = seq.int(min(base$last) + 1L, end_index)
  )
  base |>
    dplyr::inner_join(months, by = dplyr::join_by("last" < "birth_index")) |>
    dplyr::mutate(
      trend = .national_births(national, .data$birth_index) /
        .data$base_national_births,
      births = .data$base_per_day * .season_at(season, .data$birth_index) *
        .days_in_month(.data$birth_index) * .data$trend,
      basis = "extended"
    ) |>
    dplyr::select(".key", "birth_index", "births", "basis")
}

# internal helper: national births in the year of each month index, using
# the nearest year in the table; 1 when no births table is given
#' @noRd
.national_births <- function(national, index) {
  if (is.null(national)) {
    return(rep(1, length(index)))
  }
  if (!all(c("year", "births") %in% names(national))) {
    cli::cli_abort(
      "{.arg births} must have columns {.val year} and {.val births}."
    )
  }
  by_year <- national |>
    dplyr::filter(!duplicated(.data$year)) |>
    dplyr::select("year", "births")

  dplyr::tibble(
    year = pmin(pmax(index %/% 12L, min(national$year)), max(national$year))
  ) |>
    dplyr::left_join(by_year, by = "year") |>
    dplyr::pull("births")
}

# internal helper: monthly survival from birth, one row per birth month and
# completed month of age (from 0) up to the last month needed
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

  # every single age from 0 for each year; a repeated row uses the last
  rates <- tidyr::expand_grid(
    year = sort(unique(mortality$year)),
    age = 0:max(mortality$age)
  ) |>
    dplyr::left_join(
      mortality |>
        dplyr::summarise(
          dplyr::across("mx", dplyr::last),
          .by = c("year", "age")
        ),
      by = c("year", "age")
    )
  if (anyNA(rates$mx)) {
    cli::cli_abort(
      "{.arg mortality} must have every age from 0 for each year."
    )
  }
  first_year <- min(rates$year)
  last_year <- max(rates$year)
  oldest <- max(rates$age)
  infant <- rates |>
    dplyr::filter(.data$age == 0) |>
    dplyr::select("year", m0 = "mx")

  # log survival over each month of age, summed along each birth cohort
  steps <- tidyr::expand_grid(
    birth_index = seq.int(first_birth, last_month),
    age_months = seq_len(last_month - first_birth) - 1L
  ) |>
    dplyr::filter(.data$birth_index + .data$age_months < last_month) |>
    dplyr::mutate(
      year = pmin(
        pmax((.data$birth_index + .data$age_months) %/% 12L, first_year),
        last_year
      ),
      rate_age = pmin(.data$age_months %/% 12L, oldest)
    ) |>
    dplyr::left_join(infant, by = "year") |>
    dplyr::left_join(rates, by = c("year", rate_age = "age")) |>
    dplyr::mutate(
      step = dplyr::if_else(
        .data$age_months < 12L,
        log(.infant_survival(.data$m0, .data$age_months)),
        -.data$mx / 12
      )
    ) |>
    dplyr::arrange(.data$birth_index, .data$age_months) |>
    dplyr::mutate(
      dplyr::across("step", cumsum, .names = "log_s"),
      .by = "birth_index"
    ) |>
    dplyr::transmute(
      .data$birth_index,
      age_months = .data$age_months + 1L,
      .data$log_s
    )

  dplyr::tibble(
    birth_index = seq.int(first_birth, last_month),
    age_months = 0L,
    log_s = 0
  ) |>
    dplyr::bind_rows(steps) |>
    dplyr::transmute(
      .data$birth_index,
      .data$age_months,
      month_index = .data$birth_index + .data$age_months,
      survival = exp(.data$log_s)
    )
}

# internal helper: monthly survival in the first year of life. the infant
# death probability comes from m0 with the Coale-Demeny separation factor;
# half of infant deaths fall in month 0 and the rest evenly over months 1-11
#' @noRd
.infant_survival <- function(m0, age_months) {
  a0 <- dplyr::if_else(m0 >= 0.107, 0.34, 0.049 + 2.742 * m0)
  q0 <- m0 / (1 + (1 - a0) * m0)
  first <- 1 - 0.5 * q0
  first_month <- rep_len(
    age_months == 0L, max(length(m0), length(age_months))
  )
  dplyr::if_else(first_month, first, ((1 - q0) / first)^(1 / 11))
}

# internal helper: add survival from birth to `age_months` (completed
# months); 1 when no survival table is used
#' @noRd
.add_survival <- function(data, survival) {
  if (is.null(survival)) {
    return(dplyr::mutate(data, survival = 1))
  }
  data |>
    dplyr::mutate(.age = pmax(.data$age_months, 0L)) |>
    dplyr::left_join(
      dplyr::select(survival, "birth_index", .age = "age_months", "survival"),
      by = c("birth_index", ".age")
    ) |>
    dplyr::select(-".age")
}

# internal helper: rows for each cohort's months within the output range
#' @noRd
.panel_rows <- function(cohorts, start_index, end_index, max_age_months) {
  months <- dplyr::tibble(month_index = seq.int(start_index, end_index))
  cohorts |>
    dplyr::mutate(
      first = pmax(.data$birth_index, start_index),
      last = pmin(end_index, .data$birth_index + max_age_months)
    ) |>
    dplyr::inner_join(
      months,
      by = dplyr::join_by("first" <= "month_index", "last" >= "month_index")
    ) |>
    dplyr::select(-"first", -"last")
}

# internal helper: cohort sizes by month from births and survival
#' @noRd
.survival_panel <- function(births, survival, start_index, end_index,
                            max_age_months) {
  in_range <- survival |>
    dplyr::filter(
      .data$month_index >= start_index,
      .data$month_index <= end_index,
      .data$age_months <= max_age_months
    )
  panel <- births |>
    dplyr::inner_join(
      in_range,
      by = "birth_index", relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$.key,
      .data$birth_index,
      .data$month_index,
      population = .data$births * .data$survival,
      .data$basis
    )
  if (nrow(panel) == 0) {
    cli::cli_abort("No cohorts fall within {.arg start} and {.arg end}.")
  }
  panel
}

# internal helper: cohort sizes by month interpolated along each cohort
# between its anchor dates, holding the nearest value outside them
#' @noRd
.uniform_panel <- function(obs, start_index, end_index, max_age_months) {
  cohorts <- obs |>
    dplyr::select(".key", "birth_index") |>
    dplyr::distinct()
  panel <- .panel_rows(cohorts, start_index, end_index, max_age_months)
  if (nrow(panel) == 0) {
    cli::cli_abort("No cohorts fall within {.arg start} and {.arg end}.")
  }

  # the anchor dates at or before and after the middle of each month
  points <- obs |>
    dplyr::transmute(
      .data$.key,
      .data$birth_index,
      x = as.numeric(.data$ref_date),
      y = .data$population
    )
  before <- dplyr::rename(points, x_before = "x", y_before = "y")
  after <- dplyr::rename(points, x_after = "x", y_after = "y")

  panel |>
    dplyr::mutate(
      mid = as.numeric(.month_from_index(.data$month_index) + 14L)
    ) |>
    dplyr::left_join(
      before,
      by = dplyr::join_by(".key", "birth_index", closest("mid" >= "x_before"))
    ) |>
    dplyr::left_join(
      after,
      by = dplyr::join_by(".key", "birth_index", closest("mid" < "x_after"))
    ) |>
    dplyr::mutate(
      population = dplyr::case_when(
        is.na(.data$x_before) ~ .data$y_after,
        is.na(.data$x_after) ~ .data$y_before,
        .default = .data$y_before +
          (.data$mid - .data$x_before) / (.data$x_after - .data$x_before) *
            (.data$y_after - .data$y_before)
      ),
      basis = "anchored"
    ) |>
    dplyr::select(".key", "birth_index", "month_index", "population", "basis")
}

# internal helper: change in each cohort between consecutive anchor dates,
# split into the part survival explains and the unexplained remainder
#' @noRd
.cohort_changes <- function(obs) {
  obs |>
    dplyr::mutate(
      same_cohort = .data$.key == dplyr::lag(.data$.key) &
        .data$birth_index == dplyr::lag(.data$birth_index),
      from_date = dplyr::lag(.data$ref_date),
      from_population = dplyr::lag(.data$population),
      from_survival = dplyr::lag(.data$survival)
    ) |>
    dplyr::filter(.data$same_cohort) |>
    dplyr::mutate(
      observed = .data$population / .data$from_population,
      expected = .data$survival / .data$from_survival
    ) |>
    dplyr::transmute(
      .data$.key,
      .data$birth_index,
      .data$from_date,
      to_date = .data$ref_date,
      .data$from_population,
      to_population = .data$population,
      observed_change = .data$observed - 1,
      expected_change = .data$expected - 1,
      unexplained_change = .data$observed / .data$expected - 1
    )
}

# internal helper: rebuild each anchor from the fitted cohort sizes on its
# reference date
#' @noRd
.anchor_check <- function(anchors, pieces, fitted, geography, tolerance) {
  rebuilt <- pieces |>
    dplyr::select(
      ".anchor", ".key", "birth_index", "ref_index", "overlap", "days"
    ) |>
    dplyr::inner_join(
      fitted,
      by = c(".key", "birth_index", "ref_index"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(rebuilt = .data$fitted * .data$overlap / .data$days) |>
    dplyr::summarise(dplyr::across("rebuilt", sum), .by = ".anchor")

  anchors |>
    dplyr::left_join(rebuilt, by = ".anchor") |>
    dplyr::mutate(
      difference = .data$rebuilt - .data$population,
      relative_difference = dplyr::if_else(
        .data$population > 0,
        .data$difference / .data$population,
        NA_real_
      ),
      within_tolerance = abs(.data$relative_difference) <= tolerance + 1e-9
    ) |>
    dplyr::select(
      dplyr::all_of(geography), "ref_date", "age_lower", "age_upper",
      "population", "rebuilt", "difference", "relative_difference",
      "within_tolerance"
    )
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
  midmonth <- convention == "midmonth"
  age_min <- age_min_months
  age_max <- age_max_months

  # one row per day of birth in each distinct birth month
  shares <- dplyr::tibble(index = unique(index)) |>
    dplyr::mutate(
      n_days = if (midmonth) 1L else .days_in_month(.data$index)
    ) |>
    tidyr::uncount(.data$n_days, .id = "day") |>
    dplyr::mutate(
      birth_date = .month_from_index(.data$index) +
        if (midmonth) 14L else .data$day - 1L,
      old_enough = .add_months(.data$birth_date, age_min) <= end,
      young_enough = if (is.finite(age_max)) {
        .add_months(.data$birth_date, age_max) > start
      } else {
        TRUE
      }
    ) |>
    dplyr::summarise(
      share = mean(.data$old_enough & .data$young_enough),
      .by = "index"
    )

  dplyr::tibble(index = index) |>
    dplyr::left_join(shares, by = "index") |>
    dplyr::pull("share")
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
