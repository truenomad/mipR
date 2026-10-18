#' Get Demographic Inputs from UN WPP 2024
#'
#' Pulls the single-year age structure, vital rates and mortality for one
#' country from the UN World Population Prospects 2024 and returns them as
#' three data frames: rates by year, and age distribution and mortality by
#' year and single year of age.
#'
#' @details
#' Years up to 2023 come from the WPP estimates and later years from the
#' medium-variant projections, so 1950 to 2100 is covered. A year outside
#' that range is replaced by the closest available year, with a warning.
#' The single-year population in wpp2024 refers to 31 December of each year.
#'
#' Births and mortality rates refer to the calendar year. `mortality` can
#' be passed to [reconstruct_cohorts()] as its `mortality` argument and
#' `rates` as its `births` argument.
#'
#' `age_days` is the upper end of each year of age in days, so
#' `cumulative_proportion` is the share of the population younger than
#' `age_days`. Age 100 means 100 and over.
#'
#' The data come from the wpp2024 package, which is not on CRAN. Install it
#' with `remotes::install_github("PPgp/wpp2024")`.
#'
#' @param country A single ISO3 code such as `"NGA"`, or a country name as
#'   used in wpp2024 such as `"Nigeria"`. Case is ignored. A name that does
#'   not match exactly is accepted if it matches part of exactly one name.
#' @param year Reference year, or a vector of years.
#'
#' @return A list of three data frames:
#'   \describe{
#'     \item{rates}{One row per year with `country` (upper case), `iso3`,
#'       `year`, `births` (live births in the year), `crude_birth_rate` and
#'       `crude_death_rate` (per 1,000 population per year) and
#'       `birth_rate_daily` (births per person per day)}
#'     \item{age_distribution}{One row per year and single year of age (0 to
#'       100) with `country`, `iso3`, `year`, `age`, `proportion`,
#'       `cumulative_proportion` and `age_days`}
#'     \item{mortality}{One row per year and single year of age (0 to 100)
#'       with `country`, `iso3`, `year`, `age` and `mx`, the central death
#'       rate for both sexes (deaths per person-year)}
#'   }
#'
#' @examples
#' \dontrun{
#' demog <- get_demog_wpp("NGA", 2000:2026)
#' demog$rates
#' demog$age_distribution
#' }
#' @export
get_demog_wpp <- function(country, year) {
  if (!requireNamespace("wpp2024", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg wpp2024} package is needed to pull UN WPP 2024 data.",
      "i" = "Install it with {.code remotes::install_github(\"PPgp/wpp2024\")}"
    ))
  }
  if (length(country) != 1) {
    cli::cli_abort("{.arg country} must be a single country.")
  }

  # estimates run to 2023; projections (medium variant) from 2024
  pop_cols <- c("country_code", "name", "year", "age", "pop")
  pop_age <- rbind(
    .load_wpp("popAge1dt")[, pop_cols],
    .load_wpp("popprojAge1dt")[, pop_cols]
  )
  misc_cols <- c("country_code", "name", "year", "births", "cbr", "cdr")
  misc <- rbind(
    .load_wpp("misc1dt")[, misc_cols],
    .load_wpp("miscproj1dt")[, misc_cols]
  )

  locations <- unique(pop_age[, c("country_code", "name")])
  country_name <- .match_wpp_country(country, locations)
  pop_age <- pop_age[pop_age$name == country_name, ]
  misc <- misc[misc$name == country_name, ]
  mx <- .load_wpp("mx1dt")
  mx <- mx[mx$name == country_name, c("year", "age", "mxB")]
  iso3 <- suppressWarnings(countrycode::countrycode(
    pop_age$country_code[1], "un", "iso3c",
    custom_match = stats::setNames(names(.wpp_extra_m49), .wpp_extra_m49)
  ))

  available_years <- intersect(unique(pop_age$year), unique(misc$year))
  years_used <- vapply(
    year,
    function(yr) {
      if (yr %in% available_years) {
        return(as.integer(yr))
      }
      closest <- as.integer(.closest_year(available_years, yr))
      cli::cli_alert_warning("Year {yr} not available. Using {closest}")
      closest
    },
    integer(1)
  )

  rates <- misc[match(years_used, misc$year), ]
  rates <- data.frame(
    country = toupper(country_name),
    iso3 = iso3,
    year = years_used,
    births = rates$births * 1000,
    crude_birth_rate = rates$cbr,
    crude_death_rate = rates$cdr,
    birth_rate_daily = rates$cbr / 1000 / 365
  )

  age_distribution <- do.call(rbind, lapply(years_used, function(yr) {
    age_data <- pop_age[pop_age$year == yr, ]
    age_data <- age_data[order(age_data$age), ]
    proportion <- age_data$pop / sum(age_data$pop)
    data.frame(
      country = toupper(country_name),
      iso3 = iso3,
      year = yr,
      age = as.integer(age_data$age),
      proportion = proportion,
      cumulative_proportion = cumsum(proportion),
      age_days = (age_data$age + 1) * 365
    )
  }))
  rownames(age_distribution) <- NULL

  mx <- mx[mx$year %in% years_used, ]
  mx <- mx[order(mx$year, mx$age), ]
  mortality <- data.frame(
    country = toupper(country_name),
    iso3 = iso3,
    year = as.integer(mx$year),
    age = as.integer(mx$age),
    mx = mx$mxB
  )

  cli::cli_alert_success(
    "{country_name}: WPP 2024 rates, age distribution and mortality for \\
     {length(unique(years_used))} year{?s}"
  )

  list(
    rates = rates,
    age_distribution = age_distribution,
    mortality = mortality
  )
}

# internal helper: load one wpp2024 dataset as a data frame. the data files
# are R scripts, so the environment needs base functions
#' @noRd
.load_wpp <- function(name) {
  env <- new.env(parent = baseenv())
  utils::data(list = name, package = "wpp2024", envir = env)
  as.data.frame(env[[name]])
}

# WPP locations that have no UN M49 code in countrycode
.wpp_extra_m49 <- c(TWN = 158, XKX = 412)

# internal helper: match an ISO3 code or a country name to a wpp2024 name.
# locations has columns country_code (UN M49) and name
#' @noRd
.match_wpp_country <- function(country, locations) {
  if (grepl("^[A-Za-z]{3}$", country)) {
    m49 <- suppressWarnings(
      countrycode::countrycode(
        toupper(country), "iso3c", "un",
        custom_match = .wpp_extra_m49
      )
    )
    matched <- locations$name[locations$country_code %in% m49]
    if (length(matched) > 0) {
      return(matched[1])
    }
  }

  available <- locations$name
  matched <- available[tolower(available) == tolower(country)]
  if (length(matched) > 0) {
    return(matched[1])
  }

  matched <- grep(country, available, value = TRUE, ignore.case = TRUE)
  if (length(matched) == 0) {
    cli::cli_abort("Country {.val {country}} not found in wpp2024.")
  }
  if (length(matched) > 1) {
    cli::cli_abort(c(
      "Country {.val {country}} matches several wpp2024 names.",
      "i" = "Use an ISO3 code or one of: {.val {matched}}"
    ))
  }
  cli::cli_alert_info(
    "Partial match: using {.val {matched}} for {.val {country}}"
  )
  matched
}

.closest_year <- function(years, year) {
  years[which.min(abs(years - year))]
}
