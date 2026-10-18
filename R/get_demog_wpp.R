#' Get Demographic Inputs from UN WPP 2024
#'
#' Pulls the single-year age structure and vital rates for one country from
#' the UN World Population Prospects 2024 and returns an age distribution,
#' crude birth and death rates, and a daily birth rate for each year.
#'
#' @details
#' The age distribution is the cumulative share of the population by single
#' year of age, with ages expressed in days, in the layout used by EMOD's
#' `AgeDistribution`.
#'
#' Years up to 2023 come from the WPP estimates and later years from the
#' medium-variant projections, so 1950 to 2100 is covered. A year outside
#' that range is replaced by the closest available year, with a warning.
#'
#' The data come from the wpp2024 package, which is not on CRAN. Install it
#' with `remotes::install_github("PPgp/wpp2024")`.
#'
#' @param country A single ISO3 code such as `"NGA"`, or a country name as
#'   used in wpp2024 such as `"Nigeria"`. Case is ignored. A name that does
#'   not match exactly is accepted if it matches part of exactly one name.
#' @param year Reference year, or a vector of years.
#'
#' @return For a single year, a named list:
#'   \describe{
#'     \item{age_distribution}{List with `DistributionValues` (cumulative
#'       share), `ResultScaleFactor` and `ResultValues` (age in days)}
#'     \item{crude_birth_rate}{Annual births per 1,000 population}
#'     \item{crude_death_rate}{Annual deaths per 1,000 population}
#'     \item{birth_rate_daily}{Crude birth rate divided by 365, for the EMOD
#'       `BirthRate` node attribute}
#'     \item{country}{Matched country name}
#'     \item{year}{Reference year used}
#'   }
#'   For several years, a list of these, named by year.
#'
#' @examples
#' \dontrun{
#' demog <- get_demog_wpp("BDI", 2022)
#' demog$crude_birth_rate
#'
#' # one entry per year
#' demog_nga <- get_demog_wpp("Nigeria", 2000:2026)
#' demog_nga[["2026"]]$crude_birth_rate
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
  misc_cols <- c("country_code", "name", "year", "cbr", "cdr")
  misc <- rbind(
    .load_wpp("misc1dt")[, misc_cols],
    .load_wpp("miscproj1dt")[, misc_cols]
  )

  locations <- unique(pop_age[, c("country_code", "name")])
  country_name <- .match_wpp_country(country, locations)
  pop_age <- pop_age[pop_age$name == country_name, ]
  misc <- misc[misc$name == country_name, ]

  out <- lapply(year, function(yr) .demog_one_year(pop_age, misc, yr))
  if (length(out) == 1) {
    return(out[[1]])
  }
  stats::setNames(out, year)
}

# internal helper: age distribution and vital rates for one country-year
#' @noRd
.demog_one_year <- function(pop_age, misc, year) {
  country_name <- pop_age$name[1]

  available_years <- intersect(unique(pop_age$year), unique(misc$year))
  if (!year %in% available_years) {
    closest <- .closest_year(available_years, year)
    cli::cli_alert_warning(
      "Year {year} not available. Using closest: {closest}"
    )
    year <- closest
  }

  age_data <- pop_age[pop_age$year == year, ]
  age_data <- age_data[order(age_data$age), ]

  prop <- age_data$pop / sum(age_data$pop, na.rm = TRUE)
  age_distribution <- list(
    DistributionValues = cumsum(prop),
    ResultScaleFactor = 1,
    ResultValues = age_data$age * 365
  )

  vital <- misc[misc$year == year, ]
  crude_birth_rate <- vital$cbr[1]
  crude_death_rate <- vital$cdr[1]

  cli::cli_alert_success(paste0(
    "{country_name} {year}: CBR {round(crude_birth_rate, 2)}, ",
    "CDR {round(crude_death_rate, 2)} (per 1,000 per year)"
  ))

  list(
    age_distribution = age_distribution,
    crude_birth_rate = crude_birth_rate,
    crude_death_rate = crude_death_rate,
    birth_rate_daily = crude_birth_rate / 365,
    country = country_name,
    year = year
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

# internal helper: match an ISO3 code or a country name to a wpp2024 name.
# locations has columns country_code (UN M49) and name
#' @noRd
.match_wpp_country <- function(country, locations) {
  if (grepl("^[A-Za-z]{3}$", country)) {
    m49 <- suppressWarnings(
      countrycode::countrycode(toupper(country), "iso3c", "un")
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
