#' Get Demographic Inputs from UN WPP 2024
#'
#' Pulls the single-year age structure and vital rates for one country from
#' the UN World Population Prospects 2024 and returns an age distribution,
#' crude birth and death rates, and a daily birth rate.
#'
#' @details
#' The age distribution is the cumulative share of the population by single
#' year of age, with ages expressed in days, in the layout used by EMOD's
#' `AgeDistribution`. If `year` is not available, the closest available year
#' is used and a warning is shown.
#'
#' The data come from the wpp2024 package, which is not on CRAN. Install it
#' with `remotes::install_github("PPgp/wpp2024")`.
#'
#' @param country Country name as used in wpp2024, for example `"Burundi"`,
#'   `"Kenya"` or `"Nigeria"`. Case is ignored. If there is no exact match,
#'   the first partial match is used.
#' @param year Reference year for the demographic data.
#'
#' @return A named list:
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
#'
#' @examples
#' \dontrun{
#' demog <- get_demog_wpp("Burundi", 2022)
#' demog$crude_birth_rate
#' }
#' @export
get_demog_wpp <- function(country, year) {
  if (!requireNamespace("wpp2024", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg wpp2024} package is needed to pull UN WPP 2024 data.",
      "i" = "Install it with {.code remotes::install_github(\"PPgp/wpp2024\")}"
    ))
  }

  # load wpp2024 datasets into a local env
  env <- new.env(parent = baseenv())
  utils::data("popAge1dt", package = "wpp2024", envir = env)
  utils::data("misc1dt", package = "wpp2024", envir = env)
  pop_age <- as.data.frame(env[["popAge1dt"]])
  misc <- as.data.frame(env[["misc1dt"]])

  country_name <- .match_wpp_country(country, unique(pop_age$name))

  available_years <- unique(pop_age$year[pop_age$name == country_name])
  if (!year %in% available_years) {
    closest <- .closest_year(available_years, year)
    cli::cli_alert_warning(
      "Year {year} not available. Using closest: {closest}"
    )
    year <- closest
  }

  cli::cli_alert_info(
    "Building age distribution for {country_name} ({year})"
  )

  age_data <- pop_age[pop_age$name == country_name & pop_age$year == year, ]
  age_data <- age_data[order(age_data$age), ]

  prop <- age_data$pop / sum(age_data$pop, na.rm = TRUE)
  age_distribution <- list(
    DistributionValues = cumsum(prop),
    ResultScaleFactor = 1,
    ResultValues = age_data$age * 365
  )

  vital <- misc[misc$name == country_name & misc$year == year, ]
  if (nrow(vital) == 0) {
    vital_years <- unique(misc$year[misc$name == country_name])
    closest_v <- .closest_year(vital_years, year)
    cli::cli_alert_warning(
      "Vital rates not available for {year}. Using {closest_v}"
    )
    vital <- misc[misc$name == country_name & misc$year == closest_v, ]
  }

  crude_birth_rate <- vital$cbr[1]
  crude_death_rate <- vital$cdr[1]

  cli::cli_alert_success(paste0(
    "CBR: {round(crude_birth_rate, 2)}, ",
    "CDR: {round(crude_death_rate, 2)} (per 1,000 per year)"
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

# exact match ignoring case, else the first partial match
.match_wpp_country <- function(country, available) {
  matched <- available[tolower(available) == tolower(country)]
  if (length(matched) > 0) {
    return(matched[1])
  }

  matched <- grep(country, available, value = TRUE, ignore.case = TRUE)
  if (length(matched) == 0) {
    cli::cli_abort("Country {.val {country}} not found in wpp2024.")
  }
  cli::cli_alert_info(
    "Partial match: using {.val {matched[1]}} for {.val {country}}"
  )
  matched[1]
}

.closest_year <- function(years, year) {
  years[which.min(abs(years - year))]
}
