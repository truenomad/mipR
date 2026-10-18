# internal helper: lower bounds of the WorldPop age groups. the 2015-2030
# releases split the old 80+ group into 80-84, 85-89 and 90+
#' @noRd
.worldpop_band_lower <- function(year) {
  top <- if (year < 2015) 80 else 90
  c(0, 1, seq(5, top, by = 5))
}

#' Make Single-Year Age Rasters from WorldPop Age Groups
#'
#' Downloads the WorldPop age-group rasters that cover the requested ages and
#' splits each group into single years of age, using the UN World Population
#' Prospects 2024 single-year population of the country and year.
#'
#' @details
#' WorldPop publishes age-sex structures in groups (0, 1-4, 5-9, and so on)
#' and does not publish single years of age. For an age `a` in a group
#' running from `lo` to `hi`, the single-year raster is the group raster
#' multiplied by WPP population aged `a` divided by WPP population aged `lo`
#' to `hi`, for the same country, year and sex. The split is the same in
#' every pixel, so the single-year rasters for a group always add up to the
#' group raster.
#'
#' The top group is 90+ for 2015 onwards and 80+ for earlier years. Within
#' that group, ages up to 100 can be requested; WPP age 100 means 100 and
#' over.
#'
#' Group rasters are kept in `out_dir` and reused by later calls. Years
#' before 2015 use the legacy 2000-2020 dataset (1km, male and female
#' only); later years use the R2025A or R2024B release. The WPP data come
#' from the wpp2024 package, which is not on CRAN. Install it with
#' `remotes::install_github("PPgp/wpp2024")`.
#'
#' @param country_codes Character vector of ISO3 country codes.
#' @param years Numeric vector of years (2000-2030).
#' @param ages Integer vector of single years of age, between 0 and 100.
#'   Default `0:4`.
#' @param sex `"both"` (default), `"m"` or `"f"`. For `"both"` from 2015
#'   onwards the WorldPop total (`t`) rasters are used; otherwise male and
#'   female rasters are summed.
#' @param resolution `"1km"` (default) or `"100m"`. 100m is only available
#'   from 2015 onwards.
#' @param release WorldPop release for 2015 onwards: `"R2025A"` (default,
#'   constrained to built-up areas) or `"R2024B"` (unconstrained).
#' @param out_dir Directory for the group and single-year rasters. Default
#'   `"."`.
#' @param quiet Logical. If `TRUE`, suppresses progress messages.
#'
#' @return Invisible character vector of the paths to the single-year
#'   rasters, named `{iso3}_{sex}_age{NN}_{year}.tif`, where sex is
#'   `"total"`, `"m"` or `"f"`.
#'
#' @examples
#' \dontrun{
#' # ages 1, 2, 3 and 4 for Nigeria
#' paths <- get_worldpop_single_age(
#'   country_codes = "NGA",
#'   years = 2025,
#'   ages = 1:4,
#'   out_dir = "data/worldpop"
#' )
#' }
#' @export
get_worldpop_single_age <- function(
  country_codes,
  years,
  ages = 0:4,
  sex = "both",
  resolution = "1km",
  release = "R2025A",
  out_dir = ".",
  quiet = FALSE
) {
  sex <- match.arg(sex, choices = c("both", "m", "f"))
  resolution <- match.arg(resolution, choices = c("1km", "100m"))
  release <- match.arg(release, choices = c("R2025A", "R2024B"))

  if (!requireNamespace("wpp2024", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg wpp2024} package is needed to split age groups.",
      "i" = "Install it with {.code remotes::install_github(\"PPgp/wpp2024\")}"
    ))
  }
  if (any(toupper(country_codes) == "GLOBAL")) {
    cli::cli_abort("{.val GLOBAL} is not supported for single-year ages.")
  }
  if (!is.numeric(ages) || any(ages < 0 | ages > 100 | ages %% 1 != 0)) {
    cli::cli_abort("{.arg ages} must be whole numbers between 0 and 100.")
  }
  if (resolution == "100m" && any(years < 2015)) {
    cli::cli_abort("100m resolution only available for years >= 2015")
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  ages <- sort(unique(ages))
  sex_label <- if (sex == "both") "total" else sex
  wpp_col <- c(both = "pop", m = "popM", f = "popF")[[sex]]

  pop_cols <- c("country_code", "name", "year", "age", wpp_col)
  wpp <- rbind(
    .load_wpp("popAge1dt")[, pop_cols],
    .load_wpp("popprojAge1dt")[, pop_cols]
  )
  locations <- unique(wpp[, c("country_code", "name")])

  paths <- character(0)
  for (cc in toupper(unique(country_codes))) {
    wpp_cc <- wpp[wpp$name == .match_wpp_country(cc, locations), ]

    for (yr in unique(years)) {
      out_fnames <- file.path(
        out_dir,
        sprintf("%s_%s_age%02d_%d.tif", tolower(cc), sex_label, ages, yr)
      )
      paths <- c(paths, out_fnames)
      todo <- !file.exists(out_fnames)
      if (!any(todo)) {
        if (!quiet) {
          cli::cli_alert_info("Skipping {cc} {yr} (all ages already exist)")
        }
        next
      }

      .split_age_groups(
        cc, yr, ages[todo], out_fnames[todo], wpp_cc[wpp_cc$year == yr, ],
        wpp_col, sex, resolution, release, out_dir, quiet
      )
    }
  }

  invisible(paths[file.exists(paths)])
}

# internal helper: fetch the groups covering ages for one country-year and
# write one raster per age
#' @noRd
.split_age_groups <- function(
  cc,
  yr,
  ages,
  out_fnames,
  wpp_yr,
  wpp_col,
  sex,
  resolution,
  release,
  out_dir,
  quiet
) {
  if (nrow(wpp_yr) == 0) {
    cli::cli_alert_danger("No WPP 2024 population for {cc} {yr}; skipping")
    return(invisible(NULL))
  }

  band_lower <- .worldpop_band_lower(yr)
  band_of_age <- findInterval(ages, band_lower)
  bands <- data.frame(code = band_lower[sort(unique(band_of_age))])
  bands$upper <- c(band_lower[-1] - 1, 100)[sort(unique(band_of_age))]

  if (!quiet) {
    group_labels <- ifelse(
      bands$upper == 100,
      paste0(bands$code, "+"),
      ifelse(
        bands$code == bands$upper,
        bands$code,
        paste(bands$code, bands$upper, sep = "-")
      )
    )
    cli::cli_alert_info(paste0(
      "Splitting {cc} {yr} age groups ",
      "{paste(group_labels, collapse = ', ')} into single years"
    ))
  }

  # the 't' (total) variant halves downloads; fall back to m+f without it
  sex_set <- if (sex == "both") c("m", "f") else sex
  sexes <- if (sex == "both" && yr >= 2015) "t" else sex_set
  file_set <- .age_band_file_set(
    cc, yr, bands, sexes, resolution, release, out_dir
  )
  fetched <- .fetch_age_band_files(list(file_set), quiet)
  if (!fetched && identical(sexes, "t")) {
    sexes <- sex_set
    file_set <- .age_band_file_set(
      cc, yr, bands, sexes, resolution, release, out_dir
    )
    fetched <- .fetch_age_band_files(list(file_set), quiet)
  }
  if (!fetched) {
    cli::cli_alert_danger("Failed to download age groups for {cc} {yr}")
    return(invisible(NULL))
  }

  # dests run band by band within each sex, as built by expand.grid
  dests <- matrix(file_set$dests, nrow = nrow(bands))
  wpp_pop <- stats::setNames(wpp_yr[[wpp_col]], wpp_yr$age)

  for (b in seq_len(nrow(bands))) {
    group_raster <- NULL
    for (band_file in dests[b, ]) {
      r <- suppressWarnings(terra::rast(band_file))
      group_raster <- if (is.null(group_raster)) r else group_raster + r
    }

    group_ages <- bands$code[b]:bands$upper[b]
    group_pop <- sum(wpp_pop[as.character(group_ages)], na.rm = TRUE)

    for (j in which(band_of_age == findInterval(bands$code[b], band_lower))) {
      share <- wpp_pop[[as.character(ages[j])]] / group_pop
      suppressWarnings(terra::writeRaster(
        group_raster * share, out_fnames[j],
        overwrite = TRUE
      ))
      if (!quiet) {
        cli::cli_alert_success("Written: {basename(out_fnames[j])}")
      }
    }
  }

  invisible(NULL)
}
