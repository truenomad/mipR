#' Download Administrative Boundaries
#'
#' Downloads administrative boundaries for one or more countries from a
#' chosen provider and returns them as an `sf` object with standardised
#' column names. OCHA Common Operational Datasets (COD-AB) are the default.
#' geoBoundaries, GADM and the WHO ArcGIS boundary services are available as
#' alternatives.
#'
#' @details
#' Each source is used only when requested. If the requested source has no
#' boundaries for a country or level, the function stops with an error rather
#' than switching to another provider, because district definitions differ
#' between providers. Use [list_boundaries()] to see which sources and levels
#' are available for a country.
#'
#' The sources are:
#'
#' - `"ocha"`: OCHA COD-AB from the Humanitarian Data Exchange (HDX). The
#'   dataset is resolved through the HDX API, so the most recent published
#'   version is used.
#' - `"geoboundaries"`: geoBoundaries gbOpen release. Only the requested
#'   level is published in each file, so the names of parent units are `NA`.
#' - `"gadm"`: GADM version 4.1. GADM data are free for academic and other
#'   non-commercial use, and may not be redistributed without permission.
#' - `"who"`: WHO administrative boundaries from ArcGIS services (levels 0 to
#'   2). `latest = FALSE` returns historical boundaries as well as current
#'   ones, with `start_date` and `end_date` columns.
#'
#' When `cache = TRUE`, downloaded files are kept in `cache_dir` and reused.
#' Each download is a single file in `cache_dir`, with no subfolders, and
#' the source is the last part of the filename: for example
#' `nga_cod_ab_20190417_ocha.zip`, `nga-adm2-..._geoboundaries.geojson`,
#' `nga_v41_gadm.zip` and `nga_adm2_latest_who.gpkg`. Shapefiles are read
#' directly from the zip archives. OCHA and geoBoundaries files are named by
#' dataset version, so a new release is downloaded automatically. Set
#' `cache = FALSE` to download into a temporary directory instead.
#'
#' @param country Character vector of ISO3 country codes (e.g. `"AFG"`).
#' @param adm_level Administrative level, as a number (`0`, `1`, `2`, ...) or a
#'   string such as `"ADM2"`. Default is `2`.
#' @param source Boundary provider: `"ocha"` (default), `"geoboundaries"`,
#'   `"gadm"` or `"who"`. Case is ignored, so `"WHO"` also works.
#' @param cache Logical. If `TRUE` (default), downloaded files are stored in
#'   `cache_dir` and reused by later calls.
#' @param cache_dir Directory for cached downloads. Defaults to the user cache
#'   directory for mipR given by [tools::R_user_dir()].
#' @param latest Logical, used only when `source = "who"`. If `TRUE`
#'   (default), only current boundaries are returned.
#' @param quiet Logical. If `TRUE`, suppresses progress messages. The
#'   summary of what was retrieved is always shown.
#'
#' @return An `sf` object in EPSG:4326 with the columns:
#'   \describe{
#'     \item{adm0}{Country name}
#'     \item{adm1, adm2, ...}{Unit names for each level up to `adm_level`}
#'     \item{adm1_code, adm2_code, ...}{Codes made from the unit's name and
#'       the names of its parents: `adm1_code` from `adm0` and `adm1`,
#'       `adm2_code` from `adm0`, `adm1` and `adm2`, and so on. Each is the
#'       first 12 characters of the MD5 hash of the names, lower-cased and
#'       joined with `|`, so the same unit gets the same code from every
#'       source that spells its names the same way. `NA` when the unit name
#'       is missing}
#'     \item{start_date, end_date}{Dates when the boundary came into and
#'       went out of use. Only returned for `source = "who"` with
#'       `latest = FALSE`}
#'     \item{source}{The provider the boundaries came from}
#'     \item{geometry}{Boundary polygons}
#'   }
#'
#' @examples
#' \dontrun{
#' # OCHA COD-AB districts for Afghanistan
#' afghanistan <- get_boundaries("AFG", adm_level = 2, source = "ocha")
#'
#' # geoBoundaries provinces
#' afg_adm1 <- get_boundaries("AFG", adm_level = 1, source = "geoboundaries")
#'
#' # GADM districts for two countries
#' gadm_adm2 <- get_boundaries(c("KEN", "UGA"), adm_level = 2, source = "gadm")
#'
#' # WHO boundaries, including historical ones
#' sudan <- get_boundaries("SDN", adm_level = 0, source = "who", latest = FALSE)
#' }
#'
#' @seealso [list_boundaries()] to see the sources available for a country.
#'
#' @export
get_boundaries <- function(
  country,
  adm_level = 2,
  source = c("ocha", "geoboundaries", "gadm", "who"),
  cache = TRUE,
  cache_dir = tools::R_user_dir("mipR", which = "cache"),
  latest = TRUE,
  quiet = FALSE
) {
  source <- match.arg(
    tolower(source),
    c("ocha", "geoboundaries", "gadm", "who")
  )
  country <- .check_country(country)
  level <- .check_level(adm_level, source)

  if (source == "who") {
    dest_path <- if (isTRUE(cache)) cache_dir else NULL
    out <- .get_boundaries_who(
      country,
      admin_level = paste0("ADM", level),
      latest = latest,
      dest_path = dest_path
    )
    return(.standardise_boundaries(out, level, source, dates = !latest))
  }

  download_dir <- if (isTRUE(cache)) {
    cache_dir
  } else {
    tempfile("mipR_boundaries_")
  }
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  get_one <- switch(
    source,
    ocha = .get_boundaries_ocha,
    geoboundaries = .get_boundaries_geoboundaries,
    gadm = .get_boundaries_gadm
  )

  pieces <- lapply(country, function(iso3) {
    out <- get_one(iso3, level, download_dir, quiet)
    .standardise_boundaries(out, level, source)
  })

  do.call(rbind, pieces)
}

#' List Available Administrative Boundary Sources
#'
#' Reports which boundary providers have data for each country, with the
#' administrative levels, dataset version, licence and download location.
#' Use it before [get_boundaries()] to choose a source.
#'
#' @details
#' The function queries each provider's metadata service and does not
#' download boundary files. A source that cannot be reached, or has no data
#' for the country, is returned with `available = FALSE`. Levels for OCHA
#' COD-AB are read from the dataset description on HDX and may be `NA` when
#' the description does not state them.
#'
#' @param country Character vector of ISO3 country codes.
#' @param source Character vector of providers to check. Defaults to all of
#'   `"ocha"`, `"geoboundaries"`, `"gadm"` and `"who"`.
#'
#' @return A data frame with one row per country and source, and the columns
#'   `country`, `source`, `available`, `levels`, `version`, `licence` and
#'   `url`.
#'
#' @examples
#' \dontrun{
#' list_boundaries("AFG")
#'
#' list_boundaries(c("AFG", "PAK"), source = c("ocha", "geoboundaries"))
#' }
#'
#' @export
list_boundaries <- function(
  country,
  source = c("ocha", "geoboundaries", "gadm", "who")
) {
  source <- match.arg(
    tolower(source),
    c("ocha", "geoboundaries", "gadm", "who"),
    several.ok = TRUE
  )
  country <- .check_country(country)

  list_one <- list(
    ocha = .list_boundaries_ocha,
    geoboundaries = .list_boundaries_geoboundaries,
    gadm = .list_boundaries_gadm,
    who = .list_boundaries_who
  )

  rows <- list()
  for (iso3 in country) {
    for (src in source) {
      row <- tryCatch(
        list_one[[src]](iso3),
        error = function(e) list(available = FALSE)
      )
      available <- isTRUE(row$available)
      rows[[length(rows) + 1]] <- data.frame(
        country = iso3,
        source = src,
        available = available,
        levels = if (available) .or_na(row$levels) else NA_character_,
        version = if (available) .or_na(row$version) else NA_character_,
        licence = .or_na(row$licence),
        url = .or_na(row$url),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

# input checks and shared helpers ---------------------------------------------

#' @noRd
.check_country <- function(country) {
  if (!is.character(country) || length(country) == 0 ||
        any(!grepl("^[A-Za-z]{3}$", country))) {
    cli::cli_abort(
      "{.arg country} must be a character vector of ISO3 codes, e.g. \"AFG\""
    )
  }
  unique(toupper(country))
}

#' @noRd
.check_level <- function(level, source) {
  max_level <- c(ocha = 4L, geoboundaries = 5L, gadm = 5L, who = 2L)[[source]]
  level_num <- suppressWarnings(
    as.integer(sub("^ADM", "", toupper(as.character(level))))
  )
  if (length(level) != 1 || is.na(level_num) || level_num < 0 ||
        level_num > max_level) {
    cli::cli_abort(c(
      "Invalid {.arg adm_level} for source {.val {source}}",
      "i" = "Use a level from 0 to {max_level}, e.g. {.code adm_level = 2}"
    ))
  }
  level_num
}

#' @noRd
.or_na <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
}

# download url to dest unless it is already there; writes to a .part file
# and renames on success so an interrupted download is not reused
#' @noRd
.download_file <- function(url, dest, quiet = FALSE) {
  if (file.exists(dest)) {
    return(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(dest, ".part")
  on.exit(unlink(part), add = TRUE)

  if (!quiet) cli::cli_alert_info("Downloading {.url {url}}")
  tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(600) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = part),
    error = function(e) {
      cli::cli_abort("Download failed: {.url {url}}", parent = e)
    }
  )
  file.rename(part, dest)
  dest
}

# list the .shp files inside a zip as GDAL /vsizip/ paths, so they are read
# from the archive without unzipping
#' @noRd
.zip_shapefiles <- function(zip_path) {
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  shp <- entries[grepl("\\.shp$", entries, ignore.case = TRUE)]
  zip_abs <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
  file.path("/vsizip", zip_abs, shp, fsep = "/")
}

# internal helper: standard columns, name-based codes and EPSG:4326. dates
# keeps start_date and end_date, which only historical WHO boundaries need
#' @noRd
.standardise_boundaries <- function(x, level, source, dates = FALSE) {
  core <- "adm0"
  for (l in seq_len(level)) {
    core <- c(core, paste0("adm", l), paste0("adm", l, "_code"))
  }
  if (dates) {
    core <- c(core, "start_date", "end_date")
  }
  for (col in setdiff(core, names(x))) {
    x[[col]] <- NA
  }
  if (dates) {
    x$start_date <- as.Date(x$start_date)
    x$end_date <- as.Date(x$end_date)
  }
  x$source <- source

  # codes come from the names of the unit and its parents, not the provider
  path <- .normalise_name(x$adm0)
  for (l in seq_len(level)) {
    unit <- x[[paste0("adm", l)]]
    path <- paste(path, .normalise_name(unit), sep = "|")
    x[[paste0("adm", l, "_code")]] <- ifelse(
      is.na(unit),
      NA_character_,
      substr(as.character(openssl::md5(path)), 1, 12)
    )
  }

  x <- x[, c(core, "source")]
  if (attr(x, "sf_column") != "geometry") {
    sf::st_geometry(x) <- "geometry"
  }
  if (!isTRUE(sf::st_crs(x) == sf::st_crs(4326))) {
    x <- sf::st_transform(x, 4326)
  }
  x
}

# internal helper: lower case with single spaces, so codes do not depend on
# case or spacing; NA becomes an empty string
#' @noRd
.normalise_name <- function(x) {
  x <- tolower(trimws(gsub("\\s+", " ", as.character(x))))
  ifelse(is.na(x), "", x)
}

# OCHA COD-AB ------------------------------------------------------------------

#' @noRd
.hdx_cod_ab <- function(iso3) {
  resp <- httr2::request(
    "https://data.humdata.org/api/3/action/package_show"
  ) |>
    httr2::req_url_query(id = paste0("cod-ab-", tolower(iso3))) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) == 404) {
    cli::cli_abort(c(
      "No OCHA COD-AB dataset found on HDX for {.val {iso3}}",
      "i" = paste0(
        "Run {.code list_boundaries(\"{iso3}\")} to see other sources, ",
        "then set {.arg source} explicitly"
      )
    ))
  }
  if (httr2::resp_is_error(resp)) {
    cli::cli_abort(
      "HDX request failed with status {httr2::resp_status(resp)}"
    )
  }

  httr2::resp_body_json(resp)$result
}

#' @noRd
.first_date <- function(x) {
  .or_na(regmatches(x, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", x)))
}

#' @noRd
.get_boundaries_ocha <- function(iso3, level, download_dir, quiet) {
  meta <- .hdx_cod_ab(iso3)

  shp_resources <- Filter(
    function(res) toupper(res$format) == "SHP",
    meta$resources
  )
  if (length(shp_resources) == 0) {
    cli::cli_abort(
      "The OCHA COD-AB dataset for {.val {iso3}} has no shapefile resource"
    )
  }
  resource <- shp_resources[[1]]

  # name the cached archive by its modification date so a new release is
  # downloaded rather than the cached copy reused
  stamp <- gsub("-", "", .first_date(resource$last_modified))
  zip_path <- file.path(
    download_dir,
    paste0(tolower(iso3), "_cod_ab_", stamp, "_ocha.zip")
  )
  .download_file(resource$url, zip_path, quiet)
  shp_files <- .zip_shapefiles(zip_path)

  # current layout is {iso3}_admin{level}.shp; older releases use names
  # such as {iso3}_admbnda_adm{level}_{source}.shp
  target <- shp_files[
    grepl(sprintf("_admin%d\\.shp$", level), basename(shp_files))
  ]
  if (length(target) == 0) {
    target <- shp_files[
      grepl(sprintf("adm%d", level), basename(shp_files)) &
        !grepl("admbnd[lpt]|lines|points", basename(shp_files))
    ]
  }
  if (length(target) == 0) {
    cli::cli_abort(c(
      "No ADM{level} layer in the OCHA COD-AB dataset for {.val {iso3}}",
      "i" = "Layers found: {.file {basename(shp_files)}}"
    ))
  }

  x <- sf::st_read(target[[1]], quiet = TRUE)
  geom_col <- attr(x, "sf_column")
  names(x)[names(x) != geom_col] <- tolower(names(x)[names(x) != geom_col])

  .pick <- function(candidates) {
    col <- intersect(candidates, names(x))
    if (length(col) == 0) NA_character_ else as.character(x[[col[[1]]]])
  }

  x$adm0_code <- iso3
  x$adm0 <- .pick(c("adm0_name", "adm0_en"))
  for (l in seq_len(level)) {
    x[[paste0("adm", l)]] <- .pick(paste0("adm", l, c("_name", "_en")))
    x[[paste0("adm", l, "_code")]] <- .pick(paste0("adm", l, "_pcode"))
  }

  dataset_date <- .first_date(meta$dataset_date)
  cli::cli_alert_success(
    paste0(
      "{iso3} ADM{level}: {nrow(x)} units from OCHA COD-AB ",
      "({meta$title}; dataset date {dataset_date}; ",
      "licence {meta$license_title})"
    )
  )
  if (!is.null(meta$caveats) && nzchar(meta$caveats)) {
    cli::cli_alert_info("HDX caveat: {meta$caveats}")
  }

  x
}

#' @noRd
.list_boundaries_ocha <- function(iso3) {
  meta <- .hdx_cod_ab(iso3)
  level_range <- regmatches(
    meta$notes,
    regexpr("[0-9] ?- ?[0-9](?= boundaries)", meta$notes, perl = TRUE)
  )
  list(
    available = TRUE,
    levels = gsub(" ", "", level_range),
    version = .first_date(meta$dataset_date),
    licence = meta$license_title,
    url = paste0("https://data.humdata.org/dataset/", meta$name)
  )
}

# geoBoundaries ---------------------------------------------------------------

#' @noRd
.geoboundaries_meta <- function(iso3, level = "ALL") {
  level_str <- if (identical(level, "ALL")) "ALL" else paste0("ADM", level)
  url <- sprintf(
    "https://www.geoboundaries.org/api/current/gbOpen/%s/%s/",
    iso3, level_str
  )
  resp <- httr2::request(url) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    cli::cli_abort(c(
      "No geoBoundaries gbOpen data for {.val {iso3}} at {level_str}",
      "i" = "Run {.code list_boundaries(\"{iso3}\")} to see other sources"
    ))
  }
  httr2::resp_body_json(resp, check_type = FALSE)
}

#' @noRd
.get_boundaries_geoboundaries <- function(iso3, level, download_dir, quiet) {
  meta <- .geoboundaries_meta(iso3, level)

  geojson_path <- file.path(
    download_dir,
    paste0(tolower(meta$boundaryID), "_geoboundaries.geojson")
  )
  .download_file(meta$gjDownloadURL, geojson_path, quiet)
  x <- sf::st_read(geojson_path, quiet = TRUE)

  x$adm0_code <- iso3
  x$adm0 <- if (level == 0) x$shapeName else meta$boundaryName
  if (level > 0) {
    x[[paste0("adm", level)]] <- x$shapeName
    x[[paste0("adm", level, "_code")]] <- x$shapeID
  }

  cli::cli_alert_success(
    paste0(
      "{iso3} ADM{level}: {nrow(x)} units from geoBoundaries gbOpen ",
      "(year represented {meta$boundaryYearRepresented}; ",
      "licence {meta$boundaryLicense}; source {meta$boundarySource})"
    )
  )

  x
}

#' @noRd
.list_boundaries_geoboundaries <- function(iso3) {
  meta <- .geoboundaries_meta(iso3, "ALL")
  .field <- function(name) {
    unique(vapply(meta, function(m) .or_na(m[[name]]), character(1)))
  }
  list(
    available = length(meta) > 0,
    levels = paste(sub("^ADM", "", .field("boundaryType")), collapse = ", "),
    version = paste(.field("boundaryYearRepresented"), collapse = ", "),
    licence = paste(.field("boundaryLicense"), collapse = ", "),
    url = sprintf(
      "https://www.geoboundaries.org/api/current/gbOpen/%s/ALL/", iso3
    )
  )
}

# GADM -------------------------------------------------------------------------

gadm_licence <- paste0(
  "GADM licence: free for academic and other non-commercial use; ",
  "redistribution or commercial use requires permission"
)

#' @noRd
.get_boundaries_gadm <- function(iso3, level, download_dir, quiet) {
  url <- sprintf(
    "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_%s_shp.zip", iso3
  )
  zip_path <- file.path(
    download_dir,
    paste0(tolower(iso3), "_v41_gadm.zip")
  )
  .download_file(url, zip_path, quiet)
  shp_files <- .zip_shapefiles(zip_path)

  target <- shp_files[
    basename(shp_files) == sprintf("gadm41_%s_%d.shp", iso3, level)
  ]
  if (length(target) == 0) {
    cli::cli_abort(c(
      "No ADM{level} layer in the GADM archive for {.val {iso3}}",
      "i" = "Layers found: {.file {basename(shp_files)}}"
    ))
  }

  x <- sf::st_read(target[[1]], quiet = TRUE)

  x$adm0_code <- x$GID_0
  x$adm0 <- x$COUNTRY
  for (l in seq_len(level)) {
    x[[paste0("adm", l)]] <- x[[paste0("NAME_", l)]]
    x[[paste0("adm", l, "_code")]] <- x[[paste0("GID_", l)]]
  }

  cli::cli_alert_success(
    "{iso3} ADM{level}: {nrow(x)} units from GADM 4.1 ({gadm_licence})"
  )

  x
}

#' @noRd
.list_boundaries_gadm <- function(iso3) {
  # GADM publishes one GeoJSON per level, so probe them to find the levels
  has_level <- vapply(0:5, function(l) {
    url <- sprintf(
      "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_%s_%d.json.zip",
      iso3, l
    )
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) == 200
  }, logical(1))

  list(
    available = any(has_level),
    levels = paste((0:5)[has_level], collapse = ", "),
    version = "4.1",
    licence = gadm_licence,
    url = sprintf(
      "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_%s_shp.zip", iso3
    )
  )
}

# WHO --------------------------------------------------------------------------

who_services <- c(
  ADM0 = "GLOBAL_ADM0",
  ADM1 = "Detailed_Boundary_ADM1",
  ADM2 = "Detailed_Boundary_ADM2"
)

#' @noRd
.who_service_url <- function(admin_level) {
  paste0(
    "https://services.arcgis.com/5T5nSi527N4F7luB",
    "/arcgis/rest/services/", who_services[[admin_level]], "/",
    "FeatureServer/0/query"
  )
}

#' @noRd
.list_boundaries_who <- function(iso3) {
  has_level <- vapply(names(who_services), function(admin_level) {
    resp <- httr2::request(.who_service_url(admin_level)) |>
      httr2::req_url_query(
        where = paste0("ISO_3_CODE = '", iso3, "'"),
        returnCountOnly = "true",
        f = "json"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(check_type = FALSE)
    isTRUE(resp$count > 0)
  }, logical(1))

  list(
    available = any(has_level),
    levels = paste((0:2)[has_level], collapse = ", "),
    version = NA_character_,
    licence = NA_character_,
    url = "https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services"
  )
}

# download WHO boundaries for country_codes at admin_level. when dest_path is
# given, results are kept in a geopackage and only missing countries fetched
#' @noRd
.get_boundaries_who <- function(
  country_codes,
  admin_level = "ADM2",
  latest = TRUE,
  dest_path = NULL
) {
  # validate admin_level
  admin_level <- toupper(admin_level)
  valid_levels <- c("ADM0", "ADM1", "ADM2")
  if (!admin_level %in% valid_levels) {
    stop(
      paste0(
        "Invalid admin_level. Must be one of: ",
        paste(valid_levels, collapse = ", ")
      )
    )
  }

  # initialise existing codes if dest_path is provided
  dest_file <- NULL
  if (!is.null(dest_path)) {
    dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)

    # build filename from country codes and admin level, with a _latest
    # suffix when latest = TRUE
    countries_str <- tolower(paste(country_codes, collapse = "_"))
    admin_str <- tolower(admin_level)
    file_stem <- paste0(countries_str, "_", admin_str)
    if (isTRUE(latest)) {
      file_stem <- paste0(file_stem, "_latest")
    }
    dest_file <- file.path(dest_path, paste0(file_stem, "_who.gpkg"))

    if (file.exists(dest_file)) {
      existing_sf <- sf::st_read(dest_file, quiet = TRUE)
      existing_codes <- unique(existing_sf$adm0_code)

      # keep only the codes not already in the file
      country_codes <- setdiff(country_codes, existing_codes)

      if (length(country_codes) == 0) {
        cli::cli_alert_info(
          paste0(
            "All requested country codes are already in ",
            "{basename(dest_file)}. No updates needed."
          )
        )
        return(existing_sf)
      }
    } else {
      existing_codes <- character(0)
    }
  }

  # fields requested from each service
  service_fields <- list(
    ADM0 = "ISO_3_CODE,ADM0_NAME,STARTDATE,ENDDATE",
    ADM1 = "ISO_3_CODE,ADM0_NAME,ADM1_NAME,STARTDATE,ENDDATE",
    ADM2 = "ISO_3_CODE,ADM0_NAME,ADM1_NAME,ADM2_NAME,STARTDATE,ENDDATE"
  )

  # download missing country codes
  codes_str <- paste0("'", paste(country_codes, collapse = "','"), "'")
  where_clause <- paste0("ISO_3_CODE IN (", codes_str, ")")
  base_url <- .who_service_url(admin_level)
  params <- list(
    where = where_clause,
    outFields = service_fields[[admin_level]],
    outSR = 4326,
    f = "geojson"
  )

  codes_label <- paste(country_codes, collapse = ", ")
  cli::cli_alert_info(
    "Downloading WHO {admin_level} data for: {.val {codes_label}}"
  )

  request_url <- httr2::request(base_url) |>
    httr2::req_url_query(!!!params)

  final_url <- request_url$url

  new_sf <- suppressWarnings(
    suppressMessages({
      invisible(
        utils::capture.output(
          out <- sf::st_read(
            final_url,
            quiet = TRUE
          )
        )
      )
      out
    })
  )

  # keep active boundaries only when latest = TRUE
  if (latest && "ENDDATE" %in% names(new_sf)) {
    new_sf <- new_sf |>
      dplyr::filter(ENDDATE == "253402214400000" | is.na(ENDDATE))
  }

  .ms_to_date <- function(ms) {
    as.Date(as.POSIXct(as.numeric(ms) / 1000, origin = "1970-01-01"))
  }

  # rename fields based on admin level
  new_sf <- switch(
    admin_level,
    ADM0 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        start_date = .ms_to_date(STARTDATE),
        end_date = .ms_to_date(ENDDATE)
      ),
    ADM1 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        adm1 = ADM1_NAME,
        start_date = .ms_to_date(STARTDATE),
        end_date = .ms_to_date(ENDDATE)
      ),
    ADM2 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        adm1 = ADM1_NAME,
        adm2 = ADM2_NAME,
        start_date = .ms_to_date(STARTDATE),
        end_date = .ms_to_date(ENDDATE)
      )
  )

  if (!is.null(dest_path) && !is.null(dest_file)) {
    if (file.exists(dest_file)) {
      sf::st_write(
        sf::st_make_valid(new_sf),
        dest_file,
        append = TRUE,
        quiet = TRUE
      )
      cli::cli_alert_success(
        paste0(
          "Appended missing country codes to existing shapefile ",
          "({basename(dest_file)}): {codes_label}"
        )
      )
      return(sf::st_read(dest_file, quiet = TRUE))
    } else {
      sf::st_write(
        sf::st_make_valid(new_sf),
        dest_file,
        append = FALSE,
        quiet = TRUE
      )
      cli::cli_alert_success(
        paste0(
          "Created new shapefile ({basename(dest_file)}) with country ",
          "codes: {codes_label}"
        )
      )
      return(new_sf)
    }
  } else {
    cli::cli_alert_success(
      "Downloaded {admin_level} data for: {codes_label}"
    )
    return(new_sf)
  }
}
