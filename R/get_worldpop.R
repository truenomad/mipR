# age-sex structures come from the University of Southampton WorldPop mirror
.worldpop_agesex_root <-
  "https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/"

# internal helper to build worldpop url and filename. cc may be an ISO3 code
# or "GLOBAL" for the worldwide mosaic (r2025a, 1km and count only)
#' @noRd
.build_worldpop_url <- function(cc, year, type, dataset, resolution) {
  is_global <- toupper(cc) == "GLOBAL"

  if (dataset == "legacy") {
    if (is_global) {
      cli::cli_abort(
        "{.val GLOBAL} mosaic not available for legacy years (2000-2014)"
      )
    }
    base_url <- "https://data.worldpop.org/GIS/Population"
    if (type == "density") base_url <- paste0(base_url, "_Density")
    base_url <- paste0(base_url, "/Global_2000_2020_1km_UNadj/")

    fn <- if (type == "count") {
      sprintf("%s_ppp_%s_1km_Aggregated_UNadj.tif", tolower(cc), year)
    } else {
      sprintf("%s_pd_%s_1km_UNadj.tif", tolower(cc), year)
    }

    url <- sprintf("%s%s/%s/%s", base_url, year, toupper(cc), fn)
    return(list(url = url, filename = fn))
  }

  # r2025a dataset
  if (is_global && resolution != "1km") {
    cli::cli_abort(
      "{.val GLOBAL} mosaic only available at 1km resolution"
    )
  }

  base_url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/"
  )

  if (resolution == "1km") {
    res_path <- "1km_ua"
    res_suffix <- "1km"
    ua_suffix <- "_UA"
  } else {
    res_path <- "100m"
    res_suffix <- "100m"
    ua_suffix <- ""
  }

  if (is_global) {
    cc_lo <- "global"
    country_segment <- "0_Mosaicked"
  } else {
    cc_lo <- tolower(cc)
    country_segment <- toupper(cc)
  }

  # remote filename includes R2025A, local filename does not
  remote_fn <- sprintf(
    "%s_pop_%s_CN_%s_R2025A%s_v1.tif",
    cc_lo, year, res_suffix, ua_suffix
  )
  local_fn <- sprintf(
    "%s_pop_%s_CN_%s%s_v1.tif",
    cc_lo, year, res_suffix, ua_suffix
  )

  url <- sprintf(
    "%s%s/%s/v1/%s/constrained/%s",
    base_url, year, country_segment, res_path, remote_fn
  )

  return(list(url = url, filename = local_fn))
}

# internal helper: download urls to dests concurrently, trying the ftp
# mirror first and falling back to https. returns TRUE per file present
#' @noRd
.worldpop_fetch <- function(urls, dests, quiet = FALSE, max_active = 6) {
  missing_idx <- which(!file.exists(dests))
  if (length(missing_idx) == 0) {
    return(rep(TRUE, length(dests)))
  }

  part_paths <- paste0(dests[missing_idx], ".part")

  ftp_urls <- sub(
    paste0(
      "^https://(data\\.worldpop\\.org|",
      "worldpop-public-data\\.soton\\.ac\\.uk)/"
    ),
    "ftp://ftp.worldpop.org.uk/",
    urls[missing_idx]
  )

  # the mirror caps connections per IP, so fetch at most 4 at a time, back
  # off between retries, and resume partial files
  fetched <- rep(FALSE, length(missing_idx))
  not_on_server <- rep(FALSE, length(missing_idx))
  backoffs <- c(0, 10, 30)
  for (attempt in 1:3) {
    todo <- which(!fetched & !not_on_server)
    if (length(todo) == 0) break
    if (backoffs[attempt] > 0) {
      if (!quiet) {
        cli::cli_alert_info(paste0(
          "Retrying {length(todo)} file{?s} on the FTP mirror ",
          "in {backoffs[attempt]}s"
        ))
      }
      Sys.sleep(backoffs[attempt])
    }
    for (chunk in split(todo, ceiling(seq_along(todo) / 4))) {
      ftp_results <- suppressWarnings(curl::multi_download(
        ftp_urls[chunk], part_paths[chunk],
        resume = TRUE, progress = !quiet, timeout = 600
      ))
      fetched[chunk] <- ftp_results$success %in% TRUE &
        file.exists(part_paths[chunk])
      # 550 = not on the server; do not retry
      not_on_server[chunk] <- !fetched[chunk] &
        grepl("550", ftp_results$error)
    }
  }

  # fall back to https for anything the ftp mirror could not deliver
  retry_idx <- which(!fetched)
  if (length(retry_idx) > 0) {
    transient_idx <- retry_idx[!not_on_server[retry_idx]]
    if (length(transient_idx) > 0) {
      cli::cli_alert_warning(paste0(
        "{length(transient_idx)} file{?s} unavailable from the FTP ",
        "mirror; falling back to throttled HTTPS (slow for large files)"
      ))
    }
    unlink(part_paths[retry_idx])
    requests <- lapply(urls[missing_idx][retry_idx], function(url) {
      httr2::request(url) |>
        httr2::req_options(low_speed_limit = 1024, low_speed_time = 120) |>
        httr2::req_retry(max_tries = 3, backoff = ~5)
    })
    responses <- httr2::req_perform_parallel(
      requests,
      paths = part_paths[retry_idx],
      max_active = max_active,
      on_error = "continue",
      progress = !quiet
    )
    fetched[retry_idx] <- vapply(
      responses,
      function(response) inherits(response, "httr2_response"),
      logical(1)
    )
  }

  file.rename(part_paths[fetched], dests[missing_idx][fetched])
  unlink(part_paths[!fetched])

  file.exists(dests)
}

#' Download Population Rasters from WorldPop
#'
#' Downloads population raster files from WorldPop for specified countries and
#' years. Automatically selects the appropriate dataset based on years:
#' legacy (2000-2020) or R2025A (2015-2030).
#'
#' @param country_codes Character vector of ISO country codes (e.g., "GBR",
#'   "GIN"). Pass `"GLOBAL"` (case-insensitive) to download the worldwide
#'   mosaic from the `0_Mosaicked` directory instead of a per-country
#'   raster. `"GLOBAL"` only works for years >= 2015 at 1km resolution and
#'   `type = "count"`.
#' @param years Numeric vector of years to download data for (2000-2030).
#' @param type Character; either "density" for persons per sq km or "count"
#'   for total population count (default: "count"). Note: density is only
#'   available for years 2000-2020.
#' @param resolution Character; either "1km" (default) or "100m". The 100m
#'   resolution is only available for years 2015-2030 and is not available
#'   for the GLOBAL mosaic.
#' @param dest_dir Destination directory for downloaded files
#'   (default: current dir)
#' @param quiet Logical; if TRUE, suppresses progress messages
#'   (default: FALSE)
#'
#' @return Invisible list containing:
#'   \itemize{
#'     \item files: Vector of paths to downloaded/existing files
#'     \item counts: Named vector of successful downloads per country
#'   }
#'
#' @details
#' The function automatically selects the appropriate WorldPop dataset:
#'
#' ## Legacy Dataset (years < 2015)
#' Downloads UN-adjusted population rasters from WorldPop's Global 2000-2020
#' dataset. Available at 1km resolution only. Supports both count and density.
#'
#' ## R2025A Dataset (years >= 2015)
#' Downloads constrained population count rasters from WorldPop's Global
#' 2015-2030 dataset (R2025A release). Available at 1km and 100m resolution.
#' Population is constrained to built-up areas.
#'
#' If your year range spans both datasets (e.g., 2010:2020), the function will
#' automatically download from both sources.
#'
#' Files are downloaded to the specified directory, with existing files
#' skipped. Progress is shown during downloads and a summary is provided upon
#' completion.
#'
#' @examples
#' \dontrun{
#' # Download population data for Guinea (auto-selects dataset)
#' get_worldpop("GIN", years = 2015:2020)
#'
#' # Download legacy data (2000-2014)
#' get_worldpop("GIN", years = 2000:2010)
#'
#' # Download spanning both datasets
#' get_worldpop("GIN", years = 2010:2020)
#'
#' # Download 100m resolution data (R2025A only)
#' get_worldpop("GIN", years = 2020, resolution = "100m")
#' }
#' @export
get_worldpop <- function(
  country_codes,
  years = 2000:2020,
  type = "count",
  resolution = "1km",
  dest_dir = ".",
  quiet = FALSE
) {
  type <- match.arg(type, choices = c("count", "density"))
  resolution <- match.arg(resolution, choices = c("1km", "100m"))

  # split years into legacy (<2015) and r2025a (>=2015)
  legacy_years <- years[years < 2015]
  r2025a_years <- years[years >= 2015]

  if (length(legacy_years) > 0 && (min(legacy_years) < 2000)) {
    cli::cli_abort("Legacy dataset only available for years 2000-2020")
  }
  if (length(r2025a_years) > 0 && max(r2025a_years) > 2030) {
    cli::cli_abort("R2025A dataset only available for years 2015-2030")
  }

  # density only available in legacy dataset
  if (type == "density" && length(r2025a_years) > 0) {
    cli::cli_abort(c(
      "Density data only available for years 2000-2020",
      "i" = "Use {.code type = 'count'} for years >= 2015"
    ))
  }

  # 100m resolution only available in r2025a
  if (resolution == "100m" && length(legacy_years) > 0) {
    cli::cli_abort(c(
      "100m resolution only available for years >= 2015",
      "i" = "Use {.code resolution = '1km'} for years < 2015"
    ))
  }

  # global mosaic only for r2025a years, count, and 1km
  has_global <- any(toupper(country_codes) == "GLOBAL")
  if (has_global) {
    if (length(legacy_years) > 0) {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available for years >= 2015",
        "i" = "Drop legacy years or use ISO3 country codes for them"
      ))
    }
    if (resolution != "1km") {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available at 1km resolution",
        "i" = "Use {.code resolution = '1km'} when requesting GLOBAL"
      ))
    }
    if (type != "count") {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available for {.val count} rasters",
        "i" = "Density is legacy-only and has no global mosaic"
      ))
    }
    if (!quiet) {
      cli::cli_alert_warning(
        paste0(
          "{.val GLOBAL} downloads each year as a worldwide raster ",
          "(~290 MB per year)"
        )
      )
    }
  }

  # normalise codes: keep "GLOBAL" (case-insensitive); uppercase ISO3
  # otherwise
  country_codes <- vapply(
    country_codes,
    function(c) if (toupper(c) == "GLOBAL") "GLOBAL" else toupper(c),
    character(1)
  )

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  all_results <- list()
  all_params <- list()

  # download legacy years (< 2015)
  if (length(legacy_years) > 0) {
    if (!quiet) {
      cli::cli_alert_info(
        paste0(
          "Using legacy dataset (2000-2020 UN-adjusted) for years: ",
          "{.val {legacy_years}}"
        )
      )
    }
    legacy_res <- .get_worldpop_batch(
      country_codes, legacy_years, type, "legacy", "1km", dest_dir, quiet
    )
    all_results <- c(all_results, legacy_res$results)
    all_params <- c(all_params, list(legacy_res$params))
  }

  # download r2025a years (>= 2015)
  if (length(r2025a_years) > 0) {
    if (!quiet) {
      cli::cli_alert_info(
        paste0(
          "Using R2025A dataset (2015-2030 constrained) for years: ",
          "{.val {r2025a_years}}"
        )
      )
    }
    r2025a_res <- .get_worldpop_batch(
      country_codes, r2025a_years, type, "r2025a", resolution, dest_dir, quiet
    )
    all_results <- c(all_results, r2025a_res$results)
    all_params <- c(all_params, list(r2025a_res$params))
  }

  # combine params for counting
  combined_params <- do.call(rbind, all_params)
  counts <- tapply(combined_params$success, combined_params$country, sum)

  if (!quiet) {
    cli::cli_alert_success(
      "Download of WorldPop {type} rasters is complete!"
    )
    for (cc in names(counts)) {
      cli::cli_alert_info(
        "{cc}: {counts[[cc]]} of {length(years)} years downloaded"
      )
    }
  }

  downloaded_files <- unlist(all_results)
  invisible(list(
    files = downloaded_files[!is.na(downloaded_files)],
    counts = counts
  ))
}

#' @noRd
.get_worldpop_batch <- function(
  country_codes,
  years,
  type,
  dataset,
  resolution,
  dest_dir,
  quiet
) {
  params <- expand.grid(
    country = country_codes,
    year = years,
    stringsAsFactors = FALSE
  )

  url_infos <- Map(
    function(cc, yr) .build_worldpop_url(cc, yr, type, dataset, resolution),
    params$country,
    params$year
  )
  urls <- vapply(url_infos, function(info) info$url, character(1))
  dests <- file.path(
    dest_dir,
    vapply(url_infos, function(info) info$filename, character(1))
  )

  cached <- file.exists(dests)
  if (!quiet && any(cached)) {
    cli::cli_alert_info("Exists: {.file {basename(dests[cached])}}")
  }

  success <- .worldpop_fetch(urls, dests, quiet)
  if (any(!success)) {
    n_failed <- sum(!success)
    cli::cli_alert_warning(paste0(
      "Failed to download {n_failed} file{?s}: ",
      "{.file {basename(dests[!success])}}"
    ))
  }
  params$success <- success

  list(results = ifelse(success, dests, NA_character_), params = params)
}

# internal helper to build worldpop age-band URL and local filename. release
# applies to year >= 2015 only; cc may be ISO3 or "GLOBAL"
#' @noRd
.build_worldpop_age_band_url <- function(
  cc,
  sex,
  code,
  year,
  resolution,
  release = "R2025A"
) {
  is_global <- toupper(cc) == "GLOBAL"

  if (year < 2015) {
    # legacy 2000-2020: 1km only, unconstrained, m/f only, no global mosaic
    if (is_global) {
      cli::cli_abort(
        "{.val GLOBAL} mosaic not available for years < 2015"
      )
    }
    if (resolution != "1km") {
      cli::cli_abort(
        "100m resolution not available for years < 2015"
      )
    }
    if (sex == "t") {
      cli::cli_abort(
        "{.val t} (total) variant not available for years < 2015"
      )
    }

    cc_lo <- tolower(cc)
    cc_up <- toupper(cc)
    url <- sprintf(
      paste0(
        .worldpop_agesex_root,
        "Global_2000_2020_1km/unconstrained/%d/%s/%s_%s_%s_%d_1km.tif"
      ),
      year, cc_up, cc_lo, sex, code, year
    )
    local_fn <- sprintf("%s_%s_%s_%d_1km.tif", cc_lo, sex, code, year)
    return(list(url = url, filename = local_fn))
  }

  # year >= 2015: R2024B (unconstrained, UC) or R2025A (constrained, CN)
  if (is_global && resolution != "1km") {
    cli::cli_abort(
      "{.val GLOBAL} mosaic only available at 1km resolution"
    )
  }

  if (release == "R2025A") {
    constraint <- "constrained"
    constraint_tag <- "CN"
  } else {
    constraint <- "unconstrained"
    constraint_tag <- "UC"
  }

  if (resolution == "1km") {
    res_path <- "1km_ua"
    res_suffix <- "1km"
    ua_suffix <- "_UA"
  } else {
    res_path <- "100m"
    res_suffix <- "100m"
    ua_suffix <- ""
  }

  if (is_global) {
    cc_lo <- "global"
    country_segment <- "0_Mosaicked"
  } else {
    cc_lo <- tolower(cc)
    country_segment <- toupper(cc)
  }

  # age band code uses leading zero for r2024b/r2025a filenames
  code_str <- sprintf("%02d", code)
  remote_fn <- sprintf(
    "%s_%s_%s_%d_%s_%s_%s%s_v1.tif",
    cc_lo, sex, code_str, year, constraint_tag,
    res_suffix, release, ua_suffix
  )
  url <- sprintf(
    paste0(
      .worldpop_agesex_root,
      "Global_2015_2030/%s/%d/%s/v1/%s/%s/%s"
    ),
    release, year, country_segment, res_path, constraint, remote_fn
  )
  # local filename drops release tag so the cache is stable across releases
  local_fn <- sprintf(
    "%s_%s_%s_%d_%s_%s%s_v1.tif",
    cc_lo, sex, code_str, year, constraint_tag, res_suffix, ua_suffix
  )

  list(url = url, filename = local_fn)
}

# internal helper: build urls + dests for one (cc, yr) combo's band files
# across bands * sexes
#' @noRd
.age_band_file_set <- function(
  cc,
  yr,
  matching_bands,
  sexes,
  resolution,
  release,
  out_dir
) {
  combos <- expand.grid(
    code = matching_bands$code,
    sex = sexes,
    stringsAsFactors = FALSE
  )
  url_infos <- Map(
    function(band_code, sx) {
      .build_worldpop_age_band_url(cc, sx, band_code, yr, resolution, release)
    },
    combos$code,
    combos$sex
  )

  list(
    urls = vapply(url_infos, function(info) info$url, character(1)),
    dests = file.path(
      out_dir,
      vapply(url_infos, function(info) info$filename, character(1))
    )
  )
}

# internal helper: fetch every combo's band files in one parallel batch,
# re-downloading corrupt cached files. returns TRUE per complete combo
#' @noRd
.fetch_age_band_files <- function(plan_files, quiet) {
  all_urls <- unlist(lapply(plan_files, function(file_set) file_set$urls))
  all_dests <- unlist(lapply(plan_files, function(file_set) file_set$dests))

  # repair cached files if corrupt
  for (cached_file in unique(all_dests[file.exists(all_dests)])) {
    valid <- tryCatch(
      {
        suppressWarnings(terra::rast(cached_file))
        TRUE
      },
      error = function(e) FALSE
    )
    if (!valid) {
      cli::cli_alert_warning(
        "Corrupt file {basename(cached_file)}, re-downloading"
      )
      unlink(cached_file)
    }
  }

  # dedupe so the same file is never fetched twice concurrently
  unique_idx <- !duplicated(all_dests)
  success <- .worldpop_fetch(
    all_urls[unique_idx], all_dests[unique_idx], quiet
  )
  ok_by_dest <- stats::setNames(success, all_dests[unique_idx])

  vapply(
    plan_files,
    function(file_set) all(ok_by_dest[file_set$dests]),
    logical(1)
  )
}
