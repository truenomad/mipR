# share token for the IHME public Nextcloud folder, used as the WebDAV
# username with an empty password
.ihme_share_token <- "9zFHFLZsCojbnwi"

.ihme_webdav_root <- paste0(
  "https://cloud.ihme.washington.edu/public.php/webdav/",
  "Geospatial%20Vaccine%20Coverage"
)

# end year of the first release this function knows about; candidate years
# run from the current year back to this one
.ihme_first_release_year <- 2025L

# internal helper: files to fetch. {year} marks the release end year
#' @noRd
.ihme_mcv_files <- function() {
  mcv1 <- "MCV/MCV1 - Routine Immunization, Target Age"
  mcv2 <- "MCV/MCV2 - Routine Immunization, Target Age"

  data.frame(
    key = c(
      "mcv1_mean", "mcv1_upper", "mcv1_lower", "mcv1_adm", "mcv2_adm",
      "mcv2_citations", "methods"
    ),
    folder = c(mcv1, mcv1, mcv1, mcv1, mcv2, mcv2, ""),
    remote = c(
      "MCV1_coverage_mean_2000_{year}.tif",
      "MCV1_coverage_upper_2000_{year}.tif",
      "MCV1_coverage_lower_2000_{year}.tif",
      "MCV1_coverage_ad1_ad2_2000-{year}.xlsx",
      "MCV2_coverage_ad1_ad2_2000-{year}.xlsx",
      "MCV2_survey_citations.csv",
      "VCM_Methods_Introduction.pdf"
    ),
    local = c(
      "mcv1_coverage_mean_2000_{year}.tif",
      "mcv1_coverage_upper_2000_{year}.tif",
      "mcv1_coverage_lower_2000_{year}.tif",
      "mcv1_coverage_adm1_adm2_2000_{year}.xlsx",
      "mcv2_coverage_adm1_adm2_2000_{year}.xlsx",
      "mcv2_survey_citations.csv",
      "vcm_methods_introduction.pdf"
    ),
    stringsAsFactors = FALSE
  )
}

# internal helper: WebDAV url for a file in the share, escaping each segment
#' @noRd
.ihme_webdav_url <- function(folder, filename) {
  mapply(
    function(folder, filename) {
      segments <- c(strsplit(folder, "/", fixed = TRUE)[[1]], filename)
      paste(c(.ihme_webdav_root, curl::curl_escape(segments)), collapse = "/")
    },
    folder, filename,
    USE.NAMES = FALSE
  )
}

# internal helper: download files from the share into .part paths, resuming
# any partial download
#' @noRd
.ihme_download <- function(urls, part_paths, quiet) {
  suppressWarnings(curl::multi_download(
    urls, part_paths,
    resume = TRUE, progress = !quiet,
    userpwd = paste0(.ihme_share_token, ":"),
    low_speed_limit = 1024, low_speed_time = 120
  ))
}

# internal helper: authenticated httr2 request against the share
#' @noRd
.ihme_request <- function(url) {
  httr2::request(url) |>
    httr2::req_auth_basic(.ihme_share_token, "") |>
    httr2::req_timeout(60)
}

# internal helper: resolve the release end year of each versioned file. a
# local copy for the current year skips the network; otherwise newer years
# are probed with HEAD requests and the local copy is the fallback
#' @noRd
.ihme_resolve_years <- function(files, dest_dir) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  candidate_years <- seq(
    max(current_year, .ihme_first_release_year),
    .ihme_first_release_year
  )

  versioned <- grepl("{year}", files$remote, fixed = TRUE)
  resolved <- rep(NA_integer_, nrow(files))

  probes <- list()
  for (i in which(versioned)) {
    local_paths <- file.path(
      dest_dir,
      vapply(
        candidate_years,
        function(year) sub("{year}", year, files$local[i], fixed = TRUE),
        character(1)
      )
    )
    local_hit <- which(file.exists(local_paths))[1]
    if (!is.na(local_hit)) resolved[i] <- candidate_years[local_hit]

    # only years newer than the local copy are worth probing
    newer_years <- if (is.na(local_hit)) {
      candidate_years
    } else {
      candidate_years[seq_len(local_hit - 1)]
    }
    for (year in newer_years) {
      probes[[length(probes) + 1]] <- list(row = i, year = year)
    }
  }

  if (length(probes) == 0) {
    return(resolved)
  }

  requests <- lapply(probes, function(probe) {
    remote <- sub("{year}", probe$year, files$remote[probe$row], fixed = TRUE)
    .ihme_webdav_url(files$folder[probe$row], remote) |>
      .ihme_request() |>
      httr2::req_method("HEAD")
  })
  responses <- httr2::req_perform_parallel(
    requests,
    on_error = "continue",
    progress = FALSE
  )
  found <- vapply(
    responses,
    function(response) {
      inherits(response, "httr2_response") &&
        httr2::resp_status(response) == 200
    },
    logical(1)
  )

  # probes run newest first, so the first hit for a row is its latest year
  for (j in which(found)) {
    row <- probes[[j]]$row
    year <- probes[[j]]$year
    if (is.na(resolved[row]) || year > resolved[row]) resolved[row] <- year
  }

  resolved
}

# internal helper: newest release document, from one PROPFIND on the top
# folder. returns the remote filename, or NA if the listing fails
#' @noRd
.ihme_latest_release_document <- function() {
  response <- tryCatch(
    .ihme_request(paste0(.ihme_webdav_root, "/")) |>
      httr2::req_method("PROPFIND") |>
      httr2::req_headers(Depth = "1") |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(response)) {
    return(NA_character_)
  }

  hrefs <- regmatches(
    httr2::resp_body_string(response),
    gregexpr("<d:href>[^<]+</d:href>", httr2::resp_body_string(response))
  )[[1]]
  filenames <- curl::curl_unescape(basename(gsub("</?d:href>", "", hrefs)))

  .newest_release_document(
    filenames,
    "^VCM_Version_([0-9]+)_Release_Document_.*\\.pdf$"
  )
}

# internal helper: pick the highest-numbered release document from a set of
# filenames, NA if none match
#' @noRd
.newest_release_document <- function(filenames, pattern) {
  matches <- filenames[grepl(pattern, filenames)]
  if (length(matches) == 0) {
    return(NA_character_)
  }
  versions <- as.integer(sub(pattern, "\\1", matches))
  matches[which.max(versions)]
}

#' Download IHME Geospatial MCV Coverage Estimates
#'
#' Downloads the IHME geospatial measles-containing vaccine (MCV) coverage
#' estimates and the accompanying methods documentation from the IHME public
#' data share into one folder. The latest release is found automatically.
#'
#' @param dest_dir Directory for the downloaded files (default: current
#'   directory). Created if it does not exist.
#' @param quiet Logical; if TRUE, suppresses progress messages
#'   (default: FALSE)
#'
#' @return Invisible named character vector of local file paths, with `NA`
#'   for any file that could not be obtained:
#'   \describe{
#'     \item{mcv1_mean, mcv1_upper, mcv1_lower}{MCV1 coverage rasters
#'       (mean, upper and lower uncertainty bounds), one band per year}
#'     \item{mcv1_adm, mcv2_adm}{MCV1 and MCV2 coverage at admin 1 and
#'       admin 2 (Excel)}
#'     \item{mcv2_citations}{Survey citations for the MCV2 estimates (CSV)}
#'     \item{methods}{Methods introduction (PDF)}
#'     \item{release_notes}{Release document for the latest model version
#'       (PDF)}
#'   }
#'
#' @details
#' Files are saved under lowercase names, for example
#' `mcv1_coverage_mean_2000_2025.tif` and
#' `mcv1_coverage_adm1_adm2_2000_2025.xlsx`.
#'
#' IHME publishes a new release each year, and the end year in the filenames
#' moves with it. For each versioned file the function looks for releases
#' from the current year back to 2025 and uses the newest one available. A
#' file already in `dest_dir` for the current year is not checked or
#' downloaded again. When a newer release appears, it is downloaded next to
#' the older copy, which is kept.
#'
#' The release document is named by model version and month (for example
#' `VCM_Version_18_Release_Document_Sept2026.pdf`), so the newest version is
#' found by listing the share. MCV2 rasters are not published in the share.
#'
#' The three rasters are about 300 MB each and are downloaded concurrently.
#' Interrupted downloads are resumed on the next call.
#'
#' @examples
#' \dontrun{
#' paths <- get_ihme_mcv(dest_dir = "data/ihme")
#' mcv1_mean <- terra::rast(paths[["mcv1_mean"]])
#' }
#' @export
get_ihme_mcv <- function(dest_dir = ".", quiet = FALSE) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  files <- .ihme_mcv_files()

  if (!quiet) {
    cli::cli_alert_info("Checking the IHME share for the latest release")
  }
  years <- .ihme_resolve_years(files, dest_dir)

  versioned <- grepl("{year}", files$remote, fixed = TRUE)
  resolved <- !versioned | !is.na(years)
  for (i in which(versioned & resolved)) {
    files$remote[i] <- sub("{year}", years[i], files$remote[i], fixed = TRUE)
    files$local[i] <- sub("{year}", years[i], files$local[i], fixed = TRUE)
  }
  for (i in which(!resolved)) {
    cli::cli_alert_danger(
      "No release found on the IHME share for {.file {files$remote[i]}}"
    )
  }

  # release document: newest on the share, else the newest local copy
  release_pattern <- "^vcm_version_([0-9]+)_release_document_.*\\.pdf$"
  release_remote <- .ihme_latest_release_document()
  release_local <- if (is.na(release_remote)) {
    .newest_release_document(list.files(dest_dir), release_pattern)
  } else {
    tolower(release_remote)
  }
  files <- rbind(
    files,
    data.frame(
      key = "release_notes",
      folder = "",
      remote = release_remote,
      local = release_local,
      stringsAsFactors = FALSE
    )
  )
  resolved <- c(resolved, !is.na(release_local))
  if (is.na(release_local)) {
    cli::cli_alert_danger("No release document found on the IHME share")
  }

  dests <- rep(NA_character_, nrow(files))
  dests[resolved] <- file.path(dest_dir, files$local[resolved])

  exists_idx <- which(resolved & file.exists(dests))
  if (!quiet) {
    for (i in exists_idx) {
      cli::cli_alert_info("Skipping {basename(dests[i])} (already exists)")
    }
  }

  fetch_idx <- which(resolved & !file.exists(dests) & !is.na(files$remote))
  if (length(fetch_idx) > 0) {
    urls <- .ihme_webdav_url(files$folder[fetch_idx], files$remote[fetch_idx])
    part_paths <- paste0(dests[fetch_idx], ".part")

    results <- .ihme_download(urls, part_paths, quiet)
    fetched <- results$success %in% TRUE & file.exists(part_paths)

    # the share can drop a file while the large rasters are downloading, so
    # retry failures one at a time
    for (j in which(!fetched)) {
      results[j, ] <- .ihme_download(urls[j], part_paths[j], quiet)
      fetched[j] <- results$success[j] %in% TRUE & file.exists(part_paths[j])
    }

    file.rename(part_paths[fetched], dests[fetch_idx][fetched])

    # keep interrupted downloads for resuming, but drop error-page bodies
    http_error <- !fetched & results$status_code %in% 400:599
    unlink(part_paths[http_error])

    for (j in which(!fetched)) {
      reason <- if (is.na(results$error[j])) {
        paste("HTTP", results$status_code[j])
      } else {
        results$error[j]
      }
      cli::cli_alert_danger(
        "Failed to download {files$remote[fetch_idx[j]]} ({reason})"
      )
    }
  }

  paths <- ifelse(file.exists(dests) %in% TRUE, dests, NA_character_)
  names(paths) <- files$key

  if (!quiet) {
    cli::cli_alert_success(paste0(
      "IHME MCV files ready: {sum(!is.na(paths))} of {length(paths)} ",
      "in {.path {dest_dir}}"
    ))
  }

  invisible(paths)
}

# admin-level tables -----------------------------------------------------------

#' Read IHME MCV Coverage Estimates by Administrative Unit
#'
#' Reads the IHME admin 1 or admin 2 coverage table for MCV1 or MCV2, as
#' downloaded by [get_ihme_mcv()], into a tidy data frame. IHME publishes
#' MCV2 only as these tables, not as rasters.
#'
#' @details
#' The unit names are IHME's own and may be spelled differently from the
#' boundaries in use. Match them before joining, for example with
#' `sntutils::prep_geonames()` against the output of [get_boundaries()].
#' Once the names match, the `adm1_code` and `adm2_code` columns of the
#' boundaries can be joined on, since they are made from the names.
#'
#' Coverage is a proportion between 0 and 1. `pop_under_1` is IHME's
#' population under 1 year in the unit and year.
#'
#' @param path Directory used by [get_ihme_mcv()], or the path to the Excel
#'   table itself. In a directory, the newest release of the table for
#'   `dose` is used.
#' @param dose `"mcv1"` or `"mcv2"` (default).
#' @param adm_level Administrative level of the table: `1` (default) or
#'   `2`.
#' @param country Optional ISO3 codes to keep. Case is ignored. Default
#'   `NULL`, every country in the table.
#'
#' @return A data frame with `iso3`, `adm0`, `adm1`, `adm2` (admin 2
#'   only), `year`, `dose`, `coverage_mean`, `coverage_lower`,
#'   `coverage_upper` and `pop_under_1`.
#'
#' @examples
#' \dontrun{
#' get_ihme_mcv(dest_dir = "data/ihme")
#' mcv2 <- read_ihme_mcv_admin("data/ihme", dose = "mcv2", country = "NGA")
#' }
#' @export
read_ihme_mcv_admin <- function(
  path,
  dose = c("mcv2", "mcv1"),
  adm_level = 1,
  country = NULL
) {
  dose <- match.arg(dose)
  if (!adm_level %in% c(1, 2)) {
    cli::cli_abort("{.arg adm_level} must be 1 or 2.")
  }

  file <- .ihme_admin_table(path, dose)
  sheet <- paste0(toupper(dose), " - Admin ", adm_level)
  if (!sheet %in% readxl::excel_sheets(file)) {
    cli::cli_abort("Sheet {.val {sheet}} not found in {.file {file}}.")
  }
  x <- as.data.frame(readxl::read_excel(file, sheet = sheet))

  label <- toupper(dose)
  needed <- c(
    "ISO3", "Year", "Country", "Admin1",
    paste(label, c("Mean", "Lower", "Upper"), sep = " - "),
    "Population under 1 year",
    if (adm_level == 2) "Admin2"
  )
  missing_cols <- setdiff(needed, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "The IHME table does not have the expected columns.",
      "x" = "Missing: {.val {missing_cols}}"
    ))
  }

  if (!is.null(country)) {
    x <- x[toupper(x$ISO3) %in% toupper(country), ]
    if (nrow(x) == 0) {
      cli::cli_abort(
        "No rows for {.val {toupper(country)}} in the IHME table."
      )
    }
  }

  out <- data.frame(
    iso3 = x$ISO3,
    adm0 = x$Country,
    adm1 = x$Admin1
  )
  if (adm_level == 2) {
    out$adm2 <- x$Admin2
  }
  out$year <- as.integer(x$Year)
  out$dose <- dose
  out$coverage_mean <- x[[paste(label, "- Mean")]]
  out$coverage_lower <- x[[paste(label, "- Lower")]]
  out$coverage_upper <- x[[paste(label, "- Upper")]]
  out$pop_under_1 <- x[["Population under 1 year"]]

  keys <- intersect(c("iso3", "adm1", "adm2", "year"), names(out))
  out <- out[do.call(order, unname(as.list(out[keys]))), ]
  rownames(out) <- NULL
  out
}

# internal helper: the admin table for a dose, from a file path or the
# newest release in a directory
#' @noRd
.ihme_admin_table <- function(path, dose) {
  if (length(path) == 1 && file.exists(path) && !dir.exists(path)) {
    return(path)
  }
  if (!dir.exists(path)) {
    cli::cli_abort("{.path {path}} is not a file or directory.")
  }
  pattern <- paste0("^", dose, "_coverage_adm1_adm2_2000_(\\d{4})\\.xlsx$")
  files <- list.files(path, pattern = pattern, ignore.case = TRUE)
  if (length(files) == 0) {
    cli::cli_abort(c(
      "No {toupper(dose)} admin table in {.path {path}}.",
      "i" = "Download it with {.fn get_ihme_mcv}."
    ))
  }
  years <- as.integer(sub(pattern, "\\1", files, ignore.case = TRUE))
  file.path(path, files[which.max(years)])
}
