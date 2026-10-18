#' Extract Raster Values for Administrative Boundaries
#'
#' Summarises one or more rasters over the polygons of a boundary layer and
#' returns one row per polygon, file and layer. The year, month and single
#' year of age are read from each filename.
#'
#' @details
#' Filenames are read as follows:
#' - **Age:** a token such as `age02` gives `age = 2`, as written by
#'   [get_worldpop_single_age()]. Files without one get `NA`.
#' - **Date:** dates are recognised as `YYYY-MM-DD` (daily), `YYYY-MM`,
#'   `YYYYMM` or `MM-YYYY` (monthly), or `YYYY` (yearly), with `-`, `_` or
#'   `.` as separators. All files must use the same date format. When a
#'   name holds several plausible years (1980-2099), the last one is used.
#' - **Ignored tokens:** age tokens and WorldPop group codes such as `_t_05_`
#'   are ignored when reading dates. If the filenames then mix date formats,
#'   numbers that appear in more than half of them (other than years) are
#'   also ignored before trying again.
#'
#' Files without a date get no time columns. For a multi-layer raster
#' every layer is extracted, and a layer name that is a year (for example
#' `2010` or `mcv1_2010`) overrides the year in the filename.
#'
#' The boundaries are transformed to the raster's CRS when they differ.
#'
#' @param rasters Character vector of raster file paths, or a single
#'   directory whose files matching `pattern` are used.
#' @param shapefile An `sf` object of boundary polygons, for example from
#'   [get_boundaries()].
#' @param id_cols Columns of `shapefile` to keep in the output. Default
#'   `c("adm0", "adm1", "adm2")`.
#' @param fun Summary statistic or statistics passed to
#'   [exactextractr::exact_extract()]: `"sum"` (default, for counts such as
#'   population), `"mean"`, `"median"`, `"min"` or `"max"`.
#' @param pattern Regular expression for the files to use when `rasters` is
#'   a directory, matched against the filenames (case is ignored). Default
#'   `"\\.tiff?$"`, every `.tif` and `.tiff` file. For example,
#'   `"_age\\d+_\\d{4}\\.tif$"` keeps only the single-year rasters
#'   written by [get_worldpop_single_age()].
#' @param quiet Logical. If `TRUE`, suppresses progress messages.
#'
#' @return A data frame with `file_name`, the `id_cols`, any of `year`,
#'   `month`, `age` and `layer` that apply, and one column per statistic in
#'   `fun`.
#'
#' @examples
#' \dontrun{
#' adm2 <- get_boundaries("NGA", adm_level = 2)
#' paths <- get_worldpop_single_age("NGA", 2025, ages = 1:4)
#'
#' pop_by_age <- extract_raster(paths, adm2)
#'
#' # from a folder that also holds the WorldPop age-group rasters
#' pop_by_age <- extract_raster(
#'   "data/worldpop", adm2,
#'   pattern = "_age\\d+_\\d{4}\\.tif$"
#' )
#' }
#' @export
extract_raster <- function(
  rasters,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  fun = "sum",
  pattern = "\\.tiff?$",
  quiet = FALSE
) {
  fun <- match.arg(
    fun,
    c("sum", "mean", "median", "min", "max"),
    several.ok = TRUE
  )
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("{.arg shapefile} must be an {.cls sf} object.")
  }
  missing_cols <- setdiff(id_cols, names(shapefile))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} {.val {missing_cols}} not in {.arg shapefile}.")
  }

  if (length(rasters) == 1 && dir.exists(rasters)) {
    rasters <- list.files(
      rasters,
      pattern = pattern, full.names = TRUE, ignore.case = TRUE
    )
  }
  if (length(rasters) == 0) {
    cli::cli_abort("No raster files found matching {.val {pattern}}.")
  }
  missing_files <- rasters[!file.exists(rasters)]
  if (length(missing_files) > 0) {
    cli::cli_abort("Raster file{?s} not found: {.file {missing_files}}")
  }

  file_info <- .raster_file_info(rasters)
  ids <- sf::st_drop_geometry(shapefile)[, id_cols, drop = FALSE]

  results <- lapply(seq_along(rasters), function(i) {
    if (!quiet) {
      cli::cli_alert_info("Extracting {basename(rasters[i])}")
    }
    r <- terra::rast(rasters[i])
    shp <- shapefile
    if (sf::st_crs(shp) != sf::st_crs(terra::crs(r))) {
      shp <- sf::st_transform(shp, terra::crs(r))
    }

    layers <- lapply(seq_len(terra::nlyr(r)), function(k) {
      stats <- exactextractr::exact_extract(
        r[[k]], shp,
        fun = fun, force_df = TRUE, progress = FALSE
      )
      names(stats) <- fun
      out <- cbind(
        data.frame(file_name = basename(rasters[i])),
        ids,
        file_info[rep(i, nrow(ids)), , drop = FALSE],
        stats
      )
      if (terra::nlyr(r) > 1) {
        layer_name <- names(r)[k]
        out$layer <- layer_name
        layer_year <- .last_plausible_year(layer_name)
        if (!is.na(layer_year) && "year" %in% names(out)) {
          out$year <- layer_year
        }
      }
      out
    })
    do.call(rbind, layers)
  })

  out <- do.call(rbind, .fill_columns(results))
  rownames(out) <- NULL
  front <- intersect(
    c("file_name", id_cols, "year", "month", "age", "layer"),
    names(out)
  )
  out[, c(front, fun), drop = FALSE]
}

# internal helper: year, month and age read from each filename. returns a
# data frame with one row per file and only the columns that apply
#' @noRd
.raster_file_info <- function(files) {
  names <- tools::file_path_sans_ext(basename(files))
  info <- data.frame(row.names = seq_along(files))

  # drop age tokens and worldpop group codes so they are not read as dates
  has_age <- grepl("age\\d+", names, perl = TRUE)
  age <- rep(NA_integer_, length(names))
  age[has_age] <- as.integer(
    regmatches(names, regexpr("(?<=age)\\d+", names, perl = TRUE))
  )
  names <- gsub("age\\d+", "", names, perl = TRUE)
  names <- gsub("_[mft]_\\d{1,2}_", "_", names, perl = TRUE)

  # shared numbers such as version stamps are dropped only when they get in
  # the way, since a shared month is also a shared number
  pattern <- tryCatch(.detect_time_pattern(names), error = function(e) NULL)
  if (is.null(pattern)) {
    names <- .drop_common_numbers(names)
    pattern <- .detect_time_pattern(names)
  }
  if (pattern != "none") {
    dates <- lapply(names, .parse_date, pattern = pattern)
    info$year <- vapply(dates, function(d) d[["year"]], integer(1))
    if (pattern %in% c("monthly", "daily")) {
      info$month <- vapply(dates, function(d) d[["month"]], integer(1))
    }
  }
  if (any(has_age)) {
    info$age <- age
  }
  info
}

# internal helper: remove standalone numbers that appear in more than half
# of the names, such as version stamps, keeping plausible years
#' @noRd
.drop_common_numbers <- function(names) {
  numbers <- regmatches(
    names,
    gregexpr("(?<![A-Za-z0-9])\\d+(?![A-Za-z0-9])", names, perl = TRUE)
  )
  freq <- table(unlist(lapply(numbers, unique)))
  common <- names(freq[freq > length(names) / 2])
  common <- common[!grepl("^(19[89]\\d|20\\d{2})$", common)]
  if (length(common) == 0) {
    return(names)
  }

  pattern <- paste0(
    "(?<![A-Za-z0-9])(", paste(common, collapse = "|"), ")(?![A-Za-z0-9])"
  )
  gsub(pattern, "", names, perl = TRUE)
}

.date_patterns <- list(
  daily = paste0(
    "(?<!\\d)(19[89]\\d|20\\d{2})[._-](0[1-9]|1[0-2])[._-]",
    "(0[1-9]|[12]\\d|3[01])(?!\\d)"
  ),
  monthly_iso = "(?<!\\d)(19[89]\\d|20\\d{2})[._-](0[1-9]|1[0-2])(?!\\d)",
  monthly_compact = "(?<!\\d)(19[89]\\d|20\\d{2})(0[1-9]|1[0-2])(?!\\d)",
  monthly_euro = "(?<!\\d)(0[1-9]|1[0-2])[._-](19[89]\\d|20\\d{2})(?!\\d)",
  yearly = "(?<!\\d)(19[89]\\d|20\\d{2})(?!\\d)"
)

# internal helper: one date format for the whole batch, or "none" when no
# name holds a date. mixed formats stop with the files in each group
#' @noRd
.detect_time_pattern <- function(names) {
  p <- .date_patterns
  is_daily <- grepl(p$daily, names, perl = TRUE)
  is_monthly <- grepl(p$monthly_iso, names, perl = TRUE) |
    grepl(p$monthly_compact, names, perl = TRUE) |
    grepl(p$monthly_euro, names, perl = TRUE)
  is_yearly <- grepl(p$yearly, names, perl = TRUE)

  if (all(is_daily)) {
    return("daily")
  }
  if (all(is_monthly)) {
    return("monthly")
  }
  if (all(is_yearly) && !any(is_monthly)) {
    return("yearly")
  }
  if (!any(is_yearly)) {
    return("none")
  }

  cli::cli_abort(c(
    "Filenames use different date formats.",
    "i" = "Daily: {sum(is_daily)}, monthly: {sum(is_monthly & !is_daily)}, \\
           yearly only: {sum(is_yearly & !is_monthly)}, \\
           no date: {sum(!is_yearly)}",
    "x" = "All files must use the same date format."
  ))
}

# internal helper: year and month from one name for a known pattern
#' @noRd
.parse_date <- function(name, pattern) {
  p <- .date_patterns
  if (pattern == "yearly") {
    return(list(year = .last_plausible_year(name), month = NA_integer_))
  }

  formats <- if (pattern == "daily") {
    list(c(p$daily, 1, 2))
  } else {
    list(
      c(p$monthly_iso, 1, 2),
      c(p$monthly_compact, 1, 2),
      c(p$monthly_euro, 2, 1)
    )
  }
  for (f in formats) {
    m <- regmatches(name, regexec(f[1], name, perl = TRUE))[[1]]
    if (length(m) > 0) {
      parts <- m[-1]
      return(list(
        year = as.integer(parts[as.integer(f[2])]),
        month = as.integer(parts[as.integer(f[3])])
      ))
    }
  }
  list(year = NA_integer_, month = NA_integer_)
}

# internal helper: last plausible year (1980-2099) in a string, or NA
#' @noRd
.last_plausible_year <- function(x) {
  years <- regmatches(
    x,
    gregexpr(.date_patterns$yearly, x, perl = TRUE)
  )[[1]]
  if (length(years) == 0) {
    return(NA_integer_)
  }
  as.integer(years[length(years)])
}

# internal helper: give every data frame the same columns before rbind
#' @noRd
.fill_columns <- function(dfs) {
  all_cols <- unique(unlist(lapply(dfs, names)))
  lapply(dfs, function(df) {
    for (col in setdiff(all_cols, names(df))) {
      df[[col]] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
}
