#' Extract Raster Values for Administrative Boundaries
#'
#' Summarises one or more rasters over the polygons of a boundary layer and
#' returns one row per polygon, file and layer. The year, month and single
#' year of age are read from each filename. With `pop_rasters`, values can
#' also be weighted by population, for example coverage or prevalence
#' weighted by the number of people in each cell.
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
#' **Population weights.** `"weighted_mean"` and `"weighted_median"` need
#' `pop_rasters`. A single population raster is used for every value
#' raster. With several, the year of each value raster (or layer) is
#' matched to the population raster with the same year, read from the
#' filenames in the same way. When no population raster has that year, the
#' nearest year is used and a warning lists the substitutions. The
#' population raster is cropped to the boundaries, projected to the value
#' raster's CRS if needed, and summed onto the value raster's grid, so each
#' value cell is weighted by the population it holds. Cells without
#' population get no weight. `pop_sum` is the population in each polygon,
#' taken from the population raster at its own resolution.
#'
#' The boundaries are transformed to the raster's CRS when they differ.
#'
#' @param rasters Character vector of raster file paths, or a single
#'   directory whose files matching `pattern` are used.
#' @param shapefile An `sf` object of boundary polygons, for example from
#'   [get_boundaries()].
#' @param id_cols Columns of `shapefile` to keep in the output. Default
#'   `c("adm0", "adm1", "adm2")`.
#' @param fun Statistic or statistics to compute: `"sum"` (default, for
#'   counts such as population, rounded to whole numbers), `"mean"`,
#'   `"median"`, `"min"`, `"max"`, `"weighted_mean"` or `"weighted_median"`.
#'   The weighted statistics need `pop_rasters`.
#' @param pattern Regular expression for the files to use when `rasters` is
#'   a directory, matched against the filenames (case is ignored). Default
#'   `"\\.tiff?$"`, every `.tif` and `.tiff` file. For example,
#'   `"_age\\d+_\\d{4}\\.tif$"` keeps only the single-year rasters
#'   written by [get_worldpop_single_age()].
#' @param pop_rasters Optional population raster file paths, or a single
#'   directory whose files matching `pop_pattern` are used, for the weighted
#'   statistics. Default `NULL`.
#' @param pop_pattern Regular expression for the files to use when
#'   `pop_rasters` is a directory. Default `"_age00_\\d{4}\\.tif$"`, the
#'   under-one population written by [get_worldpop_single_age()], one
#'   raster per year.
#' @param quiet Logical. If `TRUE`, suppresses progress messages.
#'
#' @return A data frame with the `id_cols`, any of `year`, `month`, `age`
#'   and `layer` that apply, one column per statistic in `fun`, and
#'   `pop_sum` (rounded) when `pop_rasters` is given.
#'
#' @examples
#' \dontrun{
#' adm2 <- get_boundaries("NGA", adm_level = 2)
#' paths <- get_worldpop_single_age("NGA", 2025, ages = 1:4)
#'
#' pop_by_age <- extract_raster(paths, adm2)
#'
#' # from a folder that also holds other rasters
#' pop_by_age <- extract_raster(
#'   "data/worldpop", adm2,
#'   pattern = "_age\\d+_\\d{4}\\.tif$"
#' )
#'
#' # MCV1 coverage weighted by population, matched by year
#' mcv1 <- extract_raster(
#'   "data/ihme/mcv1_coverage_mean_2000_2025.tif", adm2,
#'   fun = c("mean", "weighted_mean"),
#'   pop_rasters = "data/worldpop"
#' )
#' }
#' @export
extract_raster <- function(
  rasters,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  fun = "sum",
  pattern = "\\.tiff?$",
  pop_rasters = NULL,
  pop_pattern = "_age00_\\d{4}\\.tif$",
  quiet = FALSE
) {
  weighted_funs <- c("weighted_mean", "weighted_median")
  fun <- match.arg(
    fun,
    c("sum", "mean", "median", "min", "max", weighted_funs),
    several.ok = TRUE
  )
  if (any(fun %in% weighted_funs) && is.null(pop_rasters)) {
    cli::cli_abort(
      "{.arg pop_rasters} is needed for {.val {intersect(fun, weighted_funs)}}."
    )
  }
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("{.arg shapefile} must be an {.cls sf} object.")
  }
  missing_cols <- setdiff(id_cols, names(shapefile))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} {.val {missing_cols}} not in {.arg shapefile}.")
  }

  rasters <- .list_rasters(rasters, pattern, "rasters")
  file_info <- .raster_file_info(rasters)
  ids <- sf::st_drop_geometry(shapefile)[, id_cols, drop = FALSE]

  pop <- NULL
  if (!is.null(pop_rasters)) {
    pop_files <- .list_rasters(pop_rasters, pop_pattern, "pop_rasters")
    pop <- list(
      files = pop_files,
      years = .raster_file_info(pop_files)$year,
      substituted = character(0)
    )
  }
  plain <- setdiff(fun, weighted_funs)

  results <- lapply(seq_along(rasters), function(i) {
    if (!quiet) {
      cli::cli_alert_info("Extracting {basename(rasters[i])}")
    }
    r <- terra::rast(rasters[i])
    shp <- shapefile
    if (sf::st_crs(shp) != sf::st_crs(terra::crs(r))) {
      shp <- sf::st_transform(shp, terra::crs(r))
    }
    r <- terra::crop(r, terra::vect(shp), snap = "out")

    layers <- lapply(seq_len(terra::nlyr(r)), function(k) {
      info <- file_info[rep(i, nrow(ids)), , drop = FALSE]
      layer_year <- NA_integer_
      if (terra::nlyr(r) > 1) {
        layer_year <- .last_plausible_year(names(r)[k])
      }
      year_k <- if (!is.na(layer_year)) layer_year else info$year[1]
      if (is.null(year_k)) {
        year_k <- NA_integer_
      }

      stats <- data.frame(row.names = seq_len(nrow(ids)))
      if (length(plain) > 0) {
        stats <- exactextractr::exact_extract(
          r[[k]], shp,
          fun = plain, force_df = TRUE, progress = FALSE
        )
        names(stats) <- plain
      }
      if (!is.null(pop)) {
        pop_file <- .match_pop_raster(pop, year_k)
        if (!is.na(year_k) && !is.na(pop_file$year) &&
              pop_file$year != year_k) {
          pop$substituted <<- unique(c(
            pop$substituted, paste0(year_k, " used ", pop_file$year)
          ))
        }
        weighted <- .weighted_stats(r[[k]], shp, pop_file$path, fun)
        stats <- cbind(stats, weighted)
      }
      if ("sum" %in% names(stats)) {
        stats$sum <- round(stats$sum)
      }

      out <- cbind(ids, info, stats)
      if (terra::nlyr(r) > 1) {
        out$layer <- names(r)[k]
        if (!is.na(layer_year) && "year" %in% names(out)) {
          out$year <- layer_year
        }
      }
      out
    })
    do.call(rbind, layers)
  })

  if (!is.null(pop) && length(pop$substituted) > 0) {
    cli::cli_warn(c(
      "No population raster for some years; the nearest year was used.",
      "i" = "{pop$substituted}"
    ))
  }

  out <- do.call(rbind, .fill_columns(results))
  rownames(out) <- NULL
  front <- intersect(
    c(id_cols, "year", "month", "age", "layer"),
    names(out)
  )
  stat_cols <- c(fun, if (!is.null(pop)) "pop_sum")
  out[, c(front, stat_cols), drop = FALSE]
}

# internal helper: raster files from paths, or from a directory by pattern
#' @noRd
.list_rasters <- function(x, pattern, arg) {
  if (length(x) == 1 && dir.exists(x)) {
    x <- list.files(x, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  }
  if (length(x) == 0) {
    cli::cli_abort(
      "No raster files found for {.arg {arg}} matching {.val {pattern}}."
    )
  }
  missing_files <- x[!file.exists(x)]
  if (length(missing_files) > 0) {
    cli::cli_abort("Raster file{?s} not found: {.file {missing_files}}")
  }
  x
}

# internal helper: the population raster for a year. one raster is used
# for every year; otherwise the same year, or the nearest year
#' @noRd
.match_pop_raster <- function(pop, year) {
  if (length(pop$files) == 1) {
    return(list(path = pop$files, year = pop$years[1]))
  }
  years <- pop$years
  if (is.null(years) || anyNA(years)) {
    cli::cli_abort(c(
      "Cannot match population rasters by year.",
      "i" = "Every population raster filename needs a year, or give one raster."
    ))
  }
  if (anyDuplicated(years)) {
    dup <- unique(years[duplicated(years)])
    cli::cli_abort(c(
      "Several population rasters share year{?s} {dup}.",
      "i" = "Use {.arg pop_pattern} to keep one raster per year."
    ))
  }
  if (is.na(year)) {
    cli::cli_abort(c(
      "A value raster has no year to match a population raster to.",
      "i" = "Give a single population raster instead."
    ))
  }
  j <- which.min(abs(years - year))
  list(path = pop$files[j], year = years[j])
}

# internal helper: population-weighted statistics of one layer, with the
# population summed onto the value grid, and the polygon population
#' @noRd
.weighted_stats <- function(layer, shp, pop_path, fun) {
  pop <- terra::rast(pop_path)[[1]]
  shp_pop <- shp
  if (sf::st_crs(shp_pop) != sf::st_crs(terra::crs(pop))) {
    shp_pop <- sf::st_transform(shp_pop, terra::crs(pop))
  }
  pop <- terra::crop(pop, terra::vect(shp_pop), snap = "out")

  out <- data.frame(row.names = seq_len(nrow(shp)))
  out$pop_sum <- round(exactextractr::exact_extract(
    pop, shp_pop, fun = "sum", progress = FALSE
  ))

  weights <- pop
  if (!identical(terra::crs(weights), terra::crs(layer))) {
    weights <- terra::project(weights, terra::crs(layer))
  }
  weights <- terra::resample(weights, layer, method = "sum")

  if ("weighted_mean" %in% fun) {
    out$weighted_mean <- suppressWarnings(exactextractr::exact_extract(
      layer, shp,
      fun = "weighted_mean", weights = weights, default_weight = 0,
      progress = FALSE
    ))
  }
  if ("weighted_median" %in% fun) {
    out$weighted_median <- suppressWarnings(exactextractr::exact_extract(
      layer, shp,
      fun = function(values, coverage_fractions, weights) {
        .weighted_median(values, weights * coverage_fractions)
      },
      weights = weights, default_weight = 0, progress = FALSE
    ))
  }
  out[, c(intersect(fun, c("weighted_mean", "weighted_median")), "pop_sum"),
      drop = FALSE]
}

# internal helper: the value below which half of the total weight lies
#' @noRd
.weighted_median <- function(values, weights) {
  keep <- !is.na(values) & !is.na(weights) & weights > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  values <- values[keep]
  weights <- weights[keep]
  ord <- order(values)
  cumulative <- cumsum(weights[ord]) / sum(weights)
  values[ord][which(cumulative >= 0.5)[1]]
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
