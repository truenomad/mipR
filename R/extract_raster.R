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
#' (a stack) every layer is extracted and named in the `layer` column. Each
#' layer's year comes from its name when the name holds a year (for example
#' `2010` or `mcv1_2010`). Otherwise, when the filename holds a year range
#' with one year per layer, the layers take those years in order: the IHME
#' file `mcv1_coverage_mean_2000_2025.tif` has 26 unnamed layers, which
#' become 2000 to 2025. For that file the ascending order was checked
#' against IHME's own state-level table for Nigeria (correlation 0.998;
#' the reversed or shifted orders fit clearly worse). The layer year is also
#' used to match population rasters.
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
  file_info <- dplyr::as_tibble(.raster_file_info(rasters))
  ids <- shapefile |>
    sf::st_drop_geometry() |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(id_cols))

  pop <- NULL
  if (!is.null(pop_rasters)) {
    pop_files <- .list_rasters(pop_rasters, pop_pattern, "pop_rasters")
    pop <- list(
      files = pop_files,
      years = purrr::pluck(
        .raster_file_info(pop_files), "year",
        .default = NA_integer_
      )
    )
  }

  out <- purrr::map(seq_along(rasters), \(i) {
    if (!quiet) {
      cli::cli_alert_info("Extracting {basename(rasters[i])}")
    }
    .extract_file(
      rasters[i], dplyr::slice(file_info, i), shapefile, ids, fun, pop
    )
  }) |>
    purrr::list_rbind()

  # substitutions are collected from every layer and reported once
  if (!is.null(pop)) {
    substituted <- out$.pop_note |>
      purrr::discard(is.na) |>
      unique()
    if (length(substituted) > 0) {
      cli::cli_warn(c(
        "No population raster for some years; the nearest year was used.",
        "i" = "{substituted}"
      ))
    }
  }

  front <- intersect(
    c(id_cols, "year", "month", "age", "layer"),
    names(out)
  )
  stat_cols <- c(fun, if (!is.null(pop)) "pop_sum")
  dplyr::select(out, dplyr::all_of(c(front, stat_cols)))
}

# internal helper: every layer of one raster file over the polygons, one
# row per polygon and layer
#' @noRd
.extract_file <- function(path, info, shapefile, ids, fun, pop) {
  r <- terra::rast(path)
  shp <- shapefile
  if (sf::st_crs(shp) != sf::st_crs(terra::crs(r))) {
    shp <- sf::st_transform(shp, terra::crs(r))
  }
  r <- terra::crop(r, terra::vect(shp), snap = "out")
  layer_years <- .layer_years(names(r), basename(path))
  file_year <- purrr::pluck(info, "year", 1, .default = NA_integer_)
  info_rows <- dplyr::slice(info, rep(1L, nrow(ids)))

  purrr::map(seq_len(terra::nlyr(r)), \(k) {
    layer_year <- layer_years[k]
    year_k <- if (!is.na(layer_year)) layer_year else file_year
    layer <- terra::subset(r, k)

    stats <- .layer_stats(layer, shp, nrow(ids), fun, pop, year_k)
    out <- dplyr::bind_cols(ids, info_rows, stats)
    if (terra::nlyr(r) > 1) {
      layer_name <- names(r)[k]
      out <- dplyr::mutate(out, layer = layer_name)
      if (!is.na(layer_year)) {
        out <- dplyr::mutate(out, year = layer_year)
      }
    }
    out
  }) |>
    purrr::list_rbind()
}

# internal helper: the plain and population-weighted statistics of one
# layer, with a note when the nearest population year was used
#' @noRd
.layer_stats <- function(layer, shp, n_polygons, fun, pop, year_k) {
  plain <- setdiff(fun, c("weighted_mean", "weighted_median"))
  stats <- dplyr::tibble(.rows = n_polygons)
  if (length(plain) > 0) {
    stats <- exactextractr::exact_extract(
      layer, shp,
      fun = plain, force_df = TRUE, progress = FALSE
    ) |>
      dplyr::as_tibble() |>
      purrr::set_names(plain)
  }
  if (!is.null(pop)) {
    pop_file <- .match_pop_raster(pop, year_k)
    pop_note <- NA_character_
    if (!is.na(year_k) && !is.na(pop_file$year) &&
          pop_file$year != year_k) {
      pop_note <- paste0(year_k, " used ", pop_file$year)
    }
    stats <- dplyr::bind_cols(
      stats,
      .weighted_stats(layer, shp, pop_file$path, fun),
      .pop_note = pop_note
    )
  }
  if ("sum" %in% names(stats)) {
    stats <- dplyr::mutate(stats, sum = round(.data$sum))
  }
  stats
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
  missing_files <- purrr::discard(x, file.exists)
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
    return(list(path = pop$files, year = dplyr::first(pop$years)))
  }
  years <- pop$years
  if (is.null(years) || anyNA(years)) {
    cli::cli_abort(c(
      "Cannot match population rasters by year.",
      "i" = "Every population raster filename needs a year, or give one raster."
    ))
  }
  if (anyDuplicated(years)) {
    dup <- unique(purrr::keep(years, duplicated(years)))
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
  pop <- terra::subset(terra::rast(pop_path), 1)
  shp_pop <- shp
  if (sf::st_crs(shp_pop) != sf::st_crs(terra::crs(pop))) {
    shp_pop <- sf::st_transform(shp_pop, terra::crs(pop))
  }
  pop <- terra::crop(pop, terra::vect(shp_pop), snap = "out")

  pop_sum <- round(exactextractr::exact_extract(
    pop, shp_pop, fun = "sum", progress = FALSE
  ))

  weights <- pop
  if (!identical(terra::crs(weights), terra::crs(layer))) {
    weights <- terra::project(weights, terra::crs(layer))
  }
  weights <- terra::resample(weights, layer, method = "sum")

  wanted <- intersect(fun, c("weighted_mean", "weighted_median"))
  weighted <- purrr::map(purrr::set_names(wanted), \(stat) {
    stat_fun <- if (stat == "weighted_mean") {
      "weighted_mean"
    } else {
      \(values, coverage_fractions, weights) {
        .weighted_median(values, weights * coverage_fractions)
      }
    }
    suppressWarnings(exactextractr::exact_extract(
      layer, shp,
      fun = stat_fun, weights = weights, default_weight = 0,
      progress = FALSE
    ))
  })
  dplyr::tibble(!!!weighted, pop_sum = pop_sum)
}

# internal helper: the value below which half of the total weight lies
#' @noRd
.weighted_median <- function(values, weights) {
  dplyr::tibble(value = values, weight = weights) |>
    dplyr::filter(
      !is.na(.data$value), !is.na(.data$weight), .data$weight > 0
    ) |>
    dplyr::arrange(.data$value) |>
    dplyr::mutate(cumulative = cumsum(.data$weight) / sum(.data$weight)) |>
    dplyr::filter(.data$cumulative >= 0.5) |>
    dplyr::pull(.data$value) |>
    dplyr::first(default = NA_real_)
}

# internal helper: year, month and age read from each filename. returns a
# plain data frame with one row per file and only the columns that apply,
# so absent columns read as NULL
#' @noRd
.raster_file_info <- function(files) {
  stems <- tools::file_path_sans_ext(basename(files))
  ages <- purrr::map_int(stems, \(stem) {
    age <- regmatches(stem, regexpr("(?<=age)\\d+", stem, perl = TRUE))
    if (length(age) == 0) NA_integer_ else as.integer(age)
  })

  # drop age tokens and worldpop group codes so they are not read as dates
  stems <- gsub("age\\d+", "", stems, perl = TRUE)
  stems <- gsub("_[mft]_\\d{1,2}_", "_", stems, perl = TRUE)

  # shared numbers such as version stamps are dropped only when they get in
  # the way, since a shared month is also a shared number
  pattern <- tryCatch(.detect_time_pattern(stems), error = \(e) NULL)
  if (is.null(pattern)) {
    stems <- .drop_common_numbers(stems)
    pattern <- .detect_time_pattern(stems)
  }

  info <- dplyr::tibble(.rows = length(files))
  if (pattern != "none") {
    dates <- purrr::map(stems, \(stem) .parse_date(stem, pattern))
    info <- dplyr::mutate(info, year = purrr::map_int(dates, "year"))
    if (pattern %in% c("monthly", "daily")) {
      info <- dplyr::mutate(info, month = purrr::map_int(dates, "month"))
    }
  }
  if (any(!is.na(ages))) {
    info <- dplyr::mutate(info, age = ages)
  }
  as.data.frame(info)
}

# internal helper: remove standalone numbers that appear in more than half
# of the names, such as version stamps, keeping plausible years
#' @noRd
.drop_common_numbers <- function(names) {
  numbers <- regmatches(
    names,
    gregexpr("(?<![A-Za-z0-9])\\d+(?![A-Za-z0-9])", names, perl = TRUE)
  )
  half_names <- length(names) / 2
  common <- dplyr::tibble(
    number = purrr::list_c(purrr::map(numbers, unique), ptype = character())
  ) |>
    dplyr::count(.data$number) |>
    dplyr::filter(
      .data$n > half_names,
      !grepl("^(19[89]\\d|20\\d{2})$", .data$number)
    ) |>
    dplyr::pull(.data$number)
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

# internal helper: year and month from one name for a known pattern. the
# first format that matches is used
#' @noRd
.parse_date <- function(name, pattern) {
  p <- .date_patterns
  if (pattern == "yearly") {
    return(list(year = .last_plausible_year(name), month = NA_integer_))
  }

  formats <- if (pattern == "daily") {
    list(list(regex = p$daily, year = 1, month = 2))
  } else {
    list(
      list(regex = p$monthly_iso, year = 1, month = 2),
      list(regex = p$monthly_compact, year = 1, month = 2),
      list(regex = p$monthly_euro, year = 2, month = 1)
    )
  }
  formats |>
    purrr::map(\(f) {
      parts <- regmatches(name, regexec(f$regex, name, perl = TRUE))[[1]]
      if (length(parts) == 0) {
        return(NULL)
      }
      list(
        year = as.integer(parts[f$year + 1]),
        month = as.integer(parts[f$month + 1])
      )
    }) |>
    purrr::compact() |>
    purrr::pluck(1, .default = list(year = NA_integer_, month = NA_integer_))
}

# internal helper: last plausible year (1980-2099) in a string, or NA
#' @noRd
.last_plausible_year <- function(x) {
  dplyr::last(.plausible_years(x), default = NA_integer_)
}

# internal helper: every plausible year (1980-2099) in a string
#' @noRd
.plausible_years <- function(x) {
  as.integer(regmatches(
    x,
    gregexpr(.date_patterns$yearly, x, perl = TRUE)
  )[[1]])
}

# internal helper: year of each layer of a multi-layer raster, from the
# layer names, or else from a year range in the filename that has one year
# per layer (mcv1_coverage_mean_2000_2025.tif with 26 layers); NA otherwise
#' @noRd
.layer_years <- function(layer_names, file_name) {
  n <- length(layer_names)
  if (n == 1) {
    return(NA_integer_)
  }
  from_names <- purrr::map_int(unname(layer_names), .last_plausible_year)
  if (!all(is.na(from_names))) {
    return(from_names)
  }
  in_file <- .plausible_years(tools::file_path_sans_ext(file_name))
  if (length(in_file) >= 2) {
    first <- min(in_file)
    last <- max(in_file)
    if (last - first + 1L == n) {
      return(seq.int(first, last))
    }
  }
  rep(NA_integer_, n)
}
