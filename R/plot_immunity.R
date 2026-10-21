# palettes ---------------------------------------------------------------------

# preset palettes, ported from sntutils::get_palette(), with the
# susceptibility sequence used for immunity maps
.palette_presets <- list(
  susceptibility = c(
    "#FFF6DD", "#FFE680", "#FFD23F", "#F9B52E", "#F59A2A", "#F07F26",
    "#E05A24", "#C42F1F", "#7F1010", "#5E0B0B", "#420707"
  ),
  protection = c("#FFFFCC", "#FED976", "#FD8D3C", "#E31A1C", "#800026"),
  default = c(
    "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
    "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"
  ),
  byor = c(
    "#084594", "#2171b5", "#6baed6", "#c6dbef",
    "#fed976", "#fd8d3c", "#e31a1c", "#800026"
  ),
  gyor = c(
    "#005a32", "#238b45", "#74c476", "#c7e9c0",
    "#fed976", "#fd8d3c", "#e31a1c", "#800026"
  ),
  ylord = c(
    "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
    "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"
  ),
  ylgnbu = c(
    "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
    "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"
  ),
  blues = c(
    "#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
    "#6baed6", "#4292c6", "#2171b5", "#084594"
  ),
  greens = c(
    "#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b",
    "#74c476", "#41ab5d", "#238b45", "#005a32"
  ),
  reds = c(
    "#fff5f0", "#fee0d2", "#fcbba1", "#fc9272",
    "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"
  ),
  oranges = c(
    "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b",
    "#fd8d3c", "#f16913", "#d94801", "#8c2d04"
  ),
  purples = c(
    "#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc",
    "#9e9ac8", "#807dba", "#6a51a3", "#4a1486"
  ),
  bupu = c(
    "#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda",
    "#8c96c6", "#8c6bb1", "#88419d", "#6e016b"
  ),
  orrd = c(
    "#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84",
    "#fc8d59", "#ef6548", "#d7301f", "#990000"
  ),
  rdylgn = c(
    "#d73027", "#f46d43", "#fdae61", "#fee08b",
    "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
  ),
  rdbu = c(
    "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
    "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"
  ),
  spectral = c(
    "#d53e4f", "#f46d43", "#fdae61", "#fee08b",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd"
  ),
  piyg = c(
    "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef",
    "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221"
  ),
  viridis = c(
    "#440154", "#482878", "#3e4a89", "#31688e", "#26838e",
    "#1f9e89", "#35b779", "#6ece58", "#b5de2b", "#fde725"
  ),
  magma = c(
    "#000004", "#180f3e", "#451077", "#721f81", "#9f2f7f",
    "#cd4071", "#f1605d", "#fd9668", "#feca8d", "#fcfdbf"
  ),
  plasma = c(
    "#0d0887", "#46039f", "#7201a8", "#9c179e", "#bd3786",
    "#d8576b", "#ed7953", "#fa9e3b", "#fdc328", "#f0f921"
  ),
  redblue = c("#d73027", "#f7f7f7", "#4575b4"),
  bluered = c("#4575b4", "#f7f7f7", "#d73027"),
  yellowred = c("#ffffb2", "#bd0026"),
  yellowblue = c("#ffffb2", "#0c2c84"),
  whiteblue = c("#f7fbff", "#08306b"),
  whitered = c("#fff5f0", "#67000d"),
  whitegreen = c("#f7fcf5", "#00441b")
)

#' List Available Colour Palettes
#'
#' Names of the preset palettes that [get_palette()] and the plotting
#' functions accept. The presets follow `sntutils::list_palettes()`, plus
#' `"susceptibility"` and `"protection"` for immunity maps and heatmaps.
#'
#' @return Character vector of palette names.
#'
#' @examples
#' list_palettes()
#' @export
list_palettes <- function() {
  names(.palette_presets)
}

#' Get a Colour Palette
#'
#' Returns a preset palette by name, or passes custom colours through. With
#' `n`, the colours are interpolated to give exactly `n`.
#'
#' @param palette A preset name from [list_palettes()], or a character
#'   vector of colours.
#' @param n Number of colours to return. Default `NULL`, the palette as
#'   defined.
#'
#' @return Character vector of hex colours.
#'
#' @examples
#' get_palette("susceptibility")
#' get_palette("ylord", n = 5)
#' get_palette(c("#ffffcc", "#41b6c4", "#225ea8"), n = 10)
#' @export
get_palette <- function(palette, n = NULL) {
  is_preset <- length(palette) == 1 && palette %in% names(.palette_presets)
  if (length(palette) == 1 && !is_preset &&
        !grepl("^#[0-9A-Fa-f]{6}", palette)) {
    cli::cli_abort(c(
      "Unknown palette {.val {palette}}.",
      "i" = "See {.fn list_palettes} for the presets, or give colours."
    ))
  }
  colours <- if (is_preset) .palette_presets[[palette]] else palette
  if (is.null(n)) {
    return(colours)
  }
  grDevices::colorRampPalette(colours)(n)
}

# saving -----------------------------------------------------------------------

#' Save a Plot, Compressing PNG Files
#'
#' Saves a plot with `ggplot2::ggsave()` and, for PNG files, compresses the
#' file with pngquant as the last step, so the saved file is the compressed
#' one.
#'
#' @param plot A ggplot object.
#' @param path File path. The format comes from the extension.
#' @param width,height Size in inches.
#' @param dpi Resolution. Default `300`.
#' @param compress Logical. Compress PNG files with pngquant. Default `TRUE`.
#' @param speed pngquant speed from 1 (slowest, smallest file) to 10.
#'   Default `1`.
#' @param ... Passed to `ggplot2::ggsave()`.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \dontrun{
#' save_plot(map, "outputs/susceptible_map.png", width = 10, height = 5)
#' }
#' @export
save_plot <- function(
  plot,
  path,
  width,
  height,
  dpi = 300,
  compress = TRUE,
  speed = 1,
  ...
) {
  .check_plot_packages("ggplot2")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    path, plot,
    width = width, height = height, dpi = dpi, bg = "white", ...
  )
  if (compress && grepl("\\.png$", path, ignore.case = TRUE)) {
    compress_png(path, speed = speed)
  }
  invisible(path)
}

#' Compress PNG Files with pngquant
#'
#' Compresses one PNG file, or every PNG file in a folder, in place with
#' pngquant. Ported from `sntutils::compress_png()`.
#'
#' @details
#' pngquant is found from the `PNGQUANT_PATH` environment variable, the
#' system path, or the usual install locations. When it is not found, the
#' files are left as they are and a warning says how to install it.
#'
#' @param path A PNG file or a folder of PNG files.
#' @param speed Speed from 1 (slowest, smallest file) to 10. Default `1`.
#' @param quiet Logical. Suppress the size report. Default `FALSE`.
#'
#' @return A data frame with `file`, `size_before`, `size_after` and
#'   `percent_saved`, invisibly.
#'
#' @examples
#' \dontrun{
#' compress_png("outputs/figures")
#' }
#' @export
compress_png <- function(path, speed = 1, quiet = FALSE) {
  pngquant <- .find_pngquant()
  if (is.null(pngquant)) {
    cli::cli_warn(c(
      "pngquant was not found, so files were not compressed.",
      "i" = "Install it (for example {.code brew install pngquant}) or set
             {.envvar PNGQUANT_PATH}."
    ))
    return(invisible(NULL))
  }
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.png$", full.names = TRUE,
               ignore.case = TRUE)
  } else {
    path
  }
  if (length(files) == 0 || !all(file.exists(files))) {
    cli::cli_abort("No PNG files found at {.path {path}}.")
  }

  results <- dplyr::tibble(file = files) |>
    dplyr::mutate(
      size_before = file.size(.data$file),
      status = purrr::map_int(.data$file, \(f) {
        system2(
          pngquant,
          c("--speed", speed, "--force", "--ext", ".png", shQuote(f)),
          stdout = FALSE, stderr = FALSE
        )
      }),
      size_after = file.size(.data$file),
      percent_saved = 100 * (1 - .data$size_after / .data$size_before)
    )

  if (!quiet) {
    saved <- sum(results$size_before - results$size_after)
    cli::cli_alert_success(
      "Compressed {nrow(results)} PNG file{?s}, saving \\
       {round(saved / 1024)} KB."
    )
  }
  invisible(dplyr::select(results, -"status"))
}

# internal helper: path to pngquant, or NULL when it is not installed
#' @noRd
.find_pngquant <- function() {
  candidates <- c(
    Sys.getenv("PNGQUANT_PATH", unset = ""),
    Sys.which("pngquant"),
    "/opt/homebrew/bin/pngquant",
    "/usr/local/bin/pngquant",
    "/usr/bin/pngquant"
  )
  found <- purrr::keep(
    candidates,
    \(p) nzchar(p) && file.exists(p) && file.access(p, 1) == 0
  )
  if (length(found) == 0) NULL else unname(found[[1]])
}

# theme ------------------------------------------------------------------------

# internal helper: a plain theme for the heatmap
#' @noRd
.theme_mipr <- function(title_size = 14, subtitle_size = 11,
                        legend_title_size = 10) {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = title_size),
      plot.subtitle = ggplot2::element_text(
        colour = "grey35", size = subtitle_size
      ),
      legend.title = ggplot2::element_text(size = legend_title_size),
      plot.caption = ggplot2::element_text(colour = "grey35", hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

#' @noRd
.check_plot_packages <- function(packages) {
  missing_pkgs <- packages[!purrr::map_lgl(
    packages, \(p) requireNamespace(p, quietly = TRUE)
  )]
  if (length(missing_pkgs) > 0) {
    cli::cli_abort(c(
      "{.pkg {missing_pkgs}} {?is/are} needed for this plot.",
      "i" = paste(
        "Install {.pkg ggplot2} from CRAN and {.pkg sntutils} with",
        "{.code remotes::install_github(\"ahadi-analytics/sntutils\")}."
      )
    ))
  }
}

# cohort protection heatmap ----------------------------------------------------

#' Plot Protection Added by Each Campaign, by Birth Cohort
#'
#' Draws a heatmap with one row per birth-year group and one column per
#' campaign, showing the share of the group newly protected by each
#' campaign, beside the group's susceptible share on an evaluation date.
#'
#' @details
#' Values are weighted by each cohort's population on `at`. Infants marked
#' as protected by maternal antibodies are left out. Cells where no
#' cohort in the group was in the campaign's target ages are shown as "Not
#' targeted". Only campaign opportunities are shown; routine doses are part
#' of the susceptible share but have no column.
#'
#' @param immunity The output of [estimate_immunity()].
#' @param at Evaluation month, as a Date or `"YYYY-MM"`.
#' @param birth_years Optional named list of birth-year groups, such as
#'   `list("2015-2019" = 2015:2019, "2020-2024" = 2020:2024)`. Default: one
#'   row per birth year.
#' @param campaigns Optional character vector of `event_id` values, in the
#'   order of the columns. Default: every campaign, in date order.
#' @param palette Colour palette for the protection added: a name from
#'   [list_palettes()] or a vector of colours. Default `"protection"`.
#' @param title,subtitle,caption Plot text.
#' @param output_file Optional file path to save the plot to. Default
#'   `NULL`, not saved.
#' @param width,height Size of the saved plot in inches.
#' @param dpi Resolution of the saved plot. Default `300`.
#' @param scale Scaling factor for the saved plot. Default `1`.
#' @param title_size,subtitle_size,legend_title_size Text sizes in points.
#' @param compress_image Logical. Compress a saved PNG with pngquant.
#'   Default `TRUE`.
#'
#' @return A ggplot object, saved to `output_file` when given.
#'
#' @examples
#' \dontrun{
#' plot_cohort_protection(immunity, at = "2025-06")
#' }
#' @export
plot_cohort_protection <- function(
  immunity,
  at,
  birth_years = NULL,
  campaigns = NULL,
  palette = "protection",
  title = "Protection added by each campaign",
  subtitle = NULL,
  caption = "Vaccination-derived protection only.",
  output_file = NULL,
  width = 9,
  height = 6,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
) {
  .check_plot_packages("ggplot2")
  if (!is.list(immunity) ||
        !all(c("immunity", "contributions") %in% names(immunity))) {
    cli::cli_abort(
      "{.arg immunity} must be the output of {.fn estimate_immunity}."
    )
  }
  at_month <- .month_from_index(.month_index(.as_date(at, "at")))
  at_year <- format(at_month, "%Y")
  geography <- setdiff(
    names(immunity$contributions),
    c("birth_month", "event_id", "type", "month", "reached",
      "newly_protected")
  )

  # population of each cohort on the evaluation date, and its group
  weights <- immunity$immunity |>
    dplyr::filter(.data$month == at_month, !.data$maternal_protection) |>
    dplyr::select(
      dplyr::all_of(geography), "birth_month", "population",
      "susceptible_share"
    ) |>
    dplyr::mutate(group = .birth_group(.data$birth_month, birth_years)) |>
    dplyr::filter(!is.na(.data$group))
  if (nrow(weights) == 0) {
    cli::cli_abort("No cohorts on {.val {format(at_month, '%Y-%m')}}.")
  }

  protection <- immunity$contributions |>
    dplyr::filter(.data$type == "sia", .data$month <= at_month) |>
    dplyr::inner_join(weights, by = c(geography, "birth_month")) |>
    dplyr::summarise(
      value = sum(.data$newly_protected * .data$population) /
        sum(.data$population),
      .by = c("group", "event_id")
    )
  if (is.null(campaigns)) {
    campaigns <- immunity$contributions |>
      dplyr::filter(.data$type == "sia", .data$month <= at_month) |>
      dplyr::summarise(month = min(.data$month), .by = "event_id") |>
      dplyr::arrange(.data$month) |>
      dplyr::pull("event_id")
  }

  groups <- weights |>
    dplyr::summarise(
      susceptible = sum(.data$susceptible_share * .data$population) /
        sum(.data$population),
      first_birth = min(.data$birth_month),
      last_birth = max(.data$birth_month),
      .by = "group"
    ) |>
    dplyr::mutate(
      ages = .age_span(.data$first_birth, .data$last_birth, at_month),
      row = paste0("Born ", .data$group, " (ages ", .data$ages, ")")
    ) |>
    dplyr::arrange(.data$first_birth)

  susceptible_label <- paste0("Susceptible\nin ", at_year, " (%)")
  column_levels <- c(campaigns, susceptible_label)
  row_levels <- rev(groups$row)

  cells <- tidyr::expand_grid(group = groups$group, event_id = campaigns) |>
    dplyr::left_join(protection, by = c("group", "event_id")) |>
    dplyr::left_join(dplyr::select(groups, "group", "row"), by = "group") |>
    dplyr::mutate(
      targeted = !is.na(.data$value),
      pct = 100 * .data$value,
      label = dplyr::if_else(
        .data$targeted, sprintf("%.1f", .data$pct), "Not targeted"
      ),
      column = factor(.data$event_id, column_levels),
      row = factor(.data$row, row_levels)
    )
  top <- max(c(cells$pct, 1e-9), na.rm = TRUE)

  susceptible_cells <- groups |>
    dplyr::mutate(
      column = factor(susceptible_label, column_levels),
      row = factor(.data$row, row_levels),
      label = sprintf("%.1f", 100 * .data$susceptible),
      highest = .data$susceptible == max(.data$susceptible)
    )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = dplyr::filter(cells, .data$targeted),
      ggplot2::aes(.data$column, .data$row, fill = .data$pct),
      colour = "white", linewidth = 1.2
    ) +
    ggplot2::geom_tile(
      data = dplyr::filter(cells, !.data$targeted),
      ggplot2::aes(.data$column, .data$row),
      fill = "grey90", colour = "white", linewidth = 1.2
    ) +
    ggplot2::geom_tile(
      data = susceptible_cells,
      ggplot2::aes(.data$column, .data$row),
      fill = "grey97", colour = "white", linewidth = 1.2
    ) +
    ggplot2::geom_text(
      data = cells,
      ggplot2::aes(
        .data$column, .data$row, label = .data$label,
        colour = dplyr::case_when(
          !.data$targeted ~ "muted",
          .data$pct > 0.55 * top ~ "light",
          .default = "dark"
        )
      ),
      size = 3.6
    ) +
    ggplot2::geom_text(
      data = susceptible_cells,
      ggplot2::aes(
        .data$column, .data$row, label = .data$label,
        fontface = dplyr::if_else(.data$highest, "bold", "plain")
      ),
      colour = "grey15", size = 4
    ) +
    ggplot2::scale_colour_manual(
      values = c(light = "white", dark = "grey15", muted = "grey45"),
      guide = "none"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = get_palette(palette), limits = c(0, top), guide = "none"
    ) +
    ggplot2::scale_x_discrete(position = "top", drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      x = NULL, y = NULL, title = title,
      subtitle = if (is.null(subtitle)) {
        paste0("Additional protection in ", at_year, " (percentage points)")
      } else {
        subtitle
      },
      caption = caption
    ) +
    .theme_mipr(title_size, subtitle_size, legend_title_size)

  if (!is.null(output_file)) {
    save_plot(
      plot, output_file,
      width = width, height = height, dpi = dpi, scale = scale,
      compress = compress_image
    )
  }
  plot
}

# internal helper: the birth-year group of each birth month
#' @noRd
.birth_group <- function(birth_month, birth_years) {
  year <- as.integer(format(birth_month, "%Y"))
  if (is.null(birth_years)) {
    return(as.character(year))
  }
  lookup <- dplyr::tibble(
    group = names(birth_years),
    year = unname(birth_years)
  ) |>
    tidyr::unnest("year")
  lookup$group[match(year, lookup$year)]
}

# internal helper: age range in whole years on a date for a span of births
#' @noRd
.age_span <- function(first_birth, last_birth, at) {
  oldest <- floor(as.numeric(at - first_birth) / 365.25)
  youngest <- floor(as.numeric(at - last_birth) / 365.25)
  dplyr::if_else(
    oldest == youngest,
    as.character(youngest),
    paste0(youngest, "-", oldest)
  )
}

# cohort journeys --------------------------------------------------------------

# colours of the immunity states in cohort journeys
.journey_colours <- c(
  "Susceptible" = "#3E78B2", "Vaccination immune" = "#E08A2E"
)

#' Plot One Birth Cohort Over Time Across Areas
#'
#' Follows one monthly birth cohort from birth to an evaluation month, in
#' areas picked at percentiles of its susceptible share, as stacked areas
#' of the susceptible and vaccination-immune shares. Arrows mark the
#' routine doses and the campaigns that reached the cohort.
#'
#' @details
#' Areas are ranked by the cohort's susceptible share in `at`. For each
#' percentile, the area closest to that percentile of the shares is drawn,
#' and the panel title gives its name, the percentile and its susceptible
#' share in `at`. Panels run from the lowest to the highest
#' susceptible share.
#'
#' Routine doses are always marked. A campaign is marked when it reached at
#' least `min_reach` of the cohort in that area. Campaigns within
#' `merge_days` of each other share one label, and labels that would
#' overlap are stacked.
#'
#' @param immunity The output of [estimate_immunity()].
#' @param birth Birth month of the cohort, as a Date or `"YYYY-MM"`.
#' @param at Evaluation month, as a Date or `"YYYY-MM"`. The plot ends here
#'   and areas are ranked here.
#' @param percentiles Percentiles of the susceptible share at which areas
#'   are picked. Default `seq(0.1, 0.9, by = 0.1)`.
#' @param min_reach Smallest share of the cohort a campaign must reach to be
#'   marked. Default `0.25`.
#' @param label Geography column used to name the areas. Default: the last
#'   geography column, such as `adm1_code`.
#' @param merge_days Campaigns closer than this many days share one label.
#'   Default `75`.
#' @param ncol Number of panels per row. Default `3`.
#' @param title,subtitle,caption Plot text. The default caption gives the
#'   cohort's age in `at`, the month of the shares in the panel titles and
#'   the arrow rule.
#' @param output_file Optional file path to save the plot to. Default
#'   `NULL`, not saved.
#' @param width,height Size of the saved plot in inches.
#' @param dpi Resolution of the saved plot. Default `300`.
#' @param scale Scaling factor for the saved plot. Default `1`.
#' @param title_size,subtitle_size,legend_title_size Text sizes in points.
#' @param compress_image Logical. Compress a saved PNG with pngquant.
#'   Default `TRUE`.
#'
#' @return A ggplot object, saved to `output_file` when given.
#'
#' @examples
#' \dontrun{
#' plot_cohort_journey(immunity, birth = "2019-10", at = "2025-06",
#'                     label = "adm1")
#' }
#' @export
plot_cohort_journey <- function(
  immunity,
  birth,
  at,
  percentiles = seq(0.1, 0.9, by = 0.1),
  min_reach = 0.25,
  label = NULL,
  merge_days = 75,
  ncol = 3,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  output_file = NULL,
  width = 12.4,
  height = 5.6,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
) {
  .check_plot_packages("ggplot2")
  if (!is.list(immunity) ||
        !all(c("immunity", "contributions") %in% names(immunity))) {
    cli::cli_abort(
      "{.arg immunity} must be the output of {.fn estimate_immunity}."
    )
  }
  if (!is.numeric(percentiles) || length(percentiles) == 0 ||
        any(percentiles < 0 | percentiles > 1)) {
    cli::cli_abort("{.arg percentiles} must be between 0 and 1.")
  }
  birth_month <- .month_from_index(.month_index(.as_date(birth, "birth")))
  at_month <- .month_from_index(.month_index(.as_date(at, "at")))
  if (at_month <= birth_month) {
    cli::cli_abort("{.arg at} must be after {.arg birth}.")
  }
  geography <- setdiff(
    names(immunity$contributions),
    c("birth_month", "event_id", "type", "month", "reached",
      "newly_protected")
  )
  if (is.null(label)) {
    label <- geography[length(geography)]
  }
  if (!label %in% geography) {
    cli::cli_abort(
      "{.arg label} must be one of the geography columns: {.val {geography}}."
    )
  }

  cohort <- immunity$immunity |>
    dplyr::filter(
      .data$birth_month == .env$birth_month,
      .data$month >= .env$birth_month, .data$month <= at_month
    ) |>
    dplyr::select(dplyr::all_of(geography), "month", "susceptible_share")
  ranking <- dplyr::filter(cohort, .data$month == at_month)
  if (nrow(ranking) == 0) {
    cli::cli_abort(
      "No cohort born in {.val {format(birth_month, '%Y-%m')}} on
      {.val {format(at_month, '%Y-%m')}}."
    )
  }

  # the area nearest each percentile of the susceptible share
  picked <- purrr::map(percentiles, \(q) {
    target <- stats::quantile(ranking$susceptible_share, q, names = FALSE)
    ranking |>
      dplyr::slice_min(
        abs(.data$susceptible_share - target), n = 1, with_ties = FALSE
      ) |>
      dplyr::mutate(percentile = q)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$susceptible_share, .data$percentile) |>
    dplyr::mutate(
      panel = paste0(
        .data[[label]], " \u00b7 ", .ordinal(round(100 * .data$percentile)),
        " percentile (", round(100 * .data$susceptible_share), "%)"
      ),
      panel = factor(.data$panel, unique(.data$panel))
    ) |>
    dplyr::select(dplyr::all_of(geography), "panel")

  states <- names(.journey_colours)
  area <- cohort |>
    dplyr::inner_join(picked, by = geography, relationship = "many-to-many") |>
    dplyr::transmute(
      .data$panel, .data$month,
      Susceptible = .data$susceptible_share,
      `Vaccination immune` = 1 - .data$susceptible_share
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(states), names_to = "state", values_to = "share"
    ) |>
    dplyr::mutate(state = factor(.data$state, rev(states)))

  # routine doses, and campaigns that reached enough of the cohort
  events <- immunity$contributions |>
    dplyr::filter(
      .data$birth_month == .env$birth_month, .data$month <= at_month,
      .data$reached > 0,
      .data$type != "sia" | .data$reached >= min_reach
    ) |>
    dplyr::inner_join(picked, by = geography, relationship = "many-to-many") |>
    dplyr::summarise(
      month = min(.data$month), .by = c("panel", "event_id", "type")
    )
  tags <- .journey_tags(events, merge_days)
  top <- max(1.62, max(c(tags$y, 0)) + 0.16)

  if (is.null(caption)) {
    caption <- paste0(
      "Born ", format(birth_month, "%b %Y"), ", age ",
      .age_months_text(birth_month, at_month), " in ",
      format(at_month, "%b %Y"), ". Vaccination-derived immunity only.\n",
      "Panel titles give the share of the cohort susceptible in ",
      format(at_month, "%b %Y"), ".\n",
      "Arrows: routine doses and campaigns reaching at least ",
      round(100 * min_reach), "% of the cohort."
    )
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_area(
      data = area,
      ggplot2::aes(.data$month, .data$share, fill = .data$state),
      position = "stack"
    ) +
    ggplot2::geom_segment(
      data = events,
      ggplot2::aes(
        x = .data$month, xend = .data$month, y = 1.28, yend = 1.08
      ),
      colour = "grey50", linewidth = 0.3,
      arrow = grid::arrow(length = grid::unit(1.1, "mm"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = tags,
      ggplot2::aes(.data$month, .data$y, label = .data$label),
      size = 2.3, colour = "grey40", vjust = 0
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), ncol = ncol) +
    ggplot2::scale_fill_manual(
      values = .journey_colours, breaks = states, name = NULL
    ) +
    ggplot2::scale_x_date(
      limits = c(birth_month, at_month), date_breaks = "1 year",
      date_labels = "%Y", expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, top), clip = "off") +
    ggplot2::labs(
      x = NULL, y = "Share of cohort (0-100%)", title = title,
      subtitle = subtitle, caption = caption
    ) +
    .theme_mipr(title_size, subtitle_size, legend_title_size) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 12)
      ),
      panel.spacing.x = grid::unit(2, "lines"),
      panel.spacing.y = grid::unit(1.2, "lines"),
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 0.43
      ),
      legend.position = "top",
      legend.justification = "left",
      legend.key.height = grid::unit(3, "mm"),
      legend.key.width = grid::unit(6, "mm")
    )

  if (!is.null(output_file)) {
    save_plot(
      plot, output_file,
      width = width, height = height, dpi = dpi, scale = scale,
      compress = compress_image
    )
  }
  plot
}

# internal helper: arrow labels for each panel, with nearby campaigns merged
# into one label and overlapping labels stacked in rows
#' @noRd
.journey_tags <- function(events, merge_days) {
  if (nrow(events) == 0) {
    return(dplyr::tibble(
      panel = events$panel, month = as.Date(character()),
      label = character(), y = numeric()
    ))
  }
  events |>
    dplyr::mutate(routine = .data$type != "sia") |>
    dplyr::arrange(.data$panel, .data$month) |>
    dplyr::mutate(
      gap = as.numeric(.data$month - dplyr::lag(.data$month)),
      group = cumsum(
        .data$routine | is.na(.data$gap) | .data$gap > merge_days
      ),
      .by = "panel"
    ) |>
    dplyr::summarise(
      label = dplyr::case_when(
        dplyr::first(.data$type) != "sia" ~
          toupper(dplyr::first(.data$type)),
        dplyr::n() == 1 ~ format(dplyr::first(.data$month), "%b %Y"),
        format(min(.data$month), "%Y") == format(max(.data$month), "%Y") ~
          paste0(
            format(min(.data$month), "%b"), "-",
            format(max(.data$month), "%b %Y")
          ),
        .default = paste0(
          format(min(.data$month), "%b %Y"), "-",
          format(max(.data$month), "%b %Y")
        )
      ),
      month = min(.data$month) + (max(.data$month) - min(.data$month)) / 2,
      .by = c("panel", "group")
    ) |>
    dplyr::arrange(.data$panel, .data$month) |>
    dplyr::mutate(row = .label_rows(.data$month, .data$label), .by = "panel") |>
    dplyr::mutate(y = 1.31 + 0.12 * (.data$row - 1)) |>
    dplyr::select("panel", "month", "label", "y")
}

# internal helper: the lowest row where each label clears the labels already
# placed, with label widths approximated in days per character
#' @noRd
.label_rows <- function(date, label, days_per_char = 38, gap_days = 60) {
  half <- days_per_char * nchar(label) / 2
  placed <- purrr::reduce2(
    as.numeric(date) - half, as.numeric(date) + half,
    \(acc, lo, hi) {
      free <- which(acc$ends + gap_days < lo)
      row <- if (length(free) > 0) min(free) else length(acc$ends) + 1L
      list(ends = replace(acc$ends, row, hi), rows = c(acc$rows, row))
    },
    .init = list(ends = numeric(), rows = integer())
  )
  placed$rows
}

# internal helper: an ordinal number, such as 10th or 22nd
#' @noRd
.ordinal <- function(x) {
  suffix <- dplyr::case_when(
    x %% 100 %in% 11:13 ~ "th",
    x %% 10 == 1 ~ "st",
    x %% 10 == 2 ~ "nd",
    x %% 10 == 3 ~ "rd",
    .default = "th"
  )
  paste0(x, suffix)
}

# internal helper: age in years and months, such as 5y 8m
#' @noRd
.age_months_text <- function(birth, at) {
  months <- .month_index(at) - .month_index(birth)
  paste0(months %/% 12, "y ", months %% 12, "m")
}

# maps -------------------------------------------------------------------------

#' Map the Susceptible Share by Area
#'
#' Draws choropleth maps of the susceptible share from
#' [summarise_immunity()], one panel per age band or month, with
#' `sntutils::facetted_map_bins()` or `sntutils::facetted_map_gradient()`.
#'
#' @details
#' By default the scale comes from the data being mapped: bins of 0-5,
#' 5-10 and 10-20%, then steps of 20 points up to the highest share, or a
#' gradient from 0 to the same upper bound. The lightest colour is used
#' only for a bin below 5%. Pass `breaks` (bins) or `limits` (gradient) to fix
#' the scale, for example so that several maps or rounds share one scale.
#' Bin colours are tied to their bin, so a bin with no areas does not shift
#' the others.
#'
#' The summary is joined to `boundaries` on the geography columns they
#' share, such as `adm1_code`.
#'
#' @param summary The output of [summarise_immunity()].
#' @param boundaries An `sf` object of the same areas, such as the output of
#'   [get_boundaries()].
#' @param facet Column to draw one panel per value of: `"age_band"`
#'   (default) or `"month"`.
#' @param type `"bins"` (default) or `"gradient"`.
#' @param breaks Bin breaks in percent, for `type = "bins"`. Default: from
#'   the data.
#' @param limits Lower and upper bound in percent, for
#'   `type = "gradient"`. Default: from the data.
#' @param palette Colours from low to high susceptibility: a name from
#'   [list_palettes()] or a vector of colours. Default `"susceptibility"`.
#' @param adm1_shp Optional `sf` object of higher-level boundaries drawn as
#'   outlines.
#' @param title,subtitle,caption Plot text. With panels by age band, the
#'   default subtitle gives the month of the summary.
#' @param ncol Number of panels per row. Default `3`.
#' @param output_file Optional file path to save the plot to. Default
#'   `NULL`, not saved.
#' @param width,height Size of the saved plot in inches.
#' @param dpi Resolution of the saved plot. Default `300`.
#' @param scale Scaling factor for the saved plot. Default `1`.
#' @param title_size,subtitle_size,legend_title_size Text sizes in points.
#' @param compress_image Logical. Compress a saved PNG with pngquant.
#'   Default `TRUE`.
#'
#' @return The plot returned by the `sntutils` function, saved to
#'   `output_file` when given.
#'
#' @examples
#' \dontrun{
#' summary <- summarise_immunity(immunity, at = "2025-06")
#' map_immunity(summary, boundaries = shp)
#' }
#' @export
map_immunity <- function(
  summary,
  boundaries,
  facet = c("age_band", "month"),
  type = c("bins", "gradient"),
  breaks = NULL,
  limits = NULL,
  palette = "susceptibility",
  adm1_shp = NULL,
  title = "Share of children susceptible to measles",
  subtitle = NULL,
  caption = "Vaccination-derived immunity only.",
  ncol = 3,
  output_file = NULL,
  width = 7,
  height = 10,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
) {
  .check_plot_packages(c("ggplot2", "sntutils"))
  facet <- match.arg(facet)
  type <- match.arg(type)
  if (!inherits(boundaries, "sf")) {
    cli::cli_abort("{.arg boundaries} must be an {.cls sf} object.")
  }
  needed <- c(facet, "susceptible_share")
  if (!all(needed %in% names(summary))) {
    cli::cli_abort(
      "{.arg summary} must be the output of {.fn summarise_immunity}."
    )
  }
  join_cols <- intersect(
    setdiff(names(summary), c(
      "month", "age_band", "population", "susceptible", "susceptible_share",
      "immune_share", "dose_0", "dose_1", "dose_2", "dose_3_plus"
    )),
    names(boundaries)
  )
  if (length(join_cols) == 0) {
    cli::cli_abort(
      "{.arg summary} and {.arg boundaries} share no geography columns."
    )
  }
  if (any(!is.finite(summary$susceptible_share)) ||
      any(summary$susceptible_share < 0 | summary$susceptible_share > 1)) {
    cli::cli_abort("Susceptible shares must be finite proportions.")
  }

  data <- boundaries |>
    dplyr::select(dplyr::all_of(join_cols)) |>
    dplyr::inner_join(summary, by = join_cols) |>
    dplyr::mutate(
      susceptible_pct = 100 * .data$susceptible_share,
      panel = if (facet == "month") {
        factor(
          format(.data$month, "%B %Y"),
          unique(format(sort(unique(.data$month)), "%B %Y"))
        )
      } else {
        .data$age_band
      }
    )
  if (nrow(data) == 0) {
    cli::cli_abort("No summary geographies match the boundaries.")
  }

  # panels by age band give the month of the summary in the subtitle
  if (is.null(subtitle) && facet == "age_band" && "month" %in% names(data)) {
    subtitle <- paste(
      format(sort(unique(data$month)), "%B %Y"), collapse = ", "
    )
  }
  scale_top <- max(1, pretty(c(0, max(data$susceptible_pct))))
  if (!is.null(limits) &&
      (!is.numeric(limits) || length(limits) != 2 ||
       any(!is.finite(limits)) || any(diff(limits) <= 0))) {
    cli::cli_abort("{.arg limits} must be two increasing finite numbers.")
  }

  if (type == "gradient") {
    plot <- sntutils::facetted_map_gradient(
      data = data,
      fill_col = "susceptible_pct",
      colors = get_palette(palette),
      limits = if (is.null(limits)) c(0, scale_top) else limits,
      facet_col = "panel",
      adm1_shp = adm1_shp,
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill_label = "Susceptible (%)",
      ncol = ncol,
      title_size = title_size,
      subtitle_size = subtitle_size,
      legend_title_size = legend_title_size
    )
    return(.finish_map(
      plot, output_file, width, height, dpi, scale, compress_image
    ))
  }

  if (is.null(breaks)) {
    breaks <- .default_breaks(scale_top)
  }
  if (!is.numeric(breaks) || length(breaks) == 0 ||
      any(!is.finite(breaks)) || any(diff(breaks) <= 0) ||
      min(breaks) != 0 || max(breaks) > 100) {
    cli::cli_abort(
      "{.arg breaks} must increase from zero and stay within 0 to 100."
    )
  }
  bands <- .percent_bands(breaks, get_palette(palette))
  data <- data |>
    dplyr::mutate(
      band = cut(
        .data$susceptible_pct,
        breaks = c(breaks, Inf), labels = bands$label, right = FALSE,
        include.lowest = TRUE
      )
    )
  fill_colors <- stats::setNames(bands$colour, bands$label)

  plot <- sntutils::facetted_map_bins(
    data = data,
    fill_col = "band",
    facet_col = "panel",
    adm1_shp = adm1_shp,
    fill_colors = fill_colors,
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill_label = "Susceptible (%)",
    ncol = ncol,
    title_size = title_size,
    subtitle_size = subtitle_size,
    legend_title_size = legend_title_size
  )
  .finish_map(plot, output_file, width, height, dpi, scale, compress_image)
}

# internal helper: give a map a white background and save it when asked
#' @noRd
.finish_map <- function(plot, output_file, width, height, dpi, scale,
                        compress_image) {
  plot <- plot +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  if (!is.null(output_file)) {
    save_plot(
      plot, output_file,
      width = width, height = height, dpi = dpi, scale = scale,
      compress = compress_image
    )
  }
  plot
}

# internal helper: labels and colours for percent bins, spread evenly along
# the palette and named by bin so colours stay with their bin
#' @noRd
.percent_bands <- function(breaks, colours) {
  ramp <- grDevices::colorRampPalette(colours)(101)
  dplyr::tibble(lower = breaks, upper = c(breaks[-1], NA)) |>
    dplyr::mutate(
      label = dplyr::if_else(
        is.na(.data$upper),
        paste0(.data$lower, "% or more"),
        paste0(.data$lower, "-", .data$upper, "%")
      ),
      # the lightest colour is kept for a bin below 5%; the other bins
      # spread over the rest of the palette
      lightest = !is.na(.data$upper) & .data$upper <= 5,
      step = cumsum(!.data$lightest),
      position = dplyr::if_else(
        .data$lightest,
        0,
        0.15 + 0.85 * (.data$step - 1) / max(max(.data$step) - 1, 1)
      ),
      colour = ramp[round(.data$position * 100) + 1]
    ) |>
    dplyr::select("label", "colour")
}

# internal helper: default bins in percent. finer below 20% (0-5, 5-10,
# 10-20), then steps of 20 up to the top of the scale
#' @noRd
.default_breaks <- function(scale_top) {
  c(0, 5, 10, seq(20, max(20, scale_top), by = 20)) |>
    unique() |>
    purrr::keep(\(b) b == 0 || b < scale_top)
}
