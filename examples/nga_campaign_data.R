################################################################################
# mipR example: Nigeria measles campaign data, admin 1
################################################################################

## ---------------------------------------------------------------------------##
# Setup and parameters ---------------------------------------------------------
## ---------------------------------------------------------------------------##

raw_dir <- here::here("01_data/1d_campaigns/raw")
processed_dir <- here::here("01_data/1d_campaigns/processed")
name_cache <- here::here("01_data/1d_campaigns/cache/nga_adm1_names.rds")

nbs_url <- "https://microdata.nigerianstat.gov.ng/index.php/catalog"

# the NBS survey microdata need a login, so only the reports are downloaded
sources <- tibble::tribble(
  ~key, ~file, ~url,
  "pmccs", "nga_pmccs_2018_report.pdf", paste0(nbs_url, "/61/download/590"),
  "ipccs", "nga_ipccs_2024_report.pdf", paste0(nbs_url, "/163/download/1179"),
  "ncdc", "nga_ncdc_measles_sitrep_2024_10.pdf",
  paste0(
    "https://ncdc.gov.ng/themes/common/files/sitreps/",
    "cb9bfeb95970347750a43c8eda93d22a.pdf"
  )
)

campaign_pmccs <- "NGA-MSL-2017-2018"
campaign_ipccs <- "NGA-MSL-2024"

# row labels in the report tables that are not states
non_state_labels <- c(
  "NIGERIA", "TOTAL", "NORTH", "SOUTH", "NORTH CENTRAL", "NORTH EAST",
  "NORTH WEST", "SOUTH EAST", "SOUTH SOUTH", "SOUTH WEST", "MALE", "FEMALE",
  "URBAN", "RURAL"
)

## ---------------------------------------------------------------------------##
# Helpers ----------------------------------------------------------------------
## ---------------------------------------------------------------------------##

.download_source <- function(url, path) {
  if (!file.exists(path)) {
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0") |>
      httr2::req_perform(path = path)
  }

  path
}

.read_report <- function(path) {
  pdftools::pdf_text(path) |>
    stringr::str_split("\n") |>
    purrr::list_c()
}

# lines of a report table, from its title to the first line that ends it;
# the last title match is used, since the list of tables comes first
.table_lines <- function(report_lines, title) {
  lines <- tibble::tibble(line = report_lines, row = seq_along(report_lines))
  start <- lines |>
    dplyr::filter(stringr::str_starts(
      stringr::str_trim(.data$line), stringr::fixed(title)
    )) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::pull("row")
  if (length(start) == 0) {
    cli::cli_abort("Table {.val {title}} not found in the report.")
  }
  ending <- lines |>
    dplyr::filter(
      .data$row > start,
      stringr::str_detect(
        .data$line,
        paste0(
          "^\\s*(Abbreviations|The results in this table|Table [0-9]|",
          "Figure [0-9]|[0-9]+\\.[0-9]+\\.[0-9]+ )"
        )
      )
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull("row")
  end <- if (length(ending) == 0) nrow(lines) + 1L else ending
  lines |>
    dplyr::filter(.data$row >= start, .data$row < end) |>
    dplyr::pull("line")
}

# Parse named report fields directly into a tibble.
.report_fields <- function(text, pattern, columns) {
  stringr::str_match(text, pattern) |>
    tibble::as_tibble(.name_repair = \(names) c("matched", columns))
}

# Rows of a state table; confidence bounds are optional.
.parse_state_rows <- function(lines, cols, exact = TRUE) {
  rows <- .report_fields(
    lines, "^\\s*([A-Za-z][A-Za-z ,.'-]*[A-Za-z])\\s{2,}([0-9(].*)$",
    c("adm1_raw", "numbers")
  ) |>
    dplyr::filter(!is.na(.data$matched)) |>
    dplyr::mutate(
      values = .data$numbers |>
        stringr::str_remove_all("\\([^)]*\\)|%") |>
        stringr::str_extract_all("[0-9][0-9,]*(\\.[0-9]+)?"),
      count = lengths(.data$values)
    ) |>
    dplyr::filter(
      if (exact) .data$count == length(cols) else .data$count >= length(cols)
    )
  if (nrow(rows) == 0) {
    cli::cli_abort("No state rows matched the expected report columns.")
  }
  bounds <- .report_fields(
    rows$numbers, "\\(([0-9.]+), ([0-9.]+)\\)",
    c("ci_lower", "ci_upper")
  ) |>
    dplyr::transmute(
      ci_lower = as.numeric(.data$ci_lower),
      ci_upper = as.numeric(.data$ci_upper)
    )
  values <- rows$values |>
    purrr::map(\(values) {
      tibble::tibble(value = values) |>
        dplyr::slice_head(n = length(cols)) |>
        dplyr::mutate(
          name = cols,
          value = as.numeric(stringr::str_remove_all(.data$value, ","))
        ) |>
        tidyr::pivot_wider(names_from = "name", values_from = "value")
    }) |>
    purrr::list_rbind()
  rows |>
    dplyr::transmute(adm1_raw = stringr::str_squish(.data$adm1_raw)) |>
    dplyr::bind_cols(bounds, values) |>
    dplyr::filter(!toupper(.data$adm1_raw) %in% non_state_labels) |>
    dplyr::distinct(.data$adm1_raw, .keep_all = TRUE)
}

## ---------------------------------------------------------------------------##
# Boundaries -------------------------------------------------------------------
## ---------------------------------------------------------------------------##

shp <- mipR::get_boundaries(
  country = "NGA",
  adm_level = 1,
  source = "who",
  cache_dir = here::here("01_data/1a_shapefiles")
)

## ---------------------------------------------------------------------------##
# Source data ------------------------------------------------------------------
## ---------------------------------------------------------------------------##

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

paths <- purrr::map2_chr(
  sources$url, file.path(raw_dir, sources$file), .download_source
) |>
  stats::setNames(sources$key)

pmccs <- .read_report(paths[["pmccs"]])
ipccs <- .read_report(paths[["ipccs"]])
ncdc <- .read_report(paths[["ncdc"]])

## ---------------------------------------------------------------------------##
# 2017-2018 post measles campaign coverage survey ------------------------------
## ---------------------------------------------------------------------------##

pmccs_prior <- .table_lines(pmccs, "Table 2.4a:") |>
  .parse_state_rows(
    c(
      "card_pct", "recall_pct", "no_pct", "unknown_pct", "value_pct", "n",
      "weighted_n"
    )
  ) |>
  dplyr::mutate(indicator = "mcv_before_sia")

pmccs_coverage <- .table_lines(pmccs, "2.5a:") |>
  .parse_state_rows(
    c("card_pct", "recall_pct", "finger_pct", "value_pct", "n", "weighted_n")
  ) |>
  dplyr::mutate(indicator = "sia_coverage")

pmccs_first_dose <- .table_lines(pmccs, "Table 2.6a:") |>
  .parse_state_rows(c("value_pct", "n", "weighted_n")) |>
  dplyr::mutate(indicator = "sia_first_dose")

pmccs_lifetime <- .table_lines(pmccs, "Table 2.8b") |>
  .parse_state_rows(
    c("mcv_0_dose", "mcv_1_dose", "mcv_2plus_dose", "n", "weighted_n")
  ) |>
  tidyr::pivot_longer(
    dplyr::starts_with("mcv_"),
    names_to = "indicator",
    values_to = "value_pct"
  )

pmccs_survey <- dplyr::bind_rows(
  pmccs_prior, pmccs_coverage, pmccs_first_dose, pmccs_lifetime
) |>
  dplyr::transmute(
    campaign_id = campaign_pmccs,
    survey = "PMCCS",
    adm1_raw,
    indicator,
    value_pct,
    lower_pct = ci_lower,
    upper_pct = ci_upper,
    n,
    weighted_n
  )

## ---------------------------------------------------------------------------##
# 2024 integrated post campaign coverage survey --------------------------------
## ---------------------------------------------------------------------------##

ipccs_cols <- c(
  "value_pct", "std_err", "lower_pct", "upper_pct", "deff", "icc", "n",
  "weighted_n"
)

ipccs_coverage <- .table_lines(ipccs, "Table 4.5:") |>
  .parse_state_rows(c("card_pct", "history_pct", "finger_pct", ipccs_cols)) |>
  dplyr::mutate(indicator = "sia_coverage")

ipccs_first_dose <- .table_lines(ipccs, "Table 4.6:") |>
  .parse_state_rows(ipccs_cols) |>
  dplyr::mutate(indicator = "sia_first_dose")

# the first four columns cover all ages; the rest split them by birth cohort
ipccs_lifetime <- .table_lines(ipccs, "Table 4.7:") |>
  .parse_state_rows(
    c("mcv_0_dose", "mcv_1_dose", "mcv_2plus_dose", "n"),
    exact = FALSE
  ) |>
  tidyr::pivot_longer(
    dplyr::starts_with("mcv_"),
    names_to = "indicator",
    values_to = "value_pct"
  )

ipccs_survey <- dplyr::bind_rows(
  ipccs_coverage, ipccs_first_dose, ipccs_lifetime
) |>
  dplyr::transmute(
    campaign_id = campaign_ipccs,
    survey = "IPCCS",
    adm1_raw,
    indicator,
    value_pct,
    lower_pct,
    upper_pct,
    n,
    weighted_n
  )

## ---------------------------------------------------------------------------##
# 2017-2018 campaign calendar --------------------------------------------------
## ---------------------------------------------------------------------------##

# the PMCCS report gives the target age but no campaign dates by state, only
# "October to December 2017 and February/March 2018 for the Northern and
# Southern States respectively"; dates are left missing for completion by
# hand rather than inferred from those periods
pmccs_calendar <- pmccs_survey |>
  dplyr::distinct(campaign_id, adm1_raw) |>
  dplyr::mutate(
    start_date = as.Date(NA),
    end_date = as.Date(NA),
    target_age = "9-59 months",
    source = "PMCCS 2018 report",
    note = paste(
      "state dates not given in the report; campaign ran October to",
      "December 2017 (north) and February to March 2018 (south)"
    )
  )

## ---------------------------------------------------------------------------##
# 2024 campaign dates by state -------------------------------------------------
## ---------------------------------------------------------------------------##

calendar <- .table_lines(ipccs, "Table 1.1:") |>
  .report_fields(
    "^\\s*[0-9]+\\.\\s+(.+?)\\s{2,}(.+[0-9]{4})\\s*$",
    c("adm1_raw", "dates")
  ) |>
  dplyr::filter(!is.na(.data$matched))

campaign_dates <- .report_fields(
  calendar$dates,
  paste0(
    "([0-9]+)[a-z]{2}\\s*([A-Za-z]{3})?\\s+to\\s+([0-9]+)[a-z]{2}\\s+",
    "([A-Za-z]{3})\\s+([0-9]{4})"
  ),
  c("start_day", "start_month", "end_day", "end_month", "year")
)

sia_calendar <- campaign_dates |>
  dplyr::mutate(adm1_raw = stringr::str_squish(calendar$adm1_raw)) |>
  dplyr::transmute(
    campaign_id = campaign_ipccs,
    .data$adm1_raw,
    start_date = as.Date(
      paste(
        .data$start_day,
        dplyr::coalesce(.data$start_month, .data$end_month), .data$year
      ),
      format = "%d %b %Y"
    ),
    end_date = as.Date(
      paste(.data$end_day, .data$end_month, .data$year), format = "%d %b %Y"
    ),
    target_age = "9-59 months",
    source = "IPCCS 2024 report, Table 1.1",
    note = NA_character_
  )

sia_calendar <- dplyr::bind_rows(pmccs_calendar, sia_calendar)

## ---------------------------------------------------------------------------##
# 2024 measles surveillance by state -------------------------------------------
## ---------------------------------------------------------------------------##

# cumulative from January to the end of October 2024
measles_cases <- .table_lines(ncdc, "Table 1:") |>
  .parse_state_rows(
    c(
      "suspected", "confirmed", "lab_confirmed", "epi_linked",
      "clinically_compatible", "confirmed_9_59m_pct", "confirmed_zero_dose_pct"
    )
  ) |>
  dplyr::select(-ci_lower, -ci_upper) |>
  dplyr::mutate(period = "2024-01/2024-10", .before = 1)

## ---------------------------------------------------------------------------##
# State names ------------------------------------------------------------------
## ---------------------------------------------------------------------------##

adm1_lookup <- shp |>
  sf::st_drop_geometry() |>
  dplyr::distinct(adm0, adm1, adm1_code)

# match report spellings to the boundaries without prompting; names that
# cannot be matched are flagged by prepare_campaigns() as unmatched
adm1_names <- dplyr::bind_rows(
  pmccs_survey, ipccs_survey, sia_calendar, measles_cases
) |>
  dplyr::distinct(adm1_raw) |>
  dplyr::mutate(adm0 = unique(adm1_lookup$adm0), adm1 = adm1_raw)

dir.create(dirname(name_cache), recursive = TRUE, showWarnings = FALSE)

adm1_names <- sntutils::prep_geonames(
  adm1_names,
  lookup_df = dplyr::distinct(adm1_lookup, adm0, adm1),
  level0 = "adm0",
  level1 = "adm1",
  cache_path = name_cache,
  preserve_case = TRUE,
  interactive = FALSE
) |>
  dplyr::left_join(adm1_lookup, by = c("adm0", "adm1"))

.add_adm1 <- function(df) {
  df |>
    dplyr::left_join(adm1_names, by = "adm1_raw") |>
    dplyr::relocate(adm0, adm1, adm1_code, .after = adm1_raw) |>
    dplyr::select(-adm1_raw)
}

sia_survey <- dplyr::bind_rows(pmccs_survey, ipccs_survey) |>
  .add_adm1()

sia_calendar <- .add_adm1(sia_calendar)

measles_cases <- .add_adm1(measles_cases)

## ---------------------------------------------------------------------------##
# Campaign records -------------------------------------------------------------
## ---------------------------------------------------------------------------##

# one record per state and campaign, survey indicators kept separately, and
# a list of problems to resolve (missing 2017-2018 dates among them)
campaigns <- mipR::prepare_campaigns(
  calendar = sia_calendar,
  evaluation = sia_survey,
  geography = c("adm0", "adm1", "adm1_code"),
  boundaries = shp
)

campaigns$diagnostics |>
  dplyr::count(check, campaign_id)

## ---------------------------------------------------------------------------##
# Save -------------------------------------------------------------------------
## ---------------------------------------------------------------------------##

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(sia_survey, file.path(processed_dir, "nga_sia_survey_adm1.rds"))

saveRDS(sia_calendar, file.path(processed_dir, "nga_sia_calendar_adm1.rds"))

saveRDS(
  measles_cases,
  file.path(processed_dir, "nga_measles_cases_adm1_2024.rds")
)

saveRDS(campaigns, file.path(processed_dir, "nga_campaigns_adm1.rds"))
