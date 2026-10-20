# summaries --------------------------------------------------------------------

#' Summarise Immunity by Geography, Date and Age Band
#'
#' Adds up the cohort results of [estimate_immunity()] into susceptible and
#' immune population by geography, month and age band.
#'
#' @details
#' Shares are weighted by population, so a summary share is the share of
#' all children in the group. Infants marked as protected by maternal
#' antibodies are left out by default.
#'
#' @param immunity The output of [estimate_immunity()], or its `immunity`
#'   data frame.
#' @param geography Columns to summarise by. Default: every geography column
#'   in `immunity`. Use fewer columns, such as `"adm0"`, for a higher level.
#' @param at Months to summarise, as Dates or `"YYYY-MM"`. Default: every
#'   month.
#' @param age_breaks Lower bounds of the age bands in months; the last band
#'   ends at `max_age_months`. Default: under 1 year, 1-4, 5-9 and 10-14
#'   years.
#' @param max_age_months Upper bound of the last band (exclusive). Default
#'   `180`.
#' @param exclude_maternal Logical. Leave out infants marked as protected by
#'   maternal antibodies. Default `TRUE`.
#'
#' @return A data frame with the `geography` columns, `month`, `age_band`,
#'   `population`, `susceptible`, `susceptible_share`, `immune_share`, and
#'   `dose_0`, `dose_1`, `dose_2` and `dose_3_plus` (shares).
#'
#' @examples
#' \dontrun{
#' summary <- summarise_immunity(
#'   immunity, geography = c("adm0", "adm1"), at = "2025-06"
#' )
#' }
#' @export
summarise_immunity <- function(
  immunity,
  geography = NULL,
  at = NULL,
  age_breaks = c(0, 12, 60, 120),
  max_age_months = 180,
  exclude_maternal = TRUE
) {
  results <- .immunity_input(immunity)
  if (is.null(geography)) {
    geography <- setdiff(
      names(results),
      c(
        "birth_month", "month", "age_months", "population", "basis",
        "susceptible_share", "dose_0", "dose_1", "dose_2", "dose_3_plus",
        "susceptible", "maternal_protection"
      )
    )
  }
  missing_cols <- setdiff(geography, names(results))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} {.val {missing_cols}} not in {.arg immunity}.")
  }
  if (!is.null(at)) {
    months <- .month_from_index(.month_index(.as_date(at, "at")))
    results <- dplyr::filter(results, .data$month %in% months)
  }
  if (exclude_maternal) {
    results <- dplyr::filter(results, !.data$maternal_protection)
  }

  band_labels <- .age_band_labels(age_breaks, max_age_months)
  results |>
    dplyr::filter(.data$age_months < max_age_months) |>
    dplyr::mutate(
      age_band = cut(
        .data$age_months,
        breaks = c(age_breaks, max_age_months),
        labels = band_labels, right = FALSE
      )
    ) |>
    dplyr::filter(!is.na(.data$age_band)) |>
    dplyr::summarise(
      dplyr::across(
        c("dose_0", "dose_1", "dose_2", "dose_3_plus"),
        ~ sum(.x * .data$population) / sum(.data$population)
      ),
      susceptible = sum(.data$susceptible),
      population = sum(.data$population),
      .by = dplyr::all_of(c(geography, "month", "age_band"))
    ) |>
    dplyr::mutate(
      susceptible_share = .data$susceptible / .data$population,
      immune_share = 1 - .data$susceptible_share
    ) |>
    dplyr::relocate(
      "population", "susceptible", "susceptible_share", "immune_share",
      .after = "age_band"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(geography, "month", "age_band")
    )))
}

# internal helper: the immunity data frame from estimate_immunity()
#' @noRd
.immunity_input <- function(immunity) {
  if (is.list(immunity) && !is.data.frame(immunity)) {
    immunity <- immunity$immunity
  }
  needed <- c("month", "age_months", "population", "susceptible",
              "maternal_protection")
  if (!is.data.frame(immunity) || !all(needed %in% names(immunity))) {
    cli::cli_abort(
      "{.arg immunity} must be the output of {.fn estimate_immunity}."
    )
  }
  immunity
}

# internal helper: labels for age bands given in months
#' @noRd
.age_band_labels <- function(age_breaks, max_age_months) {
  upper <- c(age_breaks[-1], max_age_months)
  purrr::map2_chr(age_breaks, upper, \(lo, hi) {
    in_years <- lo %% 12 == 0 && hi %% 12 == 0 && hi > 12
    if (in_years) {
      paste0(lo / 12, "-", hi / 12 - 1, " years")
    } else {
      paste0(lo, "-", hi - 1, " months")
    }
  })
}

# checks -----------------------------------------------------------------------

#' Check Immunity Estimates Against Survey Evidence
#'
#' Compares the vaccination histories estimated by [estimate_immunity()]
#' with post-campaign survey results kept by [prepare_campaigns()]: the
#' distribution of lifetime doses, vaccination before the campaign, and the
#' share of children who received their first dose in the campaign.
#'
#' @details
#' For each survey indicator, the model value is taken over the campaign's
#' target ages and geography, weighted by population:
#' - `mcv_0_dose`, `mcv_1_dose` and `mcv_2plus_dose`: shares with no, one,
#'   and two or more doses in the month after the campaign ends.
#' - `mcv_before_sia`: share with at least one dose in the month before the
#'   campaign starts.
#' - `sia_first_dose`: share with no dose before the campaign that has one
#'   after it.
#'
#' Large differences point to the dose-overlap rule (`coupling`), coverage
#' inputs or vaccine effectiveness, and are the basis for choosing between
#' them.
#'
#' @param immunity The output of [estimate_immunity()].
#' @param campaigns The output of [prepare_campaigns()], with its
#'   `campaigns` and `evaluation` tables.
#'
#' @return A list:
#'   \describe{
#'     \item{comparison}{One row per geography, campaign, survey and
#'       indicator: `survey_value`, `survey_lower`, `survey_upper`,
#'       `model_value`, `difference` and `within_interval`}
#'     \item{summary}{One row per campaign and indicator: number of areas,
#'       mean difference, mean absolute difference and share within the
#'       survey interval}
#'   }
#'
#' @examples
#' \dontrun{
#' checks <- check_immunity(immunity, campaigns)
#' checks$summary
#' }
#' @export
check_immunity <- function(immunity, campaigns) {
  results <- .immunity_input(immunity)
  if (!is.list(campaigns) ||
        !all(c("campaigns", "evaluation") %in% names(campaigns))) {
    cli::cli_abort(
      "{.arg campaigns} must be the output of {.fn prepare_campaigns}."
    )
  }
  indicators <- c(
    "mcv_0_dose", "mcv_1_dose", "mcv_2plus_dose", "mcv_before_sia",
    "sia_first_dose"
  )
  evaluation <- campaigns$evaluation |>
    dplyr::filter(.data$indicator %in% indicators)
  geography <- intersect(
    setdiff(names(evaluation), c(
      "campaign_id", "survey", "indicator", "value", "lower", "upper", "n"
    )),
    names(results)
  )
  if (nrow(evaluation) == 0 || length(geography) == 0) {
    cli::cli_abort(
      "No survey indicators to compare, or no shared geography columns."
    )
  }

  windows <- campaigns$campaigns |>
    dplyr::filter(!.data$needs_dates, !is.na(.data$age_min_months)) |>
    dplyr::mutate(
      before = .month_from_index(.month_index(.data$start_date) - 1L),
      after = .month_from_index(.month_index(.data$end_date) + 1L)
    ) |>
    dplyr::select(
      dplyr::all_of(geography), "campaign_id", "age_min_months",
      "age_max_months", "before", "after"
    )

  # model dose shares over each campaign's target ages, before and after
  model <- windows |>
    dplyr::inner_join(
      results, by = geography, relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$month %in% c(.data$before, .data$after),
      .data$age_months >= .data$age_min_months,
      .data$age_months < .data$age_max_months
    ) |>
    dplyr::mutate(
      period = dplyr::if_else(.data$month == .data$before, "before", "after")
    ) |>
    dplyr::summarise(
      dplyr::across(
        c("dose_0", "dose_1", "dose_2", "dose_3_plus"),
        ~ sum(.x * .data$population) / sum(.data$population)
      ),
      .by = dplyr::all_of(c(geography, "campaign_id", "period"))
    ) |>
    tidyr::pivot_wider(
      names_from = "period",
      values_from = c("dose_0", "dose_1", "dose_2", "dose_3_plus")
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(c(geography, "campaign_id"))),
      mcv_0_dose = .data$dose_0_after,
      mcv_1_dose = .data$dose_1_after,
      mcv_2plus_dose = .data$dose_2_after + .data$dose_3_plus_after,
      mcv_before_sia = 1 - .data$dose_0_before,
      sia_first_dose = .data$dose_0_before - .data$dose_0_after
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(indicators),
      names_to = "indicator", values_to = "model_value"
    )

  comparison <- evaluation |>
    dplyr::select(
      dplyr::all_of(geography), "campaign_id", "survey", "indicator",
      survey_value = "value", survey_lower = "lower", survey_upper = "upper"
    ) |>
    dplyr::inner_join(
      model, by = c(geography, "campaign_id", "indicator")
    ) |>
    dplyr::mutate(
      difference = .data$model_value - .data$survey_value,
      within_interval = .data$model_value >= .data$survey_lower &
        .data$model_value <= .data$survey_upper
    )

  summary <- comparison |>
    dplyr::summarise(
      areas = dplyr::n(),
      mean_difference = mean(.data$difference),
      mean_absolute_difference = mean(abs(.data$difference)),
      share_within_interval = mean(.data$within_interval, na.rm = TRUE),
      .by = c("campaign_id", "indicator")
    )

  list(comparison = comparison, summary = summary)
}
