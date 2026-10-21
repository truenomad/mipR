# summaries --------------------------------------------------------------------

#' Summarise Immunity by Geography, Date and Age Band
#'
#' Adds up the cohort results of [estimate_immunity()] into susceptible and
#' immune population by geography, month and age band.
#'
#' @details
#' Shares are weighted by population, so a summary share is the share of
#' all children in the group. Infants marked as protected by maternal
#' antibodies are left out by default. A group with no population has no
#' defined share: its share columns are `NA` rather than a number.
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
  if (!is.numeric(age_breaks) || length(age_breaks) == 0 ||
        any(!is.finite(age_breaks)) ||
        is.unsorted(age_breaks, strictly = TRUE) ||
        !is.numeric(max_age_months) || length(max_age_months) != 1 ||
        !is.finite(max_age_months) || max_age_months <= max(age_breaks)) {
    cli::cli_abort(
      "{.arg age_breaks} must increase and end below {.arg max_age_months}."
    )
  }
  if (!is.null(at)) {
    months <- .month_from_index(.month_index(.as_date(at, "at")))
    results <- dplyr::filter(results, .data$month %in% months)
  }
  if (exclude_maternal) {
    results <- dplyr::filter(results, !.data$maternal_protection)
  }

  band_labels <- .age_band_labels(age_breaks, max_age_months)
  share_cols <- c("dose_0", "dose_1", "dose_2", "dose_3_plus")
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
        dplyr::all_of(share_cols), \(x) sum(x * .data$population)
      ),
      susceptible = sum(.data$susceptible),
      population = sum(.data$population),
      .by = dplyr::all_of(c(geography, "month", "age_band"))
    ) |>
    dplyr::mutate(
      susceptible_share = dplyr::if_else(
        .data$population > 0, .data$susceptible / .data$population, NA_real_
      ),
      immune_share = 1 - .data$susceptible_share,
      dplyr::across(
        dplyr::all_of(share_cols),
        \(x) dplyr::if_else(
          .data$population > 0, x / .data$population, NA_real_
        )
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(geography, "month", "age_band")),
      "population", "susceptible", "susceptible_share", "immune_share",
      dplyr::all_of(share_cols)
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
  upper <- dplyr::lead(age_breaks, default = max_age_months)
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
#' share of children whose first dose came from the campaign.
#'
#' @details
#' **Denominator.** For each campaign and geography, the comparison covers
#' the campaign's target cohorts: the birth months with children in the
#' target ages at some time during the campaign, taken from the engine's
#' `campaign_targets`. Every indicator uses the same denominator, each
#' cohort's population in the month after the campaign weighted by its
#' eligible share, so the indicators of one campaign are comparable with
#' each other. Dose shares are those of the eligible children themselves,
#' from the exact birth-day states, so children of a partly targeted cohort
#' who were outside the target ages do not enter the numerator or the
#' denominator.
#'
#' **Indicators.**
#' - `mcv_0_dose`, `mcv_1_dose` and `mcv_2plus_dose`: shares with no, one,
#'   and two or more doses on the 15th of the month after the campaign ends.
#' - `mcv_before_sia`: share with at least one dose on the 15th of the
#'   month before the campaign starts.
#' - `sia_first_dose`: share of the target children whose first ever dose
#'   was given by this campaign, from the campaign's own contribution in
#'   `opportunities`. Routine doses and other campaigns in the same months
#'   do not count. The matching survey indicator is the share of surveyed
#'   target children reporting their first measles dose in the campaign;
#'   both are shares of all target children, not of the unvaccinated.
#'
#' A campaign whose after month has no population in `immunity` is listed
#' in `diagnostics` and left out, as is one with no target cohorts; no value
#' is filled in. Differences between model and survey point to the coverage
#' inputs and the dose-overlap rule (`coupling`). They do not identify
#' vaccine effectiveness: dose counts do not depend on it, and the survey
#' indicators compared here are about doses, not protection.
#'
#' @param immunity The output of [estimate_immunity()].
#' @param campaigns The output of [prepare_campaigns()], with its
#'   `campaigns` and `evaluation` tables.
#'
#' @return A list:
#'   \describe{
#'     \item{comparison}{One row per geography, campaign, survey and
#'       indicator: `survey_value`, `survey_lower`, `survey_upper`,
#'       `model_value`, `denominator` (the weighted population behind the
#'       model value), `difference` and `within_interval`}
#'     \item{summary}{One row per campaign and indicator: number of areas,
#'       mean difference, mean absolute difference and share within the
#'       survey interval}
#'     \item{diagnostics}{Campaigns and geographies that could not be
#'       compared, with the reason}
#'   }
#'
#' @examples
#' \dontrun{
#' checks <- check_immunity(immunity, campaigns)
#' checks$summary
#' }
#' @export
check_immunity <- function(immunity, campaigns) {
  if (!is.list(immunity) ||
        !all(c("immunity", "campaign_targets") %in% names(immunity))) {
    cli::cli_abort(
      "{.arg immunity} must be the output of {.fn estimate_immunity}."
    )
  }
  results <- .immunity_input(immunity)
  targets <- immunity$campaign_targets
  if (!is.list(campaigns) ||
        !all(c("campaigns", "evaluation") %in% names(campaigns)) ||
        !is.data.frame(campaigns$evaluation)) {
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
  cohort_geography <- setdiff(
    names(targets),
    c(
      "birth_month", "event_id", "eligible_share", "before", "after",
      "dose_0_before", "dose_1_before", "dose_2_before",
      "dose_3_plus_before", "dose_0_after", "dose_1_after", "dose_2_after",
      "dose_3_plus_after", "first_doses"
    )
  )
  geography <- intersect(
    setdiff(names(evaluation), c(
      "campaign_id", "survey", "indicator", "value", "lower", "upper", "n"
    )),
    cohort_geography
  )
  if (nrow(evaluation) == 0 || length(geography) == 0) {
    cli::cli_abort(
      "No survey indicators to compare, or no shared geography columns."
    )
  }

  windows <- campaigns$campaigns |>
    dplyr::filter(!.data$needs_dates, !is.na(.data$age_min_months)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c(geography, "campaign_id")
    )))

  # the target children of each campaign and geography, with their exact
  # states before and after the campaign, weighted by the cohort population
  # after the campaign and the share of the cohort the campaign can reach
  cohort_keys <- c(cohort_geography, "birth_month")
  with_population <- targets |>
    dplyr::rename(campaign_id = "event_id") |>
    dplyr::inner_join(windows, by = c(geography, "campaign_id")) |>
    dplyr::left_join(
      dplyr::select(
        results, dplyr::all_of(cohort_keys), "month", "population"
      ),
      by = c(cohort_keys, after = "month")
    )
  complete <- with_population |>
    dplyr::summarise(
      missing_after = any(is.na(.data$population)),
      .by = dplyr::all_of(c(geography, "campaign_id"))
    )
  diagnostics <- dplyr::bind_rows(
    windows |>
      dplyr::anti_join(complete, by = c(geography, "campaign_id")) |>
      dplyr::mutate(
        check = "no_target_cohorts",
        detail = "no cohort in the target ages during the campaign"
      ),
    complete |>
      dplyr::filter(.data$missing_after) |>
      dplyr::mutate(
        check = "missing_window",
        detail = "immunity has no population in the month after the campaign"
      )
  ) |>
    dplyr::select(
      "check", dplyr::all_of(c(geography, "campaign_id")), "detail"
    )
  usable <- complete |>
    dplyr::filter(!.data$missing_after) |>
    dplyr::select(dplyr::all_of(c(geography, "campaign_id")))

  model <- with_population |>
    dplyr::semi_join(usable, by = c(geography, "campaign_id")) |>
    dplyr::mutate(weight = .data$population * .data$eligible_share) |>
    dplyr::summarise(
      denominator = sum(.data$weight),
      mcv_0_dose = sum(.data$dose_0_after * .data$weight) /
        sum(.data$weight),
      mcv_1_dose = sum(.data$dose_1_after * .data$weight) /
        sum(.data$weight),
      mcv_2plus_dose = sum(
        (.data$dose_2_after + .data$dose_3_plus_after) * .data$weight
      ) / sum(.data$weight),
      mcv_before_sia = 1 -
        sum(.data$dose_0_before * .data$weight) / sum(.data$weight),
      sia_first_dose = sum(.data$first_doses * .data$population) /
        sum(.data$weight),
      .by = dplyr::all_of(c(geography, "campaign_id"))
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
  if (nrow(comparison) == 0) {
    cli::cli_abort(c(
      "No survey indicator could be compared with the model.",
      "i" = "See the campaigns' dates, target ages and the months covered."
    ))
  }

  summary <- comparison |>
    dplyr::summarise(
      areas = dplyr::n(),
      mean_difference = mean(.data$difference),
      mean_absolute_difference = mean(abs(.data$difference)),
      share_within_interval = mean(.data$within_interval, na.rm = TRUE),
      .by = c("campaign_id", "indicator")
    )

  list(comparison = comparison, summary = summary, diagnostics = diagnostics)
}
