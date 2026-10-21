#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# columns referenced through non-standard evaluation
utils::globalVariables(c(
  ".env",
  "ENDDATE", "STARTDATE", "ISO_3_CODE", "ADM0_NAME", "ADM1_NAME",
  "ADM2_NAME", "lower", "upper",
  # used inside dplyr::join_by()
  "closest",
  # bare names in grouped calls of the immunity engine, which are much
  # faster than the .data pronoun
  "after", "age_at_dose", "age_max_months", "age_min_months", "band",
  "before", "cell", "coupling", "coverage", "coverage_year", "date_from",
  "date_to", "day", "days_in_month", "difference", "dose_0", "dose_0_after",
  "dose_0_before", "dose_1_after", "dose_2_after", "dose_3_plus_after",
  "doses", "eligible_share", "end_date", "event_key", "filled", "first_doses",
  "fraction", "gap", "k", "mass", "month", "n", "newly_protected", "pending",
  "population", "protected", "reach", "report_month", "share", "start_date",
  "susceptible", "susceptible_share", "tier_share", "timing_share", "type",
  "type_order", "usable", "value", "weight", "within_interval"
))

#' @importFrom dplyr .data
NULL
