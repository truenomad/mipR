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
  "age_at_dose", "cell", "coupling", "coverage", "difference", "doses",
  "eligible_share", "filled", "fraction", "month", "pending", "population",
  "protected", "reach", "share", "susceptible", "susceptible_share",
  "tier_share", "timing_share", "type", "type_order", "value",
  "within_interval"
))

#' @importFrom dplyr .data
NULL
