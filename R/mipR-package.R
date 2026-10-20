#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# columns referenced through non-standard evaluation
utils::globalVariables(c(
  "ENDDATE", "STARTDATE", "ISO_3_CODE", "ADM0_NAME", "ADM1_NAME",
  "ADM2_NAME", "lower", "upper",
  # used inside dplyr::join_by()
  "closest"
))

#' @importFrom dplyr .data
NULL
