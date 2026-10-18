# integration tests download real data; run them with MIPR_INTEGRATION=true
skip_if_not_integration <- function(message = "Integration test skipped.") {
  testthat::skip_if_not(
    identical(Sys.getenv("MIPR_INTEGRATION"), "true"),
    message
  )
}
