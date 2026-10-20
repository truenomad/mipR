# prepare_campaigns ------------------------------------------------------------

# a small Nigeria-shaped example: the 2017-2018 campaign has coverage but no
# verified dates, and the 2024 campaign has dates, coverage and indicators
nga_calendar <- data.frame(
  adm0 = "NIGERIA",
  adm1 = c("KANO", "LAGOS", "KANO", "LAGOS", "BORNO"),
  campaign_id = c(
    "NGA-MSL-2017-2018", "NGA-MSL-2017-2018",
    "NGA-MSL-2024", "NGA-MSL-2024", "NGA-MSL-2024"
  ),
  start_date = as.Date(c(NA, NA, "2024-11-25", "2024-10-26", "2024-10-12")),
  end_date = as.Date(c(NA, NA, "2024-12-03", "2024-11-03", "2024-10-20")),
  target_age = "9-59 months",
  source = c("PMCCS 2018 report", "PMCCS 2018 report", rep("IPCCS 2024", 3)),
  note = c(rep("dates not stated by state in the report", 2), rep(NA, 3))
)

nga_evaluation <- data.frame(
  adm0 = "NIGERIA",
  adm1 = c("KANO", "LAGOS", "KANO", "LAGOS", "KANO", "KANO", "KANO"),
  campaign_id = c(
    "NGA-MSL-2017-2018", "NGA-MSL-2017-2018", "NGA-MSL-2024", "NGA-MSL-2024",
    "NGA-MSL-2024", "NGA-MSL-2024", "NGA-MSL-2024"
  ),
  survey = c("PMCCS", "PMCCS", rep("IPCCS", 5)),
  indicator = c(
    "sia_coverage", "sia_coverage", "sia_coverage", "sia_coverage",
    "sia_first_dose", "mcv_before_sia", "mcv_0_dose"
  ),
  value_pct = c(85.2, 70.1, 88.1, 79.4, 12.3, 61.0, 9.5),
  lower_pct = c(80.0, 64.2, 84.0, 74.9, 9.1, 55.2, 6.8),
  upper_pct = c(89.5, 75.4, 91.3, 83.4, 16.4, 66.5, 13.1),
  n = c(400, 380, 410, 395, 410, 410, 410)
)

nga_reported <- data.frame(
  adm0 = "NIGERIA",
  adm1 = "BORNO",
  campaign_id = "NGA-MSL-2024",
  coverage_pct = 104.2,
  source = "campaign tally sheets"
)

nga_boundaries <- data.frame(
  adm0 = "NIGERIA",
  adm1 = c("KANO", "LAGOS", "BORNO")
)

geo <- c("adm0", "adm1")

testthat::test_that("prepare_campaigns builds one record per area", {
  out <- prepare_campaigns(
    nga_calendar, nga_evaluation, nga_reported,
    geography = geo, boundaries = nga_boundaries
  )

  testthat::expect_named(
    out, c("campaigns", "evaluation", "diagnostics", "metadata")
  )
  campaigns <- out$campaigns
  testthat::expect_identical(nrow(campaigns), 5L)
  testthat::expect_identical(unique(campaigns$age_min_months), 9)
  testthat::expect_identical(unique(campaigns$age_max_months), 60)

  kano24 <- campaigns[
    campaigns$adm1 == "KANO" & campaigns$campaign_id == "NGA-MSL-2024",
  ]
  testthat::expect_equal(kano24$coverage, 0.881)
  testthat::expect_equal(kano24$coverage_lower, 0.84)
  testthat::expect_identical(kano24$coverage_source, "survey")
  testthat::expect_identical(kano24$coverage_reference, "IPCCS")
  testthat::expect_false(kano24$needs_dates)
})

testthat::test_that("missing dates are kept and flagged, never inferred", {
  out <- prepare_campaigns(nga_calendar, nga_evaluation, geography = geo)

  pmccs <- out$campaigns[out$campaigns$campaign_id == "NGA-MSL-2017-2018", ]
  testthat::expect_true(all(is.na(pmccs$start_date)))
  testthat::expect_true(all(pmccs$needs_dates))
  testthat::expect_identical(
    unique(pmccs$note), "dates not stated by state in the report"
  )
  flagged <- out$diagnostics[out$diagnostics$check == "missing_dates", ]
  testthat::expect_identical(sort(flagged$adm1), c("KANO", "LAGOS"))
})

testthat::test_that("reported coverage is a labelled fallback, never imputed", {
  without <- prepare_campaigns(nga_calendar, nga_evaluation, geography = geo)
  borno <- without$campaigns[without$campaigns$adm1 == "BORNO", ]
  testthat::expect_true(is.na(borno$coverage))
  testthat::expect_true(borno$needs_coverage)
  testthat::expect_true(
    "missing_coverage" %in% without$diagnostics$check
  )

  with <- prepare_campaigns(
    nga_calendar, nga_evaluation, nga_reported, geography = geo
  )
  borno <- with$campaigns[with$campaigns$adm1 == "BORNO", ]
  testthat::expect_equal(borno$coverage, 1.042)
  testthat::expect_identical(
    borno$coverage_source, "coverage reported by the campaign"
  )
  testthat::expect_identical(borno$coverage_reference, "campaign tally sheets")
  testthat::expect_true(
    "coverage_out_of_range" %in% with$diagnostics$check
  )
})

testthat::test_that("evaluation keeps every survey indicator as proportions", {
  out <- prepare_campaigns(nga_calendar, nga_evaluation, geography = geo)
  ev <- out$evaluation

  testthat::expect_named(
    ev,
    c(
      "adm0", "adm1", "campaign_id", "survey", "indicator", "value",
      "lower", "upper", "n"
    )
  )
  testthat::expect_setequal(
    unique(ev$indicator),
    c("sia_coverage", "sia_first_dose", "mcv_before_sia", "mcv_0_dose")
  )
  first_dose <- ev[ev$indicator == "sia_first_dose", ]
  testthat::expect_equal(first_dose$value, 0.123)
})

testthat::test_that(".parse_target_age reads exclusive month bounds", {
  ages <- .parse_target_age(c(
    "9-59 months", "9 months-14 years", "6 months to 14 years",
    "9 and 59 months", "1-4 years", "9 \u2013 59 months", "school children",
    NA
  ))

  testthat::expect_identical(ages$min, c(9, 9, 6, 9, 12, 9, NA, NA))
  testthat::expect_identical(ages$max, c(60, 180, 180, 60, 60, 60, NA, NA))
})

testthat::test_that("prepare_campaigns flags invalid and inconsistent input", {
  calendar <- nga_calendar
  calendar$target_age[3] <- "school children"
  calendar$end_date[4] <- as.Date("2024-10-01")
  calendar <- rbind(
    calendar,
    calendar[5, ],
    transform(
      calendar[5, ],
      campaign_id = "NGA-MR-2024", adm1 = "BORNO",
      start_date = as.Date("2024-10-15"), end_date = as.Date("2024-10-25")
    ),
    transform(calendar[5, ], adm1 = "ATLANTIS", campaign_id = "NGA-MSL-2024")
  )

  out <- prepare_campaigns(
    calendar, nga_evaluation,
    geography = geo, boundaries = nga_boundaries
  )
  checks <- out$diagnostics$check

  testthat::expect_true("invalid_age" %in% checks)
  testthat::expect_true("end_before_start" %in% checks)
  testthat::expect_true("duplicate_record" %in% checks)
  testthat::expect_true("overlapping_campaigns" %in% checks)
  testthat::expect_true("unmatched_geography" %in% checks)
  unmatched <- out$diagnostics[checks == "unmatched_geography", ]
  testthat::expect_identical(unique(unmatched$adm1), "ATLANTIS")
})

testthat::test_that("prepare_campaigns accepts month bounds directly", {
  calendar <- data.frame(
    adm1 = "KANO", campaign_id = "X",
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-05"),
    age_min_months = 6, age_max_months = 180
  )
  out <- prepare_campaigns(calendar, geography = "adm1")

  testthat::expect_identical(out$campaigns$age_min_months, 6)
  testthat::expect_identical(out$campaigns$age_max_months, 180)
  testthat::expect_error(
    prepare_campaigns(calendar[, -2], geography = "adm1"),
    "campaign_id"
  )
})
