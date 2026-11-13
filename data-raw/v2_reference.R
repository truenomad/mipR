# synthetic inputs through the measles-campaign engine (v2_run rules), saved
# as tests/testthat/fixtures/v2_reference.rds; no real data are used

# MIPR_V2_PIPELINE: the pipeline 02_scripts folder, whose files are only read

scripts <- Sys.getenv("MIPR_V2_PIPELINE")
if (!nzchar(scripts)) {
  stop("Set MIPR_V2_PIPELINE to the measles-campaign 02_scripts folder.")
}

# takes only the named definitions from a pipeline file, running nothing else
take_functions <- function(path, names, env) {
  exprs <- parse(path, keep.source = FALSE)
  purrr::walk(exprs, \(e) {
    is_assign <- is.call(e) && identical(e[[1]], as.name("<-"))
    if (is_assign && as.character(e[[2]]) %in% names) {
      eval(e, env)
    }
  })
  invisible(env)
}

v2 <- new.env()
take_functions(
  file.path(scripts, "00_helper_functions", "0c_cohort_profile.R"),
  c(
    "build_birth_cohorts", "advance_birth_cohorts",
    "assign_routine_vaccination", "assign_campaign_vaccination",
    "validate_coverage", "calculate_vaccine_protection", "fill_tiers"
  ),
  v2
)
take_functions(
  file.path(scripts, "2h_models", "2hi_infection_inputs", "03_engine.R"),
  c(
    "ii_susceptible", "ii_vaccine", "ii_infection", "ii_new_state",
    "ii_apply_vaccination", "ii_apply_infection", "ii_summarise", "ii_run"
  ),
  v2
)

# synthetic inputs -------------------------------------------------------------

units <- c("u1", "u2")
birth_months <- seq(as.Date("2014-01-01"), as.Date("2020-12-01"), "month")
months <- seq(as.Date("2016-01-01"), as.Date("2021-12-01"), "month")

cohorts <- v2$build_birth_cohorts(units, birth_months, offset_days = 14) |>
  dplyr::mutate(group = .data$unit_id, size = 100)

routine <- tidyr::expand_grid(
  unit_id = units, year = 2014:2022, dose = c("MCV1", "MCV2")
) |>
  dplyr::mutate(
    coverage = 0.45 + 0.03 * (.data$year - 2014) +
      0.1 * (.data$dose == "MCV1") + 0.05 * (.data$unit_id == "u2")
  )

schedule <- tibble::tribble(
  ~dose, ~start_date, ~end_date, ~scheduled_age_months,
  "MCV1", as.Date("1990-01-01"), as.Date("2099-12-31"), 9,
  "MCV2", as.Date("2018-03-01"), as.Date("2099-12-31"), 18
)

# same-day campaigns whose ids sort against row order, repeated rounds, and
# campaigns dated on the 1st test each convention
campaigns <- tibble::tribble(
  ~unit_id, ~event_id, ~date, ~age_lo, ~age_hi, ~coverage, ~coupling,
  NA, "nat_2017", "2017-04-20", 9, 60, 0.7, NA,
  "u1", "zz_mr", "2018-11-07", 6, 120, 0.6, "independent",
  "u1", "aa_mr", "2018-11-07", 9, 60, 0.5, "complementary",
  "u2", "ori", "2019-05-01", 0, 24, 0.3, NA,
  "u2", "ori", "2019-05-01", 0, 24, 0.25, NA,
  "u1", "maac_r1", "2020-03-01", 9, 36, 0.8, "complementary",
  NA, "nat_2021", "2021-02-15", 6, 180, 0.65, NA,
  "u2", "late_1st", "2021-06-01", 12, 60, 0.4, "dependent"
) |>
  dplyr::mutate(date = as.Date(.data$date))

ve <- list(
  mode = "by_type",
  mcv1 = list(breaks = c(0, 9, 12, Inf), values = c(0.58, 0.84, 0.925)),
  mcv2 = list(breaks = c(0, Inf), values = 0.95),
  sia = list(breaks = c(0, 9, 12, Inf), values = c(0.58, 0.84, 0.95))
)

# the pipeline run -------------------------------------------------------------

events <- dplyr::bind_rows(
  v2$assign_routine_vaccination(cohorts, routine, schedule),
  v2$assign_campaign_vaccination(cohorts, campaigns)
)
run <- v2$ii_run(
  cohorts, events, "dependent", ve,
  snapshots = list(end = max(months)), monthly = months, max_doses = 8L
)

reference <- run$monthly$susceptible |>
  tibble::as_tibble(.name_repair = "minimal") |>
  stats::setNames(format(months)) |>
  dplyr::mutate(
    unit_id = cohorts$unit_id, birth_month = cohorts$birth_month
  ) |>
  tidyr::pivot_longer(
    -c("unit_id", "birth_month"),
    names_to = "month", values_to = "susceptible"
  ) |>
  dplyr::mutate(month = as.Date(.data$month))

saveRDS(
  list(
    cohorts = dplyr::select(cohorts, "unit_id", "birth_month", "size"),
    months = months,
    routine = routine,
    schedule = schedule,
    campaigns = campaigns,
    reference = reference
  ),
  file.path("tests", "testthat", "fixtures", "v2_reference.rds"),
  version = 2
)
