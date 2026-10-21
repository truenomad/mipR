# mipR 0.0.1

* Vaccination histories retain birth-day eligibility and dose dates.
  Monthly immunity uses only doses received by the 15th, with campaign
  doses distributed across eligible dates and effectiveness evaluated at
  the age on each date. Equivalent schedule partitions give the same
  result; missing routine-coverage years use the nearest available year.
* Empty vaccination histories remain fully susceptible. Added validation
  for coverage bounds, schedule ages, effectiveness and draw settings.
  Coverage draws preserve the caller's random-number state, share campaign
  draws across affected cohorts and use elapsed years for correlation.
* Survey comparisons use campaign-specific windows and eligible children.
  First doses are attributed to the campaign's own transitions. Missing
  comparison windows are diagnosed, and survey denominators are explicit.
* Fixed cohort extension with zero birth-seasonality weights, retaining
  exact zero births in those months. Sparse mortality and birth-rate
  years use the nearest available year, with earlier years breaking ties.
* Constrained reconstruction now rejects zero fitting tolerance, duplicate
  anchors and invalid demographic inputs instead of returning misleading
  populations. The fitting and infant-survival assumptions are explicit.
* Fixed default map bins for susceptibility below 20%, including all-zero
  maps, and added checks for invalid shares, breaks and unmatched areas.
* Population weights are summed directly onto a target grid when its
  coordinate reference system differs, avoiding bilinear interpolation
  of population counts.
* Annual raster stacks with default band names use the filename's year
  range, rather than assigning its final year to every band.
* Boundary downloads now respect `quiet = TRUE`; WHO messages correctly
  describe cached GeoPackages. Live OCHA tests accept French country names.
* Declared minimum versions of dplyr, purrr, tidyr and httr2. Updated the
  package description, added a self-contained example and optional-package
  guidance, and rewrote report parsing with tidyverse data frames.

* Set up the package infrastructure: DESCRIPTION, MIT licence, roxygen2
  documentation, testthat (third edition), GitHub Actions R CMD check,
  README and package logo.
* Added `get_boundaries()` and `list_boundaries()` to download
  administrative boundaries from OCHA COD-AB (default), geoBoundaries, GADM
  or WHO.
* Added `get_worldpop()` to download WorldPop population rasters.
* Added `get_ihme_mcv()` to download the IHME geospatial MCV1 and MCV2
  coverage estimates and methods documentation, using the latest release.
* Added `get_demog_wpp()` to get crude birth and death rates, the daily
  birth rate and the single-year age distribution from UN WPP 2024,
  for a country given by ISO3 code or name and for one or more years from
  1950 to 2100.
* `get_ihme_mcv()` retries files that fail during the concurrent download
  and reports the HTTP status or error for any that still fail.
* Added `get_worldpop_single_age()` to make single-year age rasters by
  splitting WorldPop age groups with UN WPP 2024 single-year population.
* WorldPop rasters are downloaded over HTTPS only (age-sex structures from
  the University of Southampton mirror). Each request retries on errors and
  dropped connections, and files still missing are tried again after 10 and
  30 seconds, except files the server does not have.
* Added `extract_raster()` to summarise rasters over boundary polygons,
  reading the year, month and single year of age from each filename.
* `get_worldpop_single_age()` now takes WPP age shares at the WorldPop
  reference date: 1 January for R2025A and R2024B, and an assumed 1 July for
  the legacy 2000-2020 data.
* `get_boundaries()` returns `adm0`, the name and code of each level, and
  `source`, plus `start_date` and `end_date` for WHO boundaries requested
  with `latest = FALSE`. The `adm1_code`, `adm2_code`, ...
  columns are now hashes of the unit's name and its parents' names, so they
  are the same across sources that spell the names the same way.
* `get_boundaries()` keeps each download as a single file in `cache_dir`,
  with no subfolders and the source at the end of the filename, and reads
  shapefiles directly from their zip archives.
* Added `reconstruct_birth_cohorts()` to rebuild the population by month of
  birth and calendar month from annual population by single year of age
  (#8). The `"uniform"` method spreads each anchor over its birth months;
  `"survival"` estimates births by month and carries cohorts forward with
  monthly survival from WPP mortality; `"constrained"` (the default) reweights
  anchors so that outlying ones pull the fit less. Cohorts can be extended
  past the last anchor with the national births trend and an optional
  birth seasonality profile. The output reports births, unexplained cohort
  change between anchors, and how well each anchor is reproduced.
* Added `cohort_eligibility()` to work out the share of a monthly cohort
  eligible on a vaccination date or during a campaign.
* `get_demog_wpp()` also returns births and single-age mortality rates for
  use in `reconstruct_birth_cohorts()`.
* `extract_raster()` gains `pattern` to choose which files in a folder are
  used, and keeps the `age` column when only some files carry an age.
* `get_worldpop_single_age()` names rasters for both sexes
  `{iso3}_age{NN}_{year}.tif`, without "total".
* `extract_raster()` no longer returns a `file_name` column.
* `get_worldpop_single_age()` downloads the WorldPop age-group rasters to a
  temporary folder and deletes them, so `out_dir` holds only the single-year
  rasters.
* `extract_raster()` rounds `sum` to whole numbers.
* `extract_raster()` gains `pop_rasters` and `pop_pattern` for population
  weighting. `fun` now also accepts `"weighted_mean"` and
  `"weighted_median"`, which weight each value cell by the population it
  holds, with population rasters matched to value rasters by year (or the
  nearest year). The output then includes `pop_sum`, the population in each
  polygon. In a folder, `pop_pattern` picks the under-one rasters from
  `get_worldpop_single_age()` by default.
* `extract_raster()` gives each layer of a stack without year names the
  years of the range in its filename, such as 2000 to 2025 for the 26 layers
  of the IHME MCV1 rasters.
* Added `read_ihme_mcv_admin()` to read the IHME MCV1 and MCV2 coverage
  tables by admin 1 or admin 2 unit into a tidy data frame.
* `reconstruct_birth_cohorts()` accepts `ref_date` as the name of a column
  holding each row's reference date, so sources with different reference
  dates can be combined. `worldpop_reference_date()` gives the date for
  each WorldPop year: 1 July for the legacy years before 2015 and 1 January
  from 2015.
* Added `prepare_campaigns()` to bring campaign calendars, post-campaign
  survey results and coverage reported by the campaign into standard
  campaign records (target ages in months, upper bound exclusive), a
  separate table of survey evaluation indicators, and diagnostics for
  missing dates and coverage, invalid ages, unmatched units, duplicates and
  overlapping campaigns. Missing dates and coverage are flagged, never
  inferred or imputed.
* Added the immunity engine, following the CDC immunity profile method (Li,
  Goodson and Perry 2024):
  * `build_vaccination_history()` lists routine and campaign opportunities
    for each monthly birth cohort, with eligibility worked out from each
    birth-date interval and IHME or survey coverage bounds kept.
  * `estimate_immunity()` tracks each cohort's vaccination state through
    its opportunities, with vaccine effectiveness by dose type and age
    (`ve_cdc()`) and a choice of dose-overlap rule, and reports susceptible
    and immune population by month and the protection each campaign added.
  * `summarise_immunity()` adds results up by geography, month and age
    band; `check_immunity()` compares them with post-campaign survey
    indicators; `immunity_draws()` gives intervals from coverage bounds.
  * `plot_cohort_protection()` draws the campaign protection heatmap and
    `map_immunity()` maps the susceptible share with data-driven scales.
    Both take a palette from `get_palette()` / `list_palettes()` and can
    save to a file. `save_plot()` and `compress_png()` save and compress
    figures.
* Added `plot_cohort_journey()` to follow one birth cohort over time in
  areas picked at percentiles of its susceptible share, with arrows for
  routine doses and the campaigns that reached it.
