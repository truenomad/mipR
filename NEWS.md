# mipR 1.1.0

- Added `mip_conventions()` to choose the date and age rules:
  how births are placed in the birth month, whether age is calendar age
  or days divided by `days_per_month`, how routine dose dates are set, the
  monthly reporting day and whether it is taken before that day's
  opportunities, and the order of opportunities on the same date. The
  defaults are the existing rules, so results do not change.
  `mip_conventions_preset("v2_pipeline")` reproduces the vaccination-only
  runs of the measles campaign pipeline.
- `build_vaccination_history()`, `estimate_immunity()`, `immunity_draws()`
  and `cohort_eligibility()` gain a `conventions` argument. The history
  stores its conventions, and the estimate uses them unless given its own.
- `prepare_campaigns()` keeps an optional `coupling` column from the
  calendar, and `build_vaccination_history()` carries a `coupling` column
  in `campaigns` into the history.
- `build_vaccination_history()` gains `repeated_campaigns`. With
  `"separate"`, several records for one campaign and geography are applied
  as separate campaigns, renamed `campaign_id#2`, `campaign_id#3` and so
  on. Campaign identifiers need to be unique within a geography only.

# mipR 1.0.0

First stable release.

- Vaccination histories keep birth-day eligibility and dose dates. Monthly
  immunity counts only doses received by the 15th, and vaccine
  effectiveness is evaluated at the age on each dose date. Empty histories
  remain fully susceptible.
- Coverage draws preserve the caller's random-number state, share campaign
  draws across affected cohorts and run on parallel workers by default.
  The immunity engine is faster.
- Survey comparisons use campaign-specific windows, eligible children and
  explicit denominators, and report missing comparison windows.
- Constrained reconstruction rejects zero fitting tolerance, duplicate
  anchors and invalid demographic inputs. Sparse mortality, birth-rate and
  routine-coverage years use the nearest available year.
- Fixed cohort extension with zero birth-seasonality weights, map bins for
  susceptibility below 20%, population weighting on grids with a different
  coordinate reference system, and year labels for annual raster stacks.
- Added input validation across the engine, reconstruction and maps.
  `get_boundaries()` respects `quiet = TRUE`.
- Declared minimum versions of dplyr, purrr, tidyr and httr2.

# mipR 0.3.0

- Added the immunity engine, following the CDC immunity profile method
  (Li, Goodson and Perry 2024):
  - `build_vaccination_history()` lists routine and campaign opportunities
    for each monthly birth cohort.
  - `estimate_immunity()` tracks each cohort's vaccination state, with
    vaccine effectiveness by dose type and age (`ve_cdc()`), and reports
    susceptible and immune population by month and the protection each
    campaign added.
  - `summarise_immunity()` adds results up by geography, month and age
    band. `check_immunity()` compares them with post-campaign survey
    indicators. `immunity_draws()` gives intervals from coverage bounds.
- Added `map_immunity()`, `plot_cohort_protection()` and
  `plot_cohort_journey()`, with palettes from `get_palette()` and
  `list_palettes()`. `save_plot()` and `compress_png()` save and compress
  figures.

# mipR 0.2.0

- Added `reconstruct_birth_cohorts()` to rebuild the population by month
  of birth and calendar month from annual population by single year of age
  (#8), using the `"uniform"`, `"survival"` or `"constrained"` (default)
  method. Cohorts can be extended past the last anchor, and `ref_date` can
  name a column so sources with different reference dates can be combined.
- Added `cohort_eligibility()` for the share of a monthly cohort eligible
  on a vaccination date or during a campaign, and
  `worldpop_reference_date()` for the reference date of each WorldPop year.
- Added `prepare_campaigns()` to turn campaign calendars, post-campaign
  survey results and coverage reported by the campaign into standard
  campaign records, with diagnostics. Missing dates and coverage are
  flagged, never imputed.
- Added `read_ihme_mcv_admin()` to read IHME MCV1 and MCV2 coverage tables
  by admin 1 or admin 2.
- `get_demog_wpp()` also returns births and single-age mortality rates.

# mipR 0.1.0

- Set up the package: MIT licence, roxygen2 documentation, testthat
  (third edition) and GitHub Actions R CMD check.
- Added `get_boundaries()` and `list_boundaries()` to download
  administrative boundaries from OCHA COD-AB (default), geoBoundaries, GADM
  or WHO. Unit codes are hashes of the unit's name and its parents' names,
  so they match across sources.
- Added `get_worldpop()` and `get_worldpop_single_age()` to download
  WorldPop population rasters and split age groups into single years with
  UN WPP 2024. Downloads use HTTPS and retry on failure.
- Added `get_ihme_mcv()` to download IHME geospatial MCV1 and MCV2
  coverage estimates.
- Added `get_demog_wpp()` for crude birth and death rates and the
  single-year age distribution from UN WPP 2024.
- Added `extract_raster()` to summarise rasters over boundary polygons,
  with population-weighted mean and median.
