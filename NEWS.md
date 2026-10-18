# mipR 0.0.1

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
* `get_boundaries()` returns `adm0`, the name and code of each level,
  `start_date`, `end_date` and `source`. The `adm1_code`, `adm2_code`, ...
  columns are now hashes of the unit's name and its parents' names, so they
  are the same across sources that spell the names the same way.
* `get_boundaries()` keeps each download as a single file in `cache_dir`,
  with no subfolders and the source at the end of the filename, and reads
  shapefiles directly from their zip archives.
* Added `reconstruct_cohorts()` (uniform method) to rebuild the population by
  month of birth and calendar month from annual population by single year of
  age, and `cohort_eligibility()` to work out the share of a monthly cohort
  eligible on a vaccination date or during a campaign (#8).
* `extract_raster()` gains `pattern` to choose which files in a folder are
  used, and keeps the `age` column when only some files carry an age.
* `get_worldpop_single_age()` names rasters for both sexes
  `{iso3}_age{NN}_{year}.tif`, without "total".
* `extract_raster()` no longer returns a `file_name` column.
* `get_worldpop_single_age()` downloads the WorldPop age-group rasters to a
  temporary folder and deletes them, so `out_dir` holds only the single-year
  rasters.
