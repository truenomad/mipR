# mipR 0.0.1

* Set up the package infrastructure: DESCRIPTION, MIT licence, roxygen2
  documentation, testthat (third edition), GitHub Actions R CMD check,
  README and package logo.
* Added `get_boundaries()` and `list_boundaries()` to download
  administrative boundaries from OCHA COD-AB (default), geoBoundaries, GADM
  or WHO.
* Added `get_worldpop()`, `get_worldpop_age_band()` and
  `get_worldpop_paths()` to download WorldPop population rasters.
* Added `get_ihme_mcv()` to download the IHME geospatial MCV1 and MCV2
  coverage estimates and methods documentation, using the latest release.
* Added `get_demog_wpp()` to get the age distribution, crude birth and
  death rates and daily birth rate from UN WPP 2024, for a country given by
  ISO3 code or name and for one or more years from 1950 to 2100.
* `get_worldpop_age_band()` now keeps the individual band rasters by default
  (`keep_band_files = TRUE`), and gains `combine = FALSE` to keep only the
  band rasters without writing the combined raster.
* `get_ihme_mcv()` retries files that fail during the concurrent download
  and reports the HTTP status or error for any that still fail.
