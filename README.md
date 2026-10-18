# mipR <img src="man/figures/logo.png" align="right" height="150" alt="mipR logo" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Spatiotemporal Measles Population Immunity Profiling**

`mipR` is an R package for reconstructing and projecting measles population
immunity across birth cohorts, geographical areas, and calendar months.

It integrates historical routine immunisation coverage, supplementary
immunisation activities (SIAs), and vaccine effectiveness assumptions to
estimate how population immunity and susceptibility change over time.

The package is designed to support **subnational immunity assessment,
identification of immunity gaps, and evidence-based vaccination campaign
planning**.

## Why mipR?

Measles outbreaks occur when sufficient numbers of susceptible individuals
accumulate in a population. Understanding this risk requires more than
current vaccination coverage.

Vaccination coverage describes the proportion of a target population
receiving a vaccine during a given period. Population immunity reflects the
cumulative effects of historical vaccination opportunities, vaccine
effectiveness, infection-acquired immunity, and demographic changes.

These processes vary across birth cohorts and geographical areas.

`mipR` is being developed to reconstruct these histories at monthly
resolution, allowing analysts to examine:

- **Where:** Which geographical areas have the largest immunity gaps?
- **Who:** Which birth cohorts and age groups remain susceptible?
- **When:** How does susceptibility accumulate over time?
- **What next:** How might alternative vaccination strategies change
  population immunity?

## Planned functionality

`mipR` is being developed around five components.

| Component | Planned capabilities |
|---|---|
| **Data acquisition** | Retrieve administrative boundaries, population estimates, and historical vaccination coverage |
| **Population reconstruction** | Reconstruct and follow birth cohorts across geographical areas and calendar months |
| **Immunity estimation** | Integrate routine vaccination, SIAs, vaccine effectiveness, and infection-acquired immunity |
| **Projection and scenarios** | Project susceptibility and compare alternative vaccination campaign strategies |
| **Visualisation** | Generate age-specific immunity profiles, cohort heatmaps, geographical maps, and temporal trends |

Additional functionality will support uncertainty propagation, sensitivity
analyses, and validation of input datasets.

*These capabilities are planned and have not yet been implemented.*

## Methodological approach

The package builds on the measles population immunity profiling framework
described by Li and colleagues (2024).

The underlying approach reconstructs immunity by following birth cohorts
through successive vaccination opportunities.

`mipR` aims to extend its practical application through:

1. **Monthly temporal resolution**, allowing vaccination histories and
   immunity to be reconstructed over time.
2. **Subnational estimation**, incorporating geographical variation in
   routine coverage, campaigns, and population structure.
3. **Flexible campaign assumptions**, including alternative models of overlap
   between routine and campaign vaccination.
4. **Prospective projections**, allowing immunity gaps to be assessed under
   alternative vaccination strategies.

The framework will distinguish vaccination-derived immunity from
infection-acquired immunity, with assumptions and uncertainty documented
explicitly.

## Installation

`mipR` is currently under development and is not available on CRAN.

Once the GitHub repository is available, the development version can be
installed using:

```r
pak::pak("OWNER/mipR")
```

Replace `OWNER` with the GitHub organisation or username hosting the
repository.

## Development status

`mipR` is in early development.

The initial release establishes the package structure, documentation, and
development infrastructure. Analytical functionality is not yet available.

The package interface and planned functionality may change during
development.

## Reference

The methodological foundation is described in:

Li X, Goodson JL, Perry RT. (2024). Measles Population Immunity Profiles:
Updated Methods and Tools. *Vaccines*, 12(8), 937.

https://doi.org/10.3390/vaccines12080937

`mipR` is an independently developed open-source package. It is not
affiliated with or endorsed by the US Centers for Disease Control and
Prevention.

## Citation

A software citation will be provided with the first functional release.

Once available, citation information can be retrieved using:

```r
citation("mipR")
```

## Licence

`mipR` is distributed under the MIT licence. See [LICENSE.md](LICENSE.md).
