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

`mipR` reconstructs these histories at monthly resolution, so analysts can
examine:

- **Where:** Which geographical areas have the largest immunity gaps?
- **Who:** Which birth cohorts and age groups remain susceptible?
- **When:** How does susceptibility accumulate over time?
- **What next:** How might alternative vaccination strategies change
  population immunity?

## What mipR does

| Component | Functions |
|---|---|
| **Data acquisition** | `get_boundaries()`, `list_boundaries()` (OCHA, geoBoundaries, GADM, WHO); `get_worldpop()`, `get_worldpop_single_age()`; `get_ihme_mcv()`, `read_ihme_mcv_admin()`; `get_demog_wpp()` (UN WPP 2024 rates, age structure and mortality) |
| **Spatial extraction** | `extract_raster()`, with population weighting and year matching for raster stacks |
| **Population reconstruction** | `reconstruct_birth_cohorts()`, monthly birth cohorts calibrated to annual population estimates |
| **Campaigns** | `prepare_campaigns()`, standard campaign records, survey indicators and diagnostics |
| **Immunity estimation** | `build_vaccination_history()`, `estimate_immunity()`, `ve_cdc()`, `summarise_immunity()` |
| **Checks and uncertainty** | `check_immunity()` against post-campaign surveys; `immunity_draws()` from coverage bounds |
| **Visualisation** | `plot_cohort_protection()`, `map_immunity()`, `get_palette()`, `save_plot()` |

Still planned: infection-acquired immunity, and helpers for comparing
vaccination scenarios. Scenarios can already be run by changing the
campaign or coverage inputs and running the immunity steps again.

A worked example for Nigeria at state level is in
[`examples/nga_pipeline.R`](examples/nga_pipeline.R), with the campaign
inputs prepared in
[`examples/nga_campaign_data.R`](examples/nga_campaign_data.R).

## Methodological approach

### The CDC immunity profile method

`mipR` follows the measles population immunity profile method of Li,
Goodson and Perry (2024). For one birth cohort, Figure 1 of that paper
builds the immune share $I$ dose by dose. With MCV1 coverage $C_1$ and
effectiveness $E_1$, and MCV2 coverage $C_2$ and effectiveness $E_2$:

$$
I_1 = C_1 E_1
$$

$$
I_2 =
\begin{cases}
I_1 + (C_1 - I_1)\,\dfrac{C_2}{C_1}\,E_2 & \text{if } C_2 < C_1 \\[6pt]
I_1 + (C_1 - I_1)\,E_2 + (C_2 - C_1)\,E_2 & \text{if } C_2 \ge C_1
\end{cases}
$$

The share ever vaccinated is $V = \max(C_1, C_2)$. Each campaign $k$ that
targets the cohort, with coverage $S_k$ and effectiveness $E_k$, then
updates the immune share:

$$
I \leftarrow
\begin{cases}
I + (V - I)\,\dfrac{S_k}{V}\,E_k & \text{if } S_k < V \\[6pt]
I + (S_k - I)\,E_k, \quad V \leftarrow S_k & \text{if } S_k \ge V
\end{cases}
$$

and the susceptible share is $1 - I$. A campaign reaches children who were
already vaccinated before children with no dose. Effectiveness depends on
the dose type and the age at the dose: for MCV1 and campaign doses it is
0.58 below 9 months, 0.84 at 9 to 11 months, and 0.925 (MCV1) or 0.95
(campaigns) from 12 months; MCV2 is 0.95 (`ve_cdc()`).

### Extending it in space and time

`mipR` applies the same arithmetic to every geography and every monthly
birth cohort, and reports it for every calendar month.

**Indices.** $g$ is a geography, $c$ a monthly birth cohort covering the
birth dates $[b_c, b_{c+1})$, $t$ a calendar month and $k$ a vaccination
opportunity. Monthly values refer to the 15th of the month.

**Population.** The population of cohort $c$ in geography $g$ and month $t$
is

$$
N_{g,c,t} = B_{g,c}\,\ell_c(t)
$$

where $B_{g,c}$ is the number of births and $\ell_c(t)$ the share surviving
to month $t$, from UN WPP single-age mortality.
`reconstruct_birth_cohorts()` estimates $B_{g,c}$ from annual population
estimates $P_{g,a,y}$ (for example WorldPop) by single year of age $a$:
each estimate is shared across the birth months it covers and divided by
survival, and the births are fitted so that

$$
\sum_{c \in C(a,y)} B_{g,c}\,\ell_c(t_y)\,w_{c,a,y} \approx P_{g,a,y}
$$

within a tolerance, where $C(a,y)$ are the cohorts aged $a$ on the
reference date $t_y$ and $w_{c,a,y}$ is the share of cohort $c$ inside that
age. Estimates that disagree with the rest are down-weighted rather than
forced, and the remaining differences are reported.

**Eligibility.** Births are spread evenly over the days of the birth month.
The share of cohort $c$ eligible for opportunity $k$ is the share of its
birth dates $d$ whose age is in the target range at some time during the
opportunity:

$$
e_{c,k} = \frac{1}{|D_c|} \sum_{d \in D_c}
\mathbf{1}\!\left[\,\text{age}_d \in [a^{\min}_k, a^{\max}_k)
\text{ during } k\,\right]
$$

so a cohort born in January becomes eligible for MCV1 at 9 months over the
days of October, and a campaign reaches only the part of a cohort inside
its target ages.

**Coverage.** The share of the cohort reached by opportunity $k$ is

$$
r_{g,c,k} = \text{coverage}_{g,k} \times e_{c,k}
$$

with routine coverage for the calendar year of the dose (IHME, by
geography) and campaign coverage from post-campaign surveys, or coverage
reported by the campaign where no survey exists.

**State.** Each cohort is split into shares $\pi_{g,c}(s)$ by state
$s = (\text{doses}, \text{MCV1 received}, \text{protected})$. An
opportunity reaches $r_{g,c,k}$ of the cohort, filling groups of states in
order: MCV2 reaches MCV1 recipients first; with the dependent rule (the
CDC default) children with any dose are reached before children with none.
Reached children who are not protected become protected with probability
$E_k$. With full eligibility and national coverage this reproduces the
Figure 1 formulas above exactly.

**Monthly values.** If opportunity $k$ falls in month $t$ and a share
$\tau_{c,k}$ of the cohort has had it by the 15th,

$$
\pi_{g,c}(t) = \pi_{g,c}^{\text{before}} +
\tau_{c,k}\left(\pi_{g,c}^{\text{after}} - \pi_{g,c}^{\text{before}}\right)
$$

**Susceptible population.** The susceptible count in each geography, cohort
and month is

$$
U_{g,c,t} = N_{g,c,t} \sum_{s:\,\text{not protected}} \pi_{g,c}(s, t)
$$

and `summarise_immunity()` adds $U_{g,c,t}$ and $N_{g,c,t}$ over the
cohorts in each age band. Infants younger than 6 months are treated as
protected by maternal antibodies and left out of summaries. Immunity from
infection is not yet included.

**Uncertainty.** `immunity_draws()` repeats the calculation with coverage
drawn on the logit scale from its 95% interval (IHME bounds for routine
coverage, survey intervals for campaigns), correlated across years and
between MCV1 and MCV2.

## Installation

`mipR` is currently under development and is not available on CRAN.

The development version can be installed from GitHub using:

```r
pak::pak("truenomad/mipR")
```

## Development status

`mipR` is in active development. The functions above work and are tested,
but their interfaces may still change before the first release.

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
