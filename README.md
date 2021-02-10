
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# SpawnIndex <img src='man/sticker/sticker.png' align="right" height="200"/>

Calculate the spawn index for Pacific Herring (*Clupea pallasii*) in
British Columbia, Canada.

<!-- badges: start -->

[![R build
status](https://github.com/grinnellm/SpawnIndex/workflows/R-CMD-check/badge.svg)](https://github.com/grinnellm/SpawnIndex/actions)
[![Codecov test
coverage](https://codecov.io/gh/grinnellm/SpawnIndex/branch/master/graph/badge.svg)](https://codecov.io/gh/grinnellm/SpawnIndex)
[![Code
factor](https://github.com/grinnellm/SpawnIndex/workflows/lint/badge.svg)](https://github.com/grinnellm/SpawnIndex/actions)
[![Development
version](https://img.shields.io/badge/Version-0.2.0-orange.svg?style=flat-square)](commits/master)
[![CRAN
status](https://www.r-pkg.org/badges/version/SpawnIndex)](https://CRAN.R-project.org/package=SpawnIndex)
<!-- badges: end -->

## Description

Calculate the spawn index for Pacific Herring (Clupea pallasii) in
British Columbia, Canada. There are three types of spawn survey
observations: surface spawn, Macrocystis spawn, and understory spawn.
These calculations rely on an estimate of female fecundity to convert
the number of eggs to the spawn index (i.e., biomass). In addition, we
provide a method to estimate the biomass of fish from spawn-on-kelp
(SOK) operations. Note that the ‘spawn index’ is a relative index of
spawning biomass.

## Installation

Install the SpawnIndex package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(repo = "grinnellm/SpawnIndex")
```

Note that the SpawnIndex package is not available on
[CRAN](https://cloud.r-project.org/).

## Example

This example which shows how to calculate the conversion factor for the
number of Pacific Herring eggs to the spawn index (i.e., biomass) in
tonnes.

``` r
library(SpawnIndex)
data(pars)
theta <- calc_egg_conversion()
theta
```

    ## [1] 1e+08

Thus, we convert eggs to biomass in tonnes by dividing the number of
eggs by 10^{8}.

## Additional information

The technical report has background information on the spawn index and
calculations. A draft technical report is available here:
`./tr/Draft.pdf`. **Please do not cite or circulate this draft.** In
addition, there is a vignette with an example workflow; build the
vignette like so

``` r
devtools::build_vignettes(pkg = ".")
```

and open the file `./doc/Introduction.html`.
