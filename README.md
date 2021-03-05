
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

Note: `R-CMD-check` works on my Windows machine but fails on GitHub
Actions (see [\#30](https://github.com/grinnellm/SpawnIndex/issues/30)).

## Description

The SpawnIndex package provides data, parameter values, and methods to
calculate the spawn index for Pacific Herring (*Clupea pallasii*) in
British Columbia (BC), Canada. Essentially, spawn index calculations
convert spawn survey observations (e.g., spawn extent, number of egg
layers, substrate type) to the Pacific Herring spawn index in BC. There
are three types of spawn survey observations: surface spawn
observations, Macrocystis spawn observations, and understory spawn
observations. In addition, we include methods to convert eggs to
biomass, and estimate spawning biomass in spawn-on-kelp operations. Note
that the ‘spawn index’ is a relative index of spawning biomass.

## Installation

Install the SpawnIndex package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(repo = "grinnellm/SpawnIndex")
```

## Examples

We show two example calculations: surface spawn egg density, and spawn
on kelp.

### Surface spawn egg density

This example shows how to calculate egg density for a surface spawn
survey. First, load the SpawnIndex package in the usual way.

``` r
library(SpawnIndex)
```

    ## This is SpawnIndex version 0.2.0.

Load the default parameter values for spawn index calculations, and
calculate egg density for a given number of egg layers.

``` r
data(pars)
layers <- 4
egg_density <- egg_dens_surf(alpha = pars$surface$alpha,
                             beta = pars$surface$beta,
                             egg_lyrs = layers)
egg_density
```

    ## [1] 863.57

In this example, spawn surveyors observed 4 layers of Pacific Herring
eggs, which has an estimated egg density of 863.57, where egg density is
in units of 10<sup>3</sup> eggs m<sup>-2</sup>.

### Spawn on kelp biomass

This example shows how to calculate the biomass of Pacific Herring that
spawned and produced eggs which were removed from the population by a
spawn-on-kelp (SOK) fishery.

First, calculate the conversion factor for the number of Pacific Herring
eggs to the spawn index (i.e., biomass) in tonnes, t. Call the function
with the default parameters.

``` r
theta <- calc_egg_conversion()
theta
```

    ## [1] 1e+08

Thus, convert eggs to biomass in tonnes by dividing the number of eggs
by 10<sup>8</sup>, where `theta` is in units of
10<sup>8</sup> eggs t<sup>-1</sup>. Finally, use `theta` to estimate the
biomass of Pacific Herring that produced a given amount of SOK product
in kilograms, kg.

``` r
sok <- 100
biomass <- calc_biomass_sok(sok = sok,
                            theta = theta)
biomass
```

    ## [1] 0.3266324

In this example, 100 kg of SOK was produced by 0.327 t of spawning
Pacific Herring.

## Additional information

The technical report has background information on the spawn index and
calculations. A draft technical report is available here:
`./tr/Draft.pdf`. **Please do not cite or circulate this draft.** In
addition, there is a vignette with an example workflow; build the
vignette

``` r
devtools::build_vignettes(pkg = ".")
```

and open the file `./doc/Introduction.html`.
