# SpawnIndex <img src='man/sticker/sticker.png' align="right" height="200"/>

Calculate the spawn index for Pacific Herring (*Clupea pallasii*) in British Columbia, Canada.

<!-- badges: start -->
[![R build status](https://github.com/grinnellm/SpawnIndex/workflows/R-CMD-check/badge.svg)](https://github.com/grinnellm/SpawnIndex/actions)
[![Codecov test coverage](https://codecov.io/gh/grinnellm/SpawnIndex/branch/master/graph/badge.svg)](https://codecov.io/gh/grinnellm/SpawnIndex)
[![CRAN status](https://www.r-pkg.org/badges/version/SpawnIndex)](https://CRAN.R-project.org/package=SpawnIndex)
[![Code size](https://img.shields.io/github/languages/code-size/grinnellm/SpawnIndex.svg)](https://github.com/grinnellm/SpawnIndex)
[![CodeFactor](https://github.com/grinnellm/SpawnIndex/workflows/lint/badge.svg)](https://github.com/grinnellm/SpawnIndex/actions)
<!-- badges: end -->

## Description

Calculate the spawn index for Pacific Herring (Clupea pallasii) in British Columbia, Canada.
There are three types of spawn survey observations:
surface spawn, Macrocystis spawn, and understory spawn.
These calculations rely on an estimate of female fecundity to
convert the number of eggs to the spawn index (i.e., biomass).
In addition, we provide a method to estimate the biomass of fish from spawn-on-kelp (SOK) operations.
Note that the 'spawn index' is a relative index of spawning biomass.

## Download and install

Download and install the package:

`devtools::install_github(repo = "grinnellm/SpawnIndex")`.

## Additional information

The technical report has background information on the spawn index and calculations.
A draft technical report is available here: `./tr/Draft.pdf`.
**Please do not cite or circulate this draft.**
The vignette has an example workflow; build the vignette like so:

`build_vignettes(pkg = ".")`,

and open the file `./doc/Introduction.html`.
