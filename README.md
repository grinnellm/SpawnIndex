# SpawnIndex <img src='sticker/SpawnIndex-sticker.png' align="right" height="150"/>

> Calculate the Pacific Herring spawn index

## Status

[![cran release](https://www.r-pkg.org/badges/version/SpawnIndex?color=blue)](https://cran.r-project.org/package=SpawnIndex)
[![R build status](https://github.com/grinnellm/SpawnIndex/workflows/R-CMD-check/badge.svg)](https://github.com/grinnellm/SpawnIndex/actions)
[![Codecov test coverage](https://codecov.io/gh/grinnellm/SpawnIndex/branch/master/graph/badge.svg)](https://codecov.io/gh/grinnellm/SpawnIndex)
[![Code size](https://img.shields.io/github/languages/code-size/grinnellm/SpawnIndex.svg)](https://github.com/grinnellm/SpawnIndex)

## Purpose

The **SpawnIndex** package calculates the spawn index for Pacific Herring
(*Clupea pallasii*) in British Columbia, Canada.
Note that the 'spawn index' is a relative index of spawning biomass.

## Download and install

Download and install the package as follows:

`devtools::install_github(repo = "grinnellm/SpawnIndex")`.

## Additional information

The technical report has background information on the spawn index and calculations.
A draft technical report is available here: "./tr/Draft.pdf".
Please do not cite or circulate this draft.
The vignette has an example workflow; build the vignette like so:

`build_vignettes(pkg = ".")`

and open the file "./doc/Introduction.html".
