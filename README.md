# Calculate the Pacific Herring spawn index

The **SpawnIndex** package calculates the spawn index for Pacific Herring
(*Clupea pallasii*) in British Columbia, Canada.
Note that the 'spawn index' is a relative index of spawning biomass.

## Status

Travis: [![Travis build status](https://travis-ci.org/grinnellm/SpawnIndex.svg?branch=master)](https://travis-ci.org/github/grinnellm/SpawnIndex)
(Note that travis is not currently implemented; see [issue #12](https://github.com/grinnellm/SpawnIndex/issues/12).)

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
