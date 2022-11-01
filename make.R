# Additional checks to run before `R-CMD-CHECK`

# Load packages
require(SpawnIndex)

# Build the raw data files
source(file = here::here("data-raw", "pars.R"))

# Build the read me file
rmarkdown::render(input = here::here("README.Rmd"))
file.remove(here::here("README.html"))

# Knit the technical report
setwd(dir = here::here("tr"))
knitr::knit2pdf(input = "SpawnIndex.Rnw")
setwd(dir = here::here())

# Compile the supporting documents
devtools::build_manual(path = here::here("doc"))
devtools::build_vignettes(pkg = ".")

# Styler
styler::style_pkg()
styler::style_file(path = here::here("vignettes", "Introduction.Rmd"))
styler::style_file(path = here::here("man", "sticker", "sticker.R"))
styler::style_file(path = here::here("README.Rmd"))
styler::style_file(path = here::here("tr", "SpawnIndex.Rnw"))

# Lint
lintr::lint_package()
lintr::lint(filename = here::here("vignettes", "Introduction.Rmd"))
lintr::lint(filename = here::here("man", "sticker", "sticker.R"))
lintr::lint(filename = here::here("README.Rmd"))
lintr::lint(filename = here::here("tr", "SpawnIndex.Rnw"))

# Good practice (takes a while; restart R and require `SpawnIndex` first)
goodpractice::gp(path = ".")
