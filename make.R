# Additional checks to run before R-CMD-CHECK

# Build the raw data files
source(file.path("data-raw", "pars.R"))

# Build the read me file
rmarkdown::render("README.Rmd")

# Knit the technical report
setwd("tr")
knitr::knit2pdf("SpawnIndex.Rnw")
setwd("..")

# Compile the manual
devtools::build_manual(path = "./doc")

# Styler
styler::style_pkg()
styler::style_file(file.path("vignettes", "Introduction.Rmd"))
styler::style_file(file.path("tr", "SpawnIndex.Rnw"))

# Lint
lintr::lint_package()
lintr::lint(file.path("vignettes", "Introduction.Rmd"))
lintr::lint(file.path("tr", "SpawnIndex.Rnw"))

# Good practice (takes a while)
goodpractice::gp()
