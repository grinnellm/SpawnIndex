# Additional checks to run before R-CMD-CHECK

# Build the raw data files
source(file = file.path("data-raw", "pars.R"))

# Build the read me file
rmarkdown::render(input = "README.Rmd")
file.remove("README.html")

# Knit the technical report
setwd(dir = "tr")
knitr::knit2pdf(input = "SpawnIndex.Rnw")
setwd(dir = "..")

# Compile the manual
devtools::build_manual(path = "./doc")

# Styler
styler::style_pkg()
styler::style_file(path = file.path("vignettes", "Introduction.Rmd"))
styler::style_file(path = file.path("man", "sticker", "sticker.R"))
styler::style_file(path = "README.Rmd")
# styler::style_file(path = file.path("tr", "SpawnIndex.Rnw"))

# Lint
lintr::lint_package()
lintr::lint(filename = file.path("vignettes", "Introduction.Rmd"))
lintr::lint(filename = file.path("man", "sticker", "sticker.R"))
lintr::lint(filename = "README.Rmd")
lintr::lint(filename = file.path("tr", "SpawnIndex.Rnw"))

# Good practice (takes a while)
goodpractice::gp(path = ".")
