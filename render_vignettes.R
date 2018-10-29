
# Script to run all relevant vignettes (including package test)

devtools::build_vignettes()
rmarkdown::render('package_test.Rmd')
