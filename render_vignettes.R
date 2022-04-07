
# Script to run all relevant vignettes (including package test)

rmarkdown::render('vignettes/How_to_Evaluate_Models.Rmd')
rmarkdown::render('vignettes/Package_Introduction.Rmd')
rmarkdown::render('vignettes/Time_Series.Rmd')
rmarkdown::render('package_test.Rmd')
