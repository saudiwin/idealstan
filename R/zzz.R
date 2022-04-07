.onLoad <- function(libname, pkgname) {
  # check for cmdstanR installation
  if (!require("cmdstanr", character.only = TRUE)) {
      print("Note: you have not installed cmdstanr. To do so, please go to https://mc-stan.org/cmdstanr/.")
  } 
}
