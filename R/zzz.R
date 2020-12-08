.onLoad <- function(libname, pkgname) {
  # check for cmdstanR installation
  if (!require("cmdstanr", character.only = TRUE)) {
      print("Note: you have not installed cmdstanr. To do so, please go to https://mc-stan.org/cmdstanr/.")
  } else if(is.null(cmdstanr::cmdstan_version())) {
    print("You need to install cmdstan with cmdstanr to compile models. Use the function install_cmdstan() in the cmdstanr package.")
  }
}
