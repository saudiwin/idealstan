Introduction to R Package Idealstan
================
Robert Kubinec
October 26, 2018

*Note: To report bugs with the package, please file an issue on the [Github page](https://github.com/saudiwin/idealstan/issues). You are currently reading the README file, which largely follows the introductory vignette in the package. To install this package, type the command `devtools::install_github('saudiwin/idealstan',local=F)` at the R console prompt. To include the package vignettes in the package install, which can be accessed by the command `vignette(package='idealstan)`, use instead the command `devtools::install_github('saudiwin/idealstan',local=F,build_vignette=TRUE)`. To install the unstable development branch `develop`, use this command: `devtools::install_github('saudiwin/idealstan',local=F,ref='develop')`.*


**If you use this package, please cite the following:**

Kubinec, Robert. "Generalized Ideal Point Models for Time-Varying and Missing-Data Inference". Working Paper.


This package implements IRT (item response theory) ideal point models, which are models designed for situations in which actors make strategic choices that correlate with a unidimensional scale, such as the left-right axis in American politics. Compared to traditional IRT, ideal point models examine the polarizing influence of a set of items on a set of persons, and has simlarities to models based on Euclidean latent spaces, such as multi-dimensional scaling. For more information, I refer you to my paper presented at [StanCon 2018](https://zenodo.org/record/1284361#.W9O_5WhKhPY) and the R package vignettes that can be accessed on [CRAN](https://CRAN.R-project.org/package=idealstan).

The goal of `idealstan` is to offer both standard ideal point models and additional models for missing data, time-varying ideal points and diverse responses, such as binary, ordinal, count, continuous and positive-continuous outcomes. In addition, `idealstan` uses the Stan estimation engine to offer full and variational Bayesian inference for all models so that every model is estimated with uncertainty. The package also exploits variational inference to automatically identify models instead of requiring users to pre-specify which persons or items in the data to constrain in advance.

The approach to handling missing data in this package is to model directly strategic censoring in observations. While this kind of missing data pattern can be found in many situations in which data is not missing at random, this particular version was developed to account for legislatures in which legislators (persons) are strategically absent for votes on bills (items). This approach to missing data can be usefully applied to many contexts in which a missing outcome is a function of the person's ideal point (i.e., people will tend to be present in the data when the item is far away or very close to their ideal point).

The package also includes ordinal ideal point models to handle situations in which a ranked outcome is polarizing, such as a legislator who can vote yes, no or to abstain. Because `idealstan` uses Bayesian inference, it can model any kind of ordinal data even if there aren't an even distribution of ordinal categories for each item.

The package also has extensive plotting functions via `ggplot2` for model parameters, particularly the legislator (person) ideal points (ability parameters).


