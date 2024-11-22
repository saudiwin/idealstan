# README
Robert Kubinec
2024-11-22

# Introduction to R Package Idealstan

*Note: This is a beta release of `idealstan` v1.0. While most features
have been implemented and are stable, there may be bugs that have not
been sorted out. To report bugs with the package, please file an issue
on the [Github page](https://github.com/saudiwin/idealstan/issues).*

At present, `idealstan` is only available on Github as one of its main
dependencies, `cmdstanr` is also not on CRAN. To use the package,
`cmdstanr` must be first be set up with a local installation of
`cmdstan`, which is used for estimation. To see how to install
`cmdstanr`, [see this guide](https://mc-stan.org/cmdstanr/). Note that
the `cmdstanr` default installation location should be used when
installing `cmdstan`.

To install this package, type the command
`remotes::install_github('saudiwin/idealstan')` at the R console prompt
(you first must have the `remotes` package installed from CRAN for this
to work). The best way to learn how the package works is to look at the
package vignettes, which can be accessed from the [package
website](https://saudiwin.github.io/idealstan/), especially
[Introduction to
Idealstan](https://saudiwin.github.io/idealstan/vignettes/Package_Introduction.html).

**If you use this package, please cite the following:**

Kubinec, Robert. “Generalized Ideal Point Models for Robust Measurement
with Dirty Data in the Social Sciences”. **SocArchiv** (2024).
doi:10.31219/osf.io/8j2bt.

The paper is available from [this link](https://osf.io/8j2bt/).

## About the Package

This package implements IRT (item response theory) ideal point models,
which are models designed for situations in which actors make strategic
choices that correlate with a unidimensional scale, such as the
left-right axis in American politics. Compared to traditional IRT, ideal
point models use a similar parameterization (the 2-Pl variant) but
without the strong assumption that all items load in the same direction
(i.e., higher ability). For more information, I refer you to [my
paper](https://osf.io/8j2bt/) about IRT and ideal point models,
documenting many of the features in the package.

The goal of `idealstan` is to offer both standard IRT/ideal point models
and additional models for missing data, time-varying ideal points and
diverse responses, such as binary, ordinal, count, continuous and
positive-continuous outcomes. In addition, `idealstan` uses the Stan
estimation engine to offer full Bayesian inference (with some options
for approximate inference) for all models so that every model is
estimated with uncertainty. Models can also have mixed outcomes, such as
discrete and continuous responses.

The approach to handling missing data in this package is to model
directly strategic censoring in observations. While this kind of missing
data pattern can be found in many situations in which data is not
missing at random, this particular version was developed to account for
legislatures in which legislators (persons) are strategically absent for
votes on bills (items). This approach to missing data can be usefully
applied to many contexts in which a missing outcome is a function of the
person’s ideal point (i.e., people will tend to be present in the data
when the item is far away or very close to their ideal point).

The package also includes ordinal ideal point models to handle
situations in which a ranked outcome is polarizing, such as a legislator
who can vote yes, no or to abstain. Because `idealstan` uses Bayesian
inference, it can model any kind of ordinal data even if there aren’t an
even distribution of ordinal categories for each item.

The package also has extensive plotting functions via `ggplot2` for
model parameters, particularly the legislator (person) ideal points
(ability parameters).
