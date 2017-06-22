README
================
Robert Kubinec
June 22, 2017

R Package Idealstan (V0.1) README
---------------------------------

This package implements new IRT models (models for absences and for dynamic hiearchical models) along with several standard IRT models (2 PL, rating scale, graded response) designed for modeling rollcall voting data and any other kind of binary or ordinal data. It is based on the Stan package for Bayesian modeling, which includes both MCMC full Bayesian inference and a faster variational Bayesian approximation. The package also has plotting functions for model parameters, particularly the legislator (person) ideal points (ability parameters).

This vignette demonstrates basic usage of the package, which is currently in alpha. A beta release is scheduled for July 13, 2017.

This package takes an approach to modeling legislative roll call data (and other IRT data) that uses a hurdle model to separately account for the probability that a legislator (i.e., test-taker) will show up to vote. This absence-inflated model estimates additional bill (item) parameters to reflect the relative gain or loss a legislator has from showing up to vote. Further details of this model are explained in Kubinec (2017).

Kubinec, Robert. 2017. “Absence Makes the Ideal Points Sharper.” In *2017 Political Methodology Annual Conference*.
