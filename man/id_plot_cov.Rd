% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot.R
\name{id_plot_cov}
\alias{id_plot_cov}
\title{Marginal Effects Plot for Hierarchical Covariates}
\usage{
id_plot_cov(
  object,
  calc_param = NULL,
  label_high = "High",
  label_low = "Low",
  group_effects = NULL,
  plot_model_id = NULL,
  pred_outcome = NULL,
  lb = 0.05,
  upb = 0.95,
  facet_ncol = 2,
  cov_type = "person_cov",
  ...
)
}
\arguments{
\item{object}{A fitted \code{idealstan} object}

\item{calc_param}{Whether to calculate ideal point marginal effects for
a given covariate. If NULL, the default, the function will instead produce
a plot of the raw coefficients from the ideal point model. If passing the
name of a covariate, should be a character value of a column in the data
passed to the
\code{id_make} function.}

\item{label_high}{What label to use on the plot for the high end of the
latent scale}

\item{label_low}{What label to use on the plot for the low end of the
latent scale}

\item{group_effects}{Character value for name of column in data by which to
subset the data. Must be a column passed to the \link{id_make} function}

\item{plot_model_id}{The integer of the model ID to plot. If NULL and there
are multiple model types, \code{facet_wrap} will be used to produce multiple
plots with one for each model type.}

\item{pred_outcome}{For discrete models with more than 2 categories,
or binary models with missing data, which outcome to predict. This should
be the value that matches what the outcome was coded as in the data
passed to \code{\link[=id_make]{id_make()}}.}

\item{lb}{The lower limit of the posterior density to use for
calculating credible intervals}

\item{upb}{The upper limit of the posterior density to use for
calculating credible intervals}

\item{facet_ncol}{If facetting by multiple models or grouped factors, sets the
number of columns in the multiple plots}

\item{cov_type}{Either \code{'person_cov'} for person or group-level hierarchical parameters,
\code{'discrim_reg_cov'} for bill/item discrimination parameters from regular (non-inflated) model, and
\code{'discrim_infl_cov'} for bill/item discrimination parameters from inflated model.}

\item{...}{Additional argument passed on to \code{id_me}}
}
\value{
A \code{ggplot2} plot that can be further customized with \code{ggplot2} functions if need be.
}
\description{
This function will calculate and plot the ideal point marginal effects, or the first derivative
of the IRT/ideal point model with respect to the hierarchical covariate,
for each item in the model. The function \code{\link[=id_me]{id_me()}} is used
to first calculate the ideal point marginal effects.
}
\details{
The ends of the latent variable can be specified via the
\code{label_low} and \code{label_high} options, which will use those
labels for item discrimination.

Note that the function produces a \code{ggplot2} object, which can
be further modified with \code{ggplot2} functions.
}
