% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstan_generics.R
\name{id_post_pred}
\alias{id_post_pred}
\title{Generic Method for Obtaining Posterior Predictive Distribution from Stan Objects}
\usage{
id_post_pred(object, ...)
}
\arguments{
\item{object}{A fitted \code{idealstan} object}

\item{...}{All other parameters passed on to the underlying function.}
}
\value{
\code{posterior_predict} methods should return a \eqn{D} by \eqn{N}
matrix, where \eqn{D} is the number of draws from the posterior predictive
distribution and \eqn{N} is the number of data points being predicted per
draw.
}
\description{
This function is a generic that is used to match the functions used with \code{\link[bayesplot:PPC-discrete]{bayesplot::ppc_bars()}} to calculate
the posterior predictive distribution of the data given the model.
}
