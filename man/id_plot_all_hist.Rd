% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot.R
\name{id_plot_all_hist}
\alias{id_plot_all_hist}
\title{Density plots of Posterior Parameters}
\usage{
id_plot_all_hist(
  object,
  params = "person",
  param_labels = NULL,
  dens_type = "all",
  return_data = FALSE,
  func = median,
  ...
)
}
\arguments{
\item{object}{A fitted \code{idealstan} object}

\item{params}{Select the type of parameter from the model to plot. \code{'person'} for person/legislator ideal points,
\code{'miss_diff'} and \code{'miss_discrim'} for difficulty and discrimination parameters from the missing/inflated item/bill parameters,
and \code{'obs_diff'} and \code{'obs_discrim'} for difficulty and discrimination parameters from the non-missing/non-inflated
item/bill parameters.}

\item{param_labels}{A vector of labels equal to the number of parameters. Primarily useful if \code{return_data} is \code{TRUE}.}

\item{dens_type}{Can be \code{'all'} for showing 90\% HPD high, 10\% HPD low and median posterior values.
Or to show one of those posterior estimates at a time, use \code{'high'} for 90\% high HPD posterior estimate,
\code{'low'} for 10\% low HPD posterior estimate, and \code{'function'} for the whatever function is specificied
in \code{func} (median by default).}

\item{return_data}{Whether or not to return the plot as a ggplot2 object and the data together in a list instead of
plotting.}

\item{func}{The function to use if \code{'dens_type'} is set to \code{'function'}.}

\item{...}{Other options passed on to the plotting function, currently ignored.}
}
\description{
This function produces density plots of the different types of parameters in an \code{idealstan} model: item (bill) difficulty and discrimination
parameters, and person (legislator) ideal points.
}
