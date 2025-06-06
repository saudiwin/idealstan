% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot.R
\name{id_show_trues}
\alias{id_show_trues}
\title{This function plots the results from a simulation generated by \code{\link[=id_sim_gen]{id_sim_gen()}}.}
\usage{
id_show_trues(sims, type = "RMSE")
}
\arguments{
\item{sims}{A fitted \code{idealstan} object that has true data generated by \code{\link[=id_sim_gen]{id_sim_gen()}}}

\item{type}{Type of analysis of true versus fitted values, can be \code{'RMSE'}, \code{'Residuals'} or \code{'Coverage'}}
}
\description{
This function plots the results from a simulation generated by \code{\link[=id_sim_gen]{id_sim_gen()}}.
}
