# Other package documentation

#' Estimate Bayesian IRT ideal point models with Stan
#'
#' Idealstan enables you to run IRT-based ideal point models with the Stan Bayesian estimation engine. In addition, it includes
#' models that allow for censoring of the data, such as that occurs when legislators/persons are absent, and for ordinal outcomes,
#' such as when legislators abstain. 
#'
#' To get started, first look at how to pre-process your data with the \code{\link{id_make}} function, and then how to run a model
#' with the \code{\link{id_estimate}} function. To report a bug, please go to \url{www.github.com/saudiwin/idealstan} and put in an
#' issue in my Github page. You can email me at \email{rmk7xy@@virginia.edu}.
"_PACKAGE"

#' Rollcall vote data for 114th Senate
#'
#' This rollcall vote object (see \code{\link[pscl]{rollcall}}) contains the full voting records for the 114th Senate in the US
#' Congress. The data can be pre-processed via the \code{\link{id_make}} function for estimation. See package vignette for details.
#'
#' @format A rollcall vote object that is a list with \code{votes} as the legislator-bill rollcall vote matrix.
#' @source \url{https://www.voteview.com}
"senate114"

#' Fitted \code{idealstan} model for 114th Senate Data
#' 
#' This R object is a fitted \code{idealstan} object that can be used for testing and illustratory purposes.
#' 
#' @format A fitted \code{idealstan} object
"senate114_fit"