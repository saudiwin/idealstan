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