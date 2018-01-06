# These functions are implemented for compatibility with the 
# rstantools package (and rstanarm)
# They are S3 generic methods that are run on idealstan objects

#' Generic function for drawing from the posterior predictive distribution
#'
#' This generic is provided for compability with \code{\link[rstan]{stan}}.
#' 
#' @param object A fitted \code{idealstan} object
#' @param ... All other parameters passed on to the underlying function.
#' @export
#' @return \code{posterior_predict} methods should return a \eqn{D} by \eqn{N}
#'   matrix, where \eqn{D} is the number of draws from the posterior predictive
#'   distribution and \eqn{N} is the number of data points being predicted per
#'   draw.
posterior_predict <- function(object, ...) {
  UseMethod("posterior_predict")
}

#' Posterior Prediction for \code{idealstan} objects
#' 
#' This function will draw from the posterior predictive distribution of the outcome, i.e., all the scores or 
#'  votes that are used to create the \code{idealstan} model. 
#'  
#'  You can then use functions such as 
#'  \code{\link[bayesplot]{ppc_bars}} to see how well the model does returning the correct number of categories
#'  in the score/vote matrix. 
#'  Also see \code{help("posterior_predict", package = "rstanarm")}
#'
#' @param object A fitted \code{idealstan} object
#' @param draws The number of draws to use from the total number of posterior draws (default is 100).
#' @param sample_scores In addition to reducing the number of posterior draws used to calculate the posterior predictive distribution.
#' @param ... Any other arguments passed on to posterior_predict (currently none available)
#' 
#' @export
posterior_predict.idealstan <- function(object,draws=100,sample_scores=NULL,...) {

  all_params <- rstan::extract(object@stan_samples)
  legis_points <- rep(1:dim(all_params$L_full)[3],times=ncol(all_params$sigma_reg_full))
  bill_points <- rep(1:ncol(all_params$sigma_reg_full),each=dim(all_params$L_full)[3])
  time_points <- object@vote_data@time[bill_points]
  n_votes <- length(legis_points)
  n_iters <- nrow(all_params$sigma_reg_full)
  
  if(!is.null(sample_scores)) {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  these_draws <- sample(1:n_iters,draws)
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))
  
  model_type <- object@model_type
  
  rep_func <- switch(as.character(model_type),`4`=.predict_abs_ord,
                     `1`=.predict_2pl,`2`=.predict_abs_bin,
                     `3`=.predict_ord)
  
  out_predict <- rep_func(all_params=all_params,
                          legis_points=legis_points,
                          bill_points=bill_points,
                          obj=object,
                          time=time_points,
                          sample_draws=these_draws,
                          sample_scores=this_sample)
  
  class(out_predict) <- c('matrix','ppd')
  return(out_predict)
}

#' Generic Method for Extracting Log Likelihood from Stan Objects
#' 
#' This function is a generic that is used to match the functions used with \code{\link[loo]{loo}} to calculate
#' Bayesian information criteria on models.
#' 
#' @export
extract_log_lik <- function(object, ...) {
  UseMethod("extract_log_lik")
}

#' Extract Log-Likelihood of the Posterior
#' 
#' This function returns a matrix of an S by N matrix of the log density of posterior draws from a fitted idealstan model, where S is the size of the 
#' posterior sample and N is the total number of parameters in the idealstan model. This matrix can then be used to 
#' fit an information criterion to assess model fit, see the \code{\link[loo]{loo}} package for details.
#' @param object A fitted \code{idealstan} object
#' 
#' @param draws The number of draws to use from the total number of posterior draws (default is 100).
#' @param sample_scores In addition to reducing the number of posterior draws used to calculate the posterior predictive distribution,
#'  you can sample from the scores/votes themselves. To do so, set \code{sample_scores} to the number of scores/votes to sample.
#'
#' @export
extract_log_lik.idealstan <- function(object,...,draws=100,sample_scores=NULL) {
  
  all_params <- rstan::extract(object@stan_samples)
  legis_points <- rep(1:dim(all_params$L_full)[3],times=ncol(all_params$sigma_reg_full))
  bill_points <- rep(1:ncol(all_params$sigma_reg_full),each=dim(all_params$L_full)[3])
  time_points <- object@vote_data@time[bill_points]
  y <- c(object@score_data@score_matrix)
  remove_nas <- !is.na(y)
  y <- y[remove_nas]

  n_votes <- length(legis_points)
  n_iters <- nrow(all_params$sigma_reg_full)
  
  if(!is.null(sample_scores)) {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  these_draws <- sample(1:n_iters,draws)
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))
  
  model_type <- object@model_type
  
  rep_func <- switch(as.character(model_type),`4`=.predict_abs_ord_ll,
                     `1`=.predict_2pl_ll,`2`=.predict_abs_bin_ll,
                     `3`=.predict_ord_ll)
  
  out_predict <- rep_func(all_params=all_params,
                          legis_points=legis_points,
                          bill_points=bill_points,
                          obj=object,
                          time=time_points,
                          sample_draws=these_draws,
                          sample_scores=this_sample,
                          y=y)
  
  class(out_predict) <- c('matrix','ppd')
  return(out_predict)
}
