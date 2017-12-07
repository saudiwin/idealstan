# These functions are implemented for compatibility with the 
# rstantools package (and rstanarm)
# They are S3 generic methods that are run on idealstan objects

#' Generic function for drawing from the posterior predictive distribution
#'
#' Draw from the posterior predictive distribution of the outcome. See
#' \code{\link[rstanarm]{posterior_predict.stanreg}} in the
#' \pkg{\link[rstanarm]{rstanarm}} package for an example.
#'
#' @export
#' @return \code{posterior_predict} methods should return a \eqn{D} by \eqn{N}
#'   matrix, where \eqn{D} is the number of draws from the posterior predictive
#'   distribution and \eqn{N} is the number of data points being predicted per
#'   draw.
#'
#'
#' @examples
#' # Example using rstanarm package:
#' # posterior_predict method for 'stanreg' objects
#' \donttest{
#' if (require("rstanarm")) {
#'   fit <- stan_glm(mpg ~ wt + am, data = mtcars)
#'   yrep <- posterior_predict(fit)
#'   all.equal(ncol(yrep), nobs(fit))
#'
#'   nd <- data.frame(wt = mean(mtcars$wt), am = c(0, 1))
#'   ytilde <- posterior_predict(fit, newdata = nd)
#'   all.equal(ncol(ytilde), nrow(nd))
#' }
#' }
#'
#' # Also see help("posterior_predict", package = "rstanarm")
#'
posterior_predict <- function(object, ...) {
  UseMethod("posterior_predict")
}

#' @export
posterior_predict.idealstan <- function(object,draws=100,seed=NULL,
                                        sample_votes=NULL) {

  all_params <- rstan::extract(object@stan_samples)
  legis_points <- rep(1:dim(all_params$L_full)[3],times=ncol(all_params$sigma_reg_full))
  bill_points <- rep(1:ncol(all_params$sigma_reg_full),each=dim(all_params$L_full)[3])
  time_points <- object@vote_data@time[bill_points]
  n_votes <- length(legis_points)
  n_iters <- nrow(all_params$sigma_reg_full)
  
  if(!is.null(sample_votes)) {
    this_sample <- sample(1:n_votes,sample_votes)
  } else {
    this_sample <- 1:n_votes
  }
  
  these_draws <- sample(1:n_iters,draws)
  
  print(paste0('Processing posterior replications for ',n_votes,' votes using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))
  
  model_type <- object@model_type
  
  rep_func <- switch(as.character(model_type),`4`=.predict_abs_ord,
                     `1`=.predict_2pl,`2`=.predict_abs_bin)
  
  out_predict <- rep_func(all_params=all_params,
                          legis_points=legis_points,
                          bill_points=bill_points,
                          obj=object,
                          time=time_points,
                          sample_draws=these_draws,
                          sample_votes=this_sample)
  
  class(out_predict) <- c('matrix','ppd')
  return(out_predict)
}

#' @export
extract_log_lik <- function(object, ...) {
  UseMethod("extract_log_lik")
}

#' This function returns a matrix of an S by N matrix of draws from a fitted idealstan model, where S is the size of the 
#' posterior sample and N is the total number of parameters in the idealstan model. This matrix can then be used to 
#' fit an information criterion to assess model fit, see the loo package for details.
#' @export
extract_log_lik.idealstan <- function(object) {
  
}
