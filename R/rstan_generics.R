# These functions are implemented for compatibility with the 
# rstantools package (and rstanarm)

#' Generic Method for Obtaining Posterior Predictive Distribution from Stan Objects
#' 
#' This function is a generic that is used to match the functions used with \code{\link[bayesplot]{ppc_bars}} to calculate
#' the posterior predictive distribution of the data given the model.
#' 
#' @param object A fitted \code{idealstan} object
#' @param ... All other parameters passed on to the underlying function.
#' @export
#' @return \code{posterior_predict} methods should return a \eqn{D} by \eqn{N}
#'   matrix, where \eqn{D} is the number of draws from the posterior predictive
#'   distribution and \eqn{N} is the number of data points being predicted per
#'   draw.
#' @export
setGeneric('id_post_pred',signature='object',
           function(object,...) standardGeneric('id_post_pred'))


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
setMethod('id_post_pred',signature(object='idealstan'),function(object,draws=100,sample_scores=NULL,...) {

  all_params <- rstan::extract(object@stan_samples)
  y <- c(object@score_data@score_matrix)
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3) & !is.null(object@score_data@miss_val)) {
    y <- na_if(y,object@score_data@miss_val)
  }
  
  legis_points <- rep(1:dim(all_params$L_full)[3],times=ncol(all_params$sigma_reg_full))
  bill_points <- rep(1:ncol(all_params$sigma_reg_full),each=dim(all_params$L_full)[3])
  time_points <- object@score_data@time[bill_points]
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
  
  remove_nas <- !is.na(y)
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  legis_points <- legis_points[remove_nas]
  
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
})

#' Generic Method for Extracting Log Likelihood from Stan Objects
#' 
#' This function is a generic that is used to match the functions used with \code{\link[loo]{loo}} to calculate
#' Bayesian information criteria on models.
#' @param object A fitted \code{idealstan} object
#' @param ... Other arguments passed onto underlying functions 
#' @export
setGeneric('id_log_lik',signature='object',
           function(object,...) standardGeneric('id_log_lik'))



#' Extract Log-Likelihood of the Posterior
#' 
#' This function returns a matrix of an S by N matrix of the log density of posterior draws from a fitted idealstan model, where S is the size of the 
#' posterior sample and N is the total number of parameters in the idealstan model. This matrix can then be used to 
#' fit an information criterion to assess model fit, see the \code{\link[loo]{loo}} package for details.
#' @param object A fitted \code{idealstan} object
#' @param draws The number of draws to use from the total number of posterior draws (default is 100).
#' @param sample_scores In addition to reducing the number of posterior draws 
#' used to calculate the posterior predictive distribution,
#'  you can sample from the scores/votes themselves. 
#'  To do so, set \code{sample_scores} to the number of scores/votes to sample.
#' @param ... Other arguments passed on to underlying function (currently unused)
#'
#' @export
setMethod('id_log_lik',signature(object='idealstan'),function(object,...,draws=100,sample_scores=NULL) {
  
  all_params <- rstan::extract(object@stan_samples)
  legis_points <- rep(1:dim(all_params$L_full)[3],times=ncol(all_params$sigma_reg_full))
  bill_points <- rep(1:ncol(all_params$sigma_reg_full),each=dim(all_params$L_full)[3])
  time_points <- object@score_data@time[bill_points]
  y <- c(object@score_data@score_matrix)
  
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3) & !is.null(object@score_data@miss_val)) {
    y <- na_if(y,object@score_data@miss_val)
  }
  
  remove_nas <- !is.na(y)
  y <- y[remove_nas]
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  legis_points <- legis_points[remove_nas]

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
})

#' Generic Method for Plotting Posterior Predictive Distribution
#' 
#' This function is a wrapper around \code{\link[bayesplot]{ppc_bars}} that enables the plotting of the posterior
#' predictive distribution from \code{\link{id_post_pred}} against the original data and for the distribution for 
#' individual persons/legislators and bills/items.
#' 
#' @param object A fitted \code{idealstan} object
#' @param ... Other arguments passed on to underlying functions
#' @export
setGeneric('id_plot_ppc',signature='object',
           function(object,...) standardGeneric('id_plot_ppc'))

#' Plot Posterior Predictive Distribution for \code{idealstan} Objects
#' 
#' This function is a wrapper around \code{\link[bayesplot]{ppc_bars}} that plots the posterior predictive distribution
#' derived from \code{\link{id_post_pred}} against the original data. You can also specify a legislator/person or
#' bill/item by specifying the index of each in the original score/vote matrix. Only person or items can be specified,
#' not both.
#' 
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the \code{\link{id_post_pred}} function on a fitted idealstan object
#' @param person The indices of the rows (persons/legislators) around which to compare the posterior prediction
#' @param item The indices of the columns (items/bills) around which to compare the posterior prediction
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                  person=NULL,
                                                                    item=NULL,...) {
  num_legis <- nrow(object@score_data@score_matrix)
  num_bills <- ncol(object@score_data@score_matrix)
  legispoints <- rep(1:num_legis,times=num_bills)
  billpoints <- rep(1:num_bills,each=num_legis)
  y <- c(object@score_data@score_matrix)
  
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3) & !is.null(object@score_data@miss_val)) {
    y <- na_if(y,object@score_data@miss_val)
  }
  
  remove_nas <- !is.na(y)
  y <- y[remove_nas]
  billpoints <- billpoints[remove_nas]
  legispoints <- legispoints[remove_nas]
  
  if(!is.null(item) && !is.null(person))
    stop('Please only specify an index to item or person, not both.')
  
  grouped <- T
  
  if(!is.null(person)) {
    group_var <- if_else(legispoints %in% person, legispoints, 0L)
  } else if(!is.null(item)) {
    group_var <- if_else(billpoints %in% item, billpoints, 0L)
  } else {
    grouped <- F
  }
  
  if(grouped && length(unique(group_var))>2) {
    
    bayesplot::ppc_bars_grouped(y=y[group_var>0],yrep=ppc_pred[,group_var>0],group=group_var[group_var>0],...)
    
  } else {
    if(grouped) {
      bayesplot::ppc_bars(y=y[group_var>0],yrep=ppc_pred[,group_var>0],...)
    } else {
      bayesplot::ppc_bars(y=y,yrep=ppc_pred,...)
    }
    
    
  }
  
})
