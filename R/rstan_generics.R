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
#' This function will draw from the posterior distribution, whether in terms of the outcome (prediction)
#' or to produce the log-likelihood values.  
#'  
#'  This function can also produce either distribution of the 
#'  outcomes (i.e., predictions) or the log-likelihood values of the posterior (set option 
#'  \code{type} to \code{'log_lik'}.
#'  For more information, see the package vignette How to Evaluate Models.
#'  
#'  You can then use functions such as 
#'  \code{\link{id_plot_ppc}} to see how well the model does returning the correct number of categories
#'  in the score/vote matrix. 
#'  Also see \code{help("posterior_predict", package = "rstanarm")}
#'
#' @param object A fitted \code{idealstan} object
#' @param draws The number of draws to use from the total number of posterior draws (default is 100).
#' @param sample_scores In addition to reducing the number of posterior draws used to 
#' calculate the posterior predictive distribution, which will reduce computational overhead.
#' Only available for calculating predictive distributions, not log-likelihood values.
#' @param type Whether to produce posterior predictive values (\code{'predict'}, the default),
#' or log-likelihood values (\code{'log_lik'}). See the How to Evaluate Models vignette for more info.
#' @param output If the model has an unbounded outcome (Poisson, continuous, etc.), then
#' specify whether to show the \code{'observed'} data (the default) or the binary 
#' output \code{'missing'} showing whether an observation was predicted as missing or not
#' @param ... Any other arguments passed on to posterior_predict (currently none available)
#' 
#' @export
setMethod('id_post_pred',signature(object='idealstan'),function(object,draws=100,
                                                                output='observed',
                                                                type='predict',
                                                                sample_scores=NULL,...) {
  #all_params <- rstan::extract(object@stan_samples)
  
  n_votes <- nrow(object@score_data@score_matrix)
  n_iters <- (object@stan_samples@stan_args[[1]]$iter-object@stan_samples@stan_args[[1]]$warmup)*length(object@stan_samples@stan_args)
  if(!is.null(sample_scores) && type!='log_lik') {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  if(type!='log_lik') {
    these_draws <- sample(1:n_iters,draws)
  } else {
    these_draws <- 1:n_iters
    draws <- n_iters
  }
  
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))
  

  y <- as.numeric(object@score_data@score_matrix$outcome)[this_sample]
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3,5,7,9,11,13) & !is.null(object@score_data@miss_val)) {
    y <- .na_if(y,object@score_data@miss_val)
  }
  if(object@use_groups) {
    person_points <- as.numeric(object@score_data@score_matrix$group_id)[this_sample]
  } else {
    person_points <- as.numeric(object@score_data@score_matrix$person_id)[this_sample]
  }

  bill_points <- as.numeric(object@score_data@score_matrix$item_id)[this_sample]
  time_points <- as.numeric(object@score_data@score_matrix$time_id)[this_sample]
  
  remove_nas <- !is.na(y) & !is.na(person_points) & !is.na(bill_points) & !is.na(time_points)
  y <- y[remove_nas]
  max_val <- max(y)
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  person_points <- person_points[remove_nas]
  
  model_type <- object@model_type
  latent_space <- model_type %in% c(13,14)
  # we can do the initial processing here
  
  # loop over posterior iterations
  
  L_tp1 <- .extract_nonp(object@stan_samples,'L_tp1')[[1]]
  A_int_free <- .extract_nonp(object@stan_samples,'A_int_free')[[1]]
  B_int_free <- .extract_nonp(object@stan_samples,'B_int_free')[[1]]
  sigma_abs_free <- .extract_nonp(object@stan_samples,'sigma_abs_free')[[1]]
  sigma_reg_free <- .extract_nonp(object@stan_samples,'sigma_reg_free')[[1]]
  

  pr_absence_iter <- sapply(these_draws, function(d) {
    if(latent_space) {
      # use latent-space formulation for likelihood
      pr_absence <- sapply(1:length(person_points),function(n) {
        -sqrt((L_tp1[d,time_points[n],person_points[n]] - A_int_free[d,bill_points[n]])^2)
      }) %>% plogis()
    } else {
      # use IRT formulation for likelihood
      pr_absence <- sapply(1:length(person_points),function(n) {
        L_tp1[d,time_points[n],person_points[n]]*sigma_abs_free[d,bill_points[n]] - A_int_free[d,bill_points[n]]
      }) %>% plogis()
      
    }
    return(pr_absence)
  })

  pr_vote_iter <- sapply(these_draws, function(d) {
    if(latent_space) {
      if(inflate) {
        pr_vote <- sapply(1:length(person_points),function(n) {
          -sqrt((L_tp1[d,time_points[n],person_points[n]] - B_int_free[d,bill_points[n]])^2)
        }) %>% plogis()
      } else {
        # latent space non-inflated formulation is different
        pr_vote <- sapply(1:length(person_points),function(n) {
          sigma_reg_free[d,bill_points[n]] + sigma_abs_free[d,bill_points[n]] -
            sqrt((L_tp1[d,time_points[n],person_points[n]] - B_int_free[d,bill_points[n]])^2)
        }) %>% plogis()
      }
      
    } else {
      pr_vote <- sapply(1:length(person_points),function(n) {
        L_tp1[d,time_points[n],person_points[n]]*sigma_reg_free[d,bill_points[n]] - B_int_free[d,bill_points[n]]
      }) %>% plogis()
    }
    
    return(pr_vote)
  })

  
  rep_func <- switch(as.character(model_type),
                     `1`=.binary,
                     `2`=.binary,
                     `3`=.ordinal_ratingscale,
                     `4`=.ordinal_ratingscale,
                     `5`=.ordinal_grm,
                     `6`=.ordinal_grm,
                     `7`=.poisson,
                     `8`=.poisson,
                     `9`=.normal,
                     `10`=.normal,
                     `11`=.lognormal,
                     `12`=.lognormal,
                     `13`=.binary,
                     `14`=.binary)
  
  # pass along cutpoints as well

  if(model_type %in% c(3,4)) {
    cutpoints <- .extract_nonp(object@stan_samples,'steps_votes')[[1]]
    cutpoints <- cutpoints[these_draws,]
  } else if(model_type %in% c(5,6)) {
    cutpoints <- .extract_nonp(object@stan_samples,'steps_votes_grm')[[1]]
    cutpoints <- cutpoints[these_draws,,]
  } else {
    cutpoints <- 1
  }
  
  out_predict <- rep_func(pr_absence=pr_absence_iter,
                          pr_vote=pr_vote_iter,
                          N=length(person_points),
                          ordinal_outcomes=length(unique(object@score_data@score_matrix$outcome)),
                          inflate=model_type %in% c(2,4,6,8,10,12,14),
                          time_points=time_points,
                          item_points=bill_points,
                          max_val=max_val,
                          outcome=y,
                          miss_val=object@score_data@miss_val,
                          person_points=person_points,
                          sigma_sd=.extract_nonp(object,'extra_sd')[these_draws],
                          cutpoints=cutpoints,
                          type=type,
                          output=output)

  # set attributes to pass along sample info
  attr(out_predict,'chain_order') <- attr(L_tp1,'chain_order')[these_draws]
  attr(out_predict,'this_sample') <- this_sample
  
  if(type=='predict') {
    class(out_predict) <- c('matrix','ppd')
  } else if(type=='log_lik') {
    class(out_predict) <- c('matrix','log_lik')
  }
  
  return(out_predict)
})

#' Generic Method for Plotting Posterior Predictive Distribution
#' 
#' This function is a wrapper around \code{\link[bayesplot]{ppc_bars}},
#' \code{\link[bayesplot]{ppc_dens_overlay}} and 
#' \code{\link[bayesplot]{ppc_violin_grouped}}
#' that enables the plotting of the posterior
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
#' This function is a wrapper around \code{\link[bayesplot]{ppc_bars}},
#' \code{\link[bayesplot]{ppc_dens_overlay}} and 
#' \code{\link[bayesplot]{ppc_violin_grouped} that plots the posterior predictive distribution
#' derived from \code{\link{id_post_pred}} against the original data. You can also subset the 
#' posterior predictions over
#' legislators/persons or
#' bills/item sby specifying the ID of each in the original data as a character vector. 
#' Only persons or items can be specified,
#' not both.
#' 
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the \code{\link{id_post_pred}} function on a fitted idealstan object
#' @param group A character vector of the person or group IDs 
#' over which to subset the predictive distribution
#' @param item A character vector of item IDs to subset the posterior distribution
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                 group=NULL,
                                                                    item=NULL,...) {

  this_sample <- attr(ppc_pred,'this_sample')
  
  # create grouping variable
  if(!is.null(group)) {
    if(object@use_groups) {
      group_var <- factor(object@score_data@score_matrix$group_id, levels=group)
    } else {
      group_var <- factor(object@score_data@score_matrix$person_id, levels=group)
    }
    grouped <- T
  } else if(!is.null(item)) {
    group_var <- factor(object@score_data@score_matrix$item_id, levels=item)
    grouped <- T
  } else {
    grouped <- F
  }
  
  y <- object@score_data@score_matrix$outcome[this_sample]
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3,5,7,9,11,13) & !is.null(object@score_data@miss_val)) {
    y <- .na_if(y,object@score_data@miss_val)
  }
  
  if(object@use_groups) {
    person_points <- as.numeric(object@score_data@score_matrix$group_id)[this_sample]
  } else {
    person_points <- as.numeric(object@score_data@score_matrix$person_id)[this_sample]
  }
  
  bill_points <- as.numeric(object@score_data@score_matrix$item_id)[this_sample]
  time_points <- as.numeric(object@score_data@score_matrix$time_id)[this_sample]
  
  remove_nas <- !is.na(y) & !is.na(person_points) & !is.na(bill_points) & !is.na(time_points)
  y <- y[remove_nas]
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  person_points <- person_points[remove_nas]
  group_var <- group_var[remove_nas]
  
  # create a second one for the grouping variable
  
  remove_nas_group <- !is.na(group)
  
  if(!is.null(item) && !is.null(person))
    stop('Please only specify an index to item or person, not both.')
  
  if(attr(ppc_pred,'output')=='all') {
    y <- as.numeric(y)
    if(grouped) {
      
      bayesplot::ppc_bars_grouped(y=y[remove_nas_group],yrep=ppc_pred[,remove_nas_group],
                                  group=group_var[remove_nas_group],...)
      
    } else {
        bayesplot::ppc_bars(y=y,yrep=ppc_pred,...)
      
      
    }
  } else if(attr(ppc_pred,'output')=='observed') {
    # only show observed data for yrep
    y <- .na_if(y,object@score_data@miss_val)
    to_remove <- !is.na(y)
    y <- y[to_remove]
    group_var <- group_var[to_remove]
    remove_nas_group <- !is.na(group_var)
    y <- as.numeric(y)
    if(attr(ppc_pred,'output_type')=='continuous') {
      ppc_pred <- ppc_pred[,to_remove]
      #unbounded observed outcomes (i.e., continuous)
      if(grouped) {
        bayesplot::ppc_violin_grouped(y=y[remove_nas_group],yrep=ppc_pred[,remove_nas_group],
                                      group=group_var[remove_nas_group],
                                      ...)
      } else {
        bayesplot::ppc_dens_overlay(y=y,yrep=ppc_pred,...)
      }
      
    } else if(attr(ppc_pred,'output_type')=='discrete') {
      ppc_pred <- ppc_pred[,to_remove]
      y <- as.numeric(y)
      if(grouped) {
        
        bayesplot::ppc_bars_grouped(y=y[remove_nas_group],yrep=ppc_pred[,remove_nas_group],
                                    group=group_var[remove_nas_group],...)
        
      } else {
          bayesplot::ppc_bars(y=y,yrep=ppc_pred,...)
      }
    }

    
  } else if(attr(ppc_pred,'output')=='missing') {

    y <- .na_if(y,object@score_data@miss_val)
    y <- as.numeric(is.na(y))
    if(grouped) {
      
      bayesplot::ppc_bars_grouped(y=y[remove_nas_group],yrep=ppc_pred[,remove_nas_group],
                                  group=group_var[remove_nas_group],...)
      
    } else {

        bayesplot::ppc_bars(y=y,yrep=ppc_pred,...)
      
    }
  }
  

  

  
})


#' Helper Function for `loo` calculation
#' 
#' This function accepts a log-likelihood matrix produced by `id_post_pred` and 
#' extracts the IDs of the MCMC chains. It is necessary to use this function
#' as the second argument to the `loo` function along with an exponentiated 
#' log-likelihood matrix. See the package vignette How to Evaluate Models 
#' for more details.
#' 
#' @param ll_matrix A log-likelihood matrix as produced by the \code{\link{id_post_pred}}
#' function
#' @export
derive_chain <- function(ll_matrix=NULL) {
  attr(ll_matrix,'chain_order')
}