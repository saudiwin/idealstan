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
  
  n_votes <- nrow(object@score_data@score_matrix)
  n_iters <- nrow(all_params$sigma_reg_free)
  
  if(!is.null(sample_scores)) {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  these_draws <- sample(1:n_iters,draws)
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))
  
  
  y <- as.numeric(object@score_data@score_matrix$outcome)[this_sample]
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3,5,7,9,11,13) & !is.null(object@score_data@miss_val)) {
    y <- na_if(y,object@score_data@miss_val)
  }
  if(object@use_groups) {
    person_points <- as.numeric(object@score_data@score_matrix$group_id)[this_sample]
  } else {
    person_points <- as.numeric(object@score_data@score_matrix$person_id)[this_sample]
  }

  bill_points <- as.numeric(object@score_data@score_matrix$item_id)[this_sample]
  time_points <- as.numeric(object@score_data@score_matrix$time_id)[this_sample]
  
  remove_nas <- !is.na(y) && !is.na(person_points) && !is.na(bill_points) && !is.na(time_points)
  y <- y[remove_nas]
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  person_points <- person_points[remove_nas]
  
  model_type <- object@model_type
  latent_space <- model_type %in% c(13,14)
  # we can do the initial processing here
  
  # loop over posterior iterations
  
  pr_absence_iter <- sapply(these_draws, function(d) {
    if(latent_space) {
      # use latent-space formulation for likelihood
      pr_absence <- sapply(1:length(person_points),function(n) {
        -sqrt((all_params$L_tp1[d,time_points[n],person_points[n]] - all_params$A_int_free[d,bill_points[n]])^2)
      }) %>% plogis()
    } else {
      # use IRT formulation for likelihood
      pr_absence <- sapply(1:length(person_points),function(n) {
        all_params$L_tp1[d,time_points[n],person_points[n]]*all_params$sigma_abs_free[d,bill_points[n]] - all_params$A_int_free[d,bill_points[n]]
      }) %>% plogis()
      
    }
    return(pr_absence)
  })

  pr_vote_iter <- sapply(these_draws, function(d) {
    if(latent_space) {
      if(inflate) {
        pr_vote <- sapply(1:length(person_points),function(n) {
          -sqrt((all_params$L_tp1[d,time_points[n],person_points[n]] - all_params$B_int_free[d,bill_points[n]])^2)
        }) %>% plogis()
      } else {
        # latent space non-inflated formulation is different
        pr_vote <- sapply(1:length(person_points),function(n) {
          all_params$sigma_reg_free[d,bill_points[n]] + all_params$sigma_abs_free[d,bill_points[n]] -
            sqrt((all_params$L_tp1[d,time_points[n],person_points[n]] - all_params$B_int_free[d,bill_points[n]])^2)
        }) %>% plogis()
      }
      
    } else {
      pr_vote <- sapply(1:length(person_points),function(n) {
        all_params$L_tp1[d,time_points[n],person_points[n]]*all_params$sigma_reg_free[d,bill_points[n]] - all_params$B_int_free[d,bill_points[n]]
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
    cutpoints <- all_params$steps_votes
  } else if(model_type %in% c(5,6)) {
    cutpoints <- all_params$steps_votes_grm
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
                          person_points=person_points,
                          sigma_sd=all_params$extra_sd,
                          cutpoints=cutpoints,
                          type='predict')
  
  # set attributes to pass along sample info
  
  attr(out_predict,'this_sample') <- this_sample
  
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
#' This funciton can then be used to 
#' fit an information criterion to assess model fit, 
#' see the package vignettes and the \code{\link[loo]{loo}} package for details.
#' @param object A fitted \code{idealstan} object
#' @param draws The number of draws to use from the total number of posterior draws (default is 100).
#' @param sample_scores In addition to reducing the number of posterior draws 
#'  used to calculate the posterior predictive distribution,
#'  you can sample from the scores/votes themselves. 
#'  To do so, set \code{sample_scores} to the number of scores/votes to sample.
#' @param r_eff Whether to report a matrix used for calculation of effective samples (see \code{\link[loo]{relative_eff}})
#' @param ... Other arguments passed on to underlying function (currently unused)
#' 
#' @return This function returns an S by N matrix of the log density of posterior draws, 
#' where S is the size of the 
#' posterior sample and N is the total number of parameters,
#' or an array of I by C by N,
#' where I is the number of MCMC iterations per chain, C is the number of chains from the Stan model object, and N 
#' is the size of the underlying data. The first matrix can be fed directly to `loo` while the second
#' array is used for the calculation of effective samples by loo.
#' @export
setMethod('id_log_lik',signature(object='idealstan'),function(object,...,draws=100,sample_scores=NULL,
                                                              r_eff=FALSE) {
  
  if(r_eff==F) {
    # if the point is to get the log likelihood density, use this code
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
  } else {
    # conversely, to get the actual log likelihood with chain indices for use to 
    # calculate r_eff with loo, then use this version
    
    all_params_chains <- rstan::extract(object@stan_samples,permuted=F)
    
    # loop over chains 
    
    over_chains <- lapply(1:dim(all_params_chains)[2], function(c) {
      
      
      all_params <- all_params_chains[,c,]
      legis_points <- rep(1:nrow(object@score_data@score_matrix),times=ncol(object@score_data@score_matrix))
      bill_points <- rep(1:ncol(object@score_data@score_matrix),each=nrow(object@score_data@score_matrix))
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
      n_iters <- nrow(all_params)
      
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
      
      # create a list from all parameters
      
      # first need to reshape L_full - quite a pain
      
      L_full <- all_params[,grepl(x = colnames(all_params),
                                  pattern='L_full')] %>% 
        as_data_frame %>% 
        mutate(iter=1:n()) %>% 
        gather(key='param',value='estimate',-iter) %>% 
        mutate(time=as.numeric(stringr::str_extract_all(param,"[0-9]+",simplify=T)[1:n()]),
               id=as.numeric(stringr::str_extract_all(param,"[0-9]+",simplify=T)[(n()+1):(n()*2)])) %>% 
        select(-param) %>% 
        spread(key = id,value=estimate)
      
      L_full <- select(L_full,-iter) %>% 
        split(.$time) %>% 
        lapply(function(this_data) {
          this_data$time <- NULL
          return(this_data)
        }) %>% 
        abind::abind(along=3) %>% 
        aperm(perm=c(1,3,2))
      
      to_list <- list(sigma_abs_full=all_params[,grepl(x = colnames(all_params),
                                                  pattern='sigma_abs_full')],
                      sigma_reg_full=all_params[,grepl(x = colnames(all_params),
                                                       pattern='sigma_reg_full')],
                      L_full=L_full,
                      B_int_full=all_params[,grepl(x = colnames(all_params),
                                                   pattern='B_int_full')],
                      A_int_full=all_params[,grepl(x = colnames(all_params),
                                                   pattern='A_int_full')],
                      steps_votes=all_params[,grepl(x = colnames(all_params),
                                                    pattern='steps_votes')],
                      steps_votes_grm=all_params[,grepl(x = colnames(all_params),
                                                        pattern='steps_votes_grm')])
                      
      
      
      out_predict <- rep_func(all_params=to_list,
                              legis_points=legis_points,
                              bill_points=bill_points,
                              obj=object,
                              time=time_points,
                              sample_draws=these_draws,
                              sample_scores=this_sample,
                              y=y)
    })
    
    # exponentiate and then bind these together as an array
    out_array <- lapply(over_chains,exp) %>% 
      abind::abind(along=3) %>% 
      aperm(perm=c(1,3,2))
    return(out_array)
  }

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
#' bill/item by specifying the ID of each in the original data as a character vector. 
#' Only persons or items can be specified,
#' not both.
#' 
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the \code{\link{id_post_pred}} function on a fitted idealstan object
#' @param person A character vector of the person IDs to calculate their model predictions
#' @param item A character vector of item IDs to calculate their model predictions
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                  person=NULL,
                                                                    item=NULL,...) {

  this_sample <- attr(ppc_pred,'this_sample')
  
  y <- as.numeric(object@score_data@score_matrix$outcome)[this_sample]
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(object@model_type %in% c(1,3,5,7,9,11,13) & !is.null(object@score_data@miss_val)) {
    y <- na_if(y,object@score_data@miss_val)
  }
  if(object@use_groups) {
    person_points <- as.numeric(object@score_data@score_matrix$group_id)[this_sample]
  } else {
    person_points <- as.numeric(object@score_data@score_matrix$person_id)[this_sample]
  }
  
  bill_points <- as.numeric(object@score_data@score_matrix$item_id)[this_sample]
  time_points <- as.numeric(object@score_data@score_matrix$time_id)[this_sample]
  
  remove_nas <- !is.na(y) && !is.na(person_points) && !is.na(bill_points) && !is.na(time_points)
  y <- y[remove_nas]
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  person_points <- person_points[remove_nas]
  
  if(!is.null(item) && !is.null(person))
    stop('Please only specify an index to item or person, not both.')
  
  grouped <- T
  
  if(!is.null(person)) {
    if(object@use_groups) {
      group_var <- as.numeric(object@score_data@score_matrix$group_id %in% person)
    } else {
      group_var <- as.numeric(object@score_data@score_matrix$person_id %in% person)
    }
    
  } else if(!is.null(item)) {
    group_var <- as.numeric(object@score_data@score_matrix$item_id %in% item)
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
