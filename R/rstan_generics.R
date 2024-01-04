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
#' For reproducibility, you can also pass a vector of specific draws to use.
#' @param sample_scores In addition to reducing the number of posterior draws used to 
#' calculate the posterior predictive distribution, which will reduce computational overhead.
#' Only available for calculating predictive distributions, not log-likelihood values.
#' @param type Whether to produce posterior predictive values (\code{'predict'}, the default),
#' or log-likelihood values (\code{'log_lik'}). See the How to Evaluate Models vignette for more info.
#' @param output If the model has an unbounded outcome (Poisson, continuous, etc.), then
#' specify whether to show the \code{'observed'} data (the default) or the binary 
#' output \code{'missing'} showing whether an observation was predicted as missing or not
#' @param covar What kind of covariates to include as part of the prediction -- either
#' "person" (the default) or "items" if you included predictors for item discriminations.
#' @param newdata Optional: pass a data frame that must have all of the predictors that
#' were given to the id_make function. Used to generate predictions from person or item
#' covariates on to items.
#' @param use_cores Number of cores to use for multicore parallel processing with
#' the base R \code{parallel} package
#' @param ... Any other arguments passed on to posterior_predict (currently none available)
#' 
#' @export
setMethod('id_post_pred',signature(object='idealstan'),function(object,draws=100,
                                                                output='observed',
                                                                type='predict',
                                                                covar="person",
                                                                sample_scores=NULL,
                                                                use_cores=1,
                                                                newdata=NULL,...) {

  n_votes <- object@this_data$N
  

  n_iters <- nrow(as_draws_matrix(object@stan_samples$draws("L_full")))
  
  if(!is.null(sample_scores) && type!='log_lik') {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  if(type!='log_lik') {
    
    if(length(draws)==1) {
      
      these_draws <- sample(1:n_iters,draws)
      
    } else {
      
      these_draws <- draws
      draws <- length(draws)
      
    }
    
    
  } else {
    these_draws <- 1:n_iters
    draws <- n_iters
  }
  
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))

  if(length(object@this_data$Y_cont)>1) {
    Y_cont <- object@this_data$Y_cont[this_sample]
  } else {
    Y_cont <- object@this_data$Y_cont
  }
  
  if(length(object@this_data$Y_int)>1) {
    Y_int <- object@this_data$Y_int[this_sample]
  } else {
    Y_int <- object@this_data$Y_int
  }
  
  modelpoints <- object@this_data$mm[this_sample]
  #discrete <- object@score_data@score_matrix$discrete[this_sample]
  ordered_id <- object@this_data$order_cats_rat[this_sample]
  
  person_points <- object@this_data$ll[this_sample]

  bill_points <- object@this_data$bb[this_sample]
  time_points <- object@this_data$time[this_sample]
  
  # check for covariates & newdata
  
  if(length(object@score_data@person_cov_formula)>0) {
    
    if(is.null(newdata)) {
      
      # pull existing data & sample it
      
      legis_x <- object@this_data$legis_pred[this_sample,]
      
    } else {
      
      legis_x <- model.matrix(object@score_data@person_cov_formula,
                              data=newdata)[,-1,drop=FALSE]

      # remove NAs if necessary
      
      legis_x <- legis_x[object@remove_nas,]
      
      if(nrow(legis_x)!=nrow(object@this_data$legis_pred)) {
        
        stop("Newdata must be same number of rows as original data.")
        
      }
      
      legis_x <- legis_x[this_sample,]
      
    }
    
    cov_type <- "persons"
    
  } else if(length(object@score_data@item_cov_formula)>0) {
    
    cov_type <- "items"
    
  } else if(length(object@score_data@item_cov_miss_formula)>0) {
    
    cov_type <- "items_missing"
    
  } else {
    
    cov_type <- "none"
    
  }
  
  # we can do the initial processing here
  
  # loop over posterior iterations
  if(object@this_data$`T`>1) {
    L_tp1 <- object@time_varying
    
    # add in person covariates if present
    
    if(cov_type=="persons") {
      
      L_tp1 <- .add_person_cov(L_tp1, object, legis_x, person_points, time_points)
      
    }
    
  }
  
  L_full <- object@stan_samples$draws('L_full') %>% as_draws_matrix()
  A_int_free <- object@stan_samples$draws('A_int_free') %>% as_draws_matrix()
  B_int_free <- object@stan_samples$draws('B_int_free') %>% as_draws_matrix()
  sigma_abs_free <- object@stan_samples$draws('sigma_abs_free') %>% as_draws_matrix()
  sigma_reg_free <- object@stan_samples$draws('sigma_reg_free') %>% as_draws_matrix()
  
  # check if we need to update covariates
  
  if(cov_type!="none" && object@this_data$`T`==1) {
    
    # add in static covariates
    
    browser()
    
  }

  # loop over model IDs
  
  out_prs <- lapply(unique(modelpoints), function(m) {
    
    latent_space <- m %in% c(13,14)
    
    inflate <- m %in% c(2,4,6,8,10,12,14)
    
    print(paste0("Now on model ",m))
    
    # loop over itempoints

    out_items <- parallel::mclapply(unique(bill_points)[unique(bill_points) %in% unique(bill_points[modelpoints==m])], function(i) {

      this_obs <- which(bill_points==i)
      
      if(length(unique(object@score_data@score_matrix$time_id))>1) {
        
        pr_absence_iter <- sapply(these_draws, function(d) {
          if(latent_space) {
            # use latent-space formulation for likelihood
            pr_absence <- sapply(this_obs,function(n) {
              print(this_obs[n])
              this_time <- paste0("L_tp1[",time_points[n],",",person_points[n],"]")
              print(this_time)
              -sqrt((L_tp1[d,this_time] - A_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          } else {
            # use IRT formulation for likelihood
            pr_absence <- sapply(this_obs,function(n) {
              this_time <- paste0("L_tp1[",time_points[n],",",person_points[n],"]")
              L_tp1[d,this_time]*sigma_abs_free[d,bill_points[n]] - A_int_free[d,bill_points[n]]
            }) %>% plogis()
            
          }
          return(pr_absence)
        })
        
        pr_vote_iter <- sapply(these_draws, function(d) {
          if(latent_space) {
            if(inflate) {
              pr_vote <- sapply(this_obs,function(n) {
                this_time <- paste0("L_tp1[",time_points[n],",",person_points[n],"]")
                -sqrt((L_tp1[d,this_time] - B_int_free[d,bill_points[n]])^2)
              }) %>% plogis()
            } else {
              # latent space non-inflated formulation is different
              pr_vote <- sapply(this_obs,function(n) {
                this_time <- paste0("L_tp1[",time_points[n],",",person_points[n],"]")
                sigma_reg_free[d,bill_points[n]] + sigma_abs_free[d,bill_points[n]] -
                  sqrt((L_tp1[d,this_time] - B_int_free[d,bill_points[n]])^2)
              }) %>% plogis()
            }
            
          } else {
            pr_vote <- sapply(this_obs,function(n) {
              this_time <- paste0("L_tp1[",time_points[n],",",person_points[n],"]")
              L_tp1[d,this_time]*sigma_reg_free[d,bill_points[n]] - B_int_free[d,bill_points[n]]
            }) %>% plogis()
          }
          
          return(pr_vote)
        })
      } else {
        
        pr_absence_iter <- sapply(these_draws, function(d) {
          if(latent_space) {
            # use latent-space formulation for likelihood
            pr_absence <- sapply(this_obs,function(n) {
              -sqrt((L_full[d,person_points[n]] - A_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          } else {
            # use IRT formulation for likelihood
            pr_absence <- sapply(this_obs,function(n) {
              L_full[d,person_points[n]]*sigma_abs_free[d,bill_points[n]] - A_int_free[d,bill_points[n]]
            }) %>% plogis()
            
          }
          return(pr_absence)
        })
        
        pr_vote_iter <- sapply(these_draws, function(d) {
          if(latent_space) {
            if(inflate) {
              pr_vote <- sapply(this_obs,function(n) {
                -sqrt((L_full[d,person_points[n]] - B_int_free[d,bill_points[n]])^2)
              }) %>% plogis()
            } else {
              # latent space non-inflated formulation is different
              pr_vote <- sapply(this_obs,function(n) {
                sigma_reg_free[d,bill_points[n]] + sigma_abs_free[d,bill_points[n]] -
                  sqrt((L_full[d,person_points[n]] - B_int_free[d,bill_points[n]])^2)
              }) %>% plogis()
            }
            
          } else {
            pr_vote <- sapply(this_obs,function(n) {
              L_full[d,person_points[n]]*sigma_reg_free[d,bill_points[n]] - B_int_free[d,bill_points[n]]
            }) %>% plogis()
          }
          
          return(pr_vote)
        })
        
        }
      
      return(list(pr_vote=pr_vote_iter,pr_absence=pr_absence_iter,model_id=m,
                  item_point=i,
                  this_obs=this_obs))
      },mc.cores=use_cores)
      
    return(out_items)
      
    }) %>% unlist(recursive=FALSE)
    
   # need to flatten the list one level so it is at the item level, not the model level

    out_mods <- lapply(out_prs, function(m) {
      
      rep_func <- switch(as.character(m$model_id),
             `1`=.binary,
             `2`=.binary,
             `3`=.ordinal_ratingscale,
             `4`=.ordinal_ratingscale,
             `5`=.ordinal_ratingscale,
             `6`=.ordinal_ratingscale,
             #`5`=.ordinal_grm,
             #`6`=.ordinal_grm,
             `7`=.poisson,
             `8`=.poisson,
             `9`=.normal,
             `10`=.normal,
             `11`=.lognormal,
             `12`=.lognormal,
             `13`=.binary,
             `14`=.binary)
      
      this_obs <- m$this_obs

      if(m$model_id %in% c(3,4,5,6)) {
        
       # if(m$model_id %in% c(3,4)) {
       #   
       #   over_id <- object@score_data@n_cats_rat
       # } else {
       #   
       #   over_id <- object@score_data@n_cats_grm
       # }
       #   
       #   over_ordered <- lapply(1:length(over_id), function(i) {
       #     
       #     if(object@score_data@n_cats_rat[i]>1) {
       #       cuts <- object@score_data@n_cats_rat[i]
       #     } else if(object@score_data@n_cats_grm[i]>1) {
       #       
       #       cuts <- object@score_data@n_cats_grm[i]
       #       
       #     } else {
       #       return(NULL)
       #     }
        
             cuts <- unique(ordered_id[this_obs])

             outcome <- Y_int[this_obs]
             miss_val <- object@score_data@miss_val[1]

           if(m$model_id %in% c(3,4)) {
             cutpoints <- object@stan_samples$draws(paste0('steps_votes',cuts)) %>% as_draws_matrix()
             cutpoints <- cutpoints[these_draws,]
           } else if(m$model_id %in% c(5,6)) {
             cutpoints <- object@stan_samples$draws(paste0('steps_votes_grm',cuts,
                                                           '[',m$item_point,
                                                           ",",1:(cuts-1),"]")) %>% as_draws_matrix()
             cutpoints <- cutpoints[these_draws,]
           } 
             
             item_points <- bill_points[this_obs]
             
             out_predict <- rep_func(pr_absence=m$pr_absence,
                                     pr_vote=m$pr_vote,
                                     N=length(person_points[this_obs]),
                                     ordinal_outcomes=1:cuts,
                                     inflate=m$model_id %in% c(2,4,6,8,10,12,14),
                                     latent_space=m$model_id %in% c(13,14),
                                     time_points=time_points[this_obs],
                                     item_points=bill_points[this_obs],
                                     max_val=max_val,
                                     outcome=outcome,
                                     miss_val=miss_val,
                                     person_points=person_points[this_obs],
                                     sigma_sd=as_draws_matrix(object@stan_samples$draws('extra_sd'))[these_draws,],
                                     cutpoints=cutpoints,
                                     type=type,
                                     output=output)
             
             # set attributes to pass along sample info
             #attr(out_predict,'chain_order') <- attr(L_tp1,'chain_order')[these_draws]
             attr(out_predict,'this_sample') <- this_sample
              
             attr(out_predict,"data") <- list(person_id=person_points[this_obs],
                                                group_id=person_points[this_obs],
                                                item_id=bill_points[this_obs],
                                                time_id=time_points[this_obs],
                                                outcome=Y_int[this_obs],
                                              miss_val=miss_val,
                                                cutpoints=cutpoints)

             attr(out_predict,'model') <- m$model_id
             attr(out_predict,'item') <- m$item_point
             attr(out_predict,"order_id") <- cuts
             attr(out_predict,'this_obs') <- this_obs
             attr(out_predict,"output_type") <- "discrete"
             
             
             if(type=='predict') {
               class(out_predict) <- c('matrix','ppd')
             } else if(type=='log_lik') {
               class(out_predict) <- c('matrix','log_lik')
             } else if(type=="epred") {
               
               class(out_predict) <- c('matrix','ppd')
               
             }
         
      } else {
        
        if(m$model_id %in% c(1,2,3,4,5,6,7,8,13,14)) {
          outcome <- Y_int[this_obs]
          miss_val <- object@this_data$Y_int_miss
        } else {
          outcome <- Y_cont[this_obs]
          miss_val <- object@this_data$Y_cont_miss
        }
      
      out_predict <- rep_func(pr_absence=m$pr_absence,
                              pr_vote=m$pr_vote,
                              N=length(person_points[this_obs]),
                              ordinal_outcomes=length(unique(object@score_data@score_matrix$outcome[this_obs])),
                              inflate=m$model_id %in% c(2,4,6,8,10,12,14),
                              latent_space=m$model_id %in% c(13,14),
                              time_points=time_points[this_obs],
                              item_points=bill_points[this_obs],
                              max_val=max_val,
                              outcome=outcome,
                              miss_val=miss_val,
                              person_points=person_points[this_obs],
                              sigma_sd=as_draws_matrix(object@stan_samples$draws('extra_sd'))[these_draws,],
                              cutpoints=NULL,
                              type=type,
                              output=output)
      
      # set attributes to pass along sample info
      #attr(out_predict,'chain_order') <- attr(L_tp1,'chain_order')[these_draws]
      attr(out_predict,'this_sample') <- this_sample
      
      if(length(Y_int)>1) {
        attr(out_predict,"data") <- list(person_id=person_points[this_obs],
                                         group_id=person_points[this_obs],
                                         item_id=bill_points[this_obs],
                                         time_id=time_points[this_obs],
                                         miss_val=miss_val,
                                         outcome=outcome)
      } else {
        attr(out_predict,"data") <- list(person_id=person_points[this_obs],
                                         group_id=person_points[this_obs],
                                         item_id=bill_points[this_obs],
                                         time_id=time_points[this_obs],
                                         miss_val=miss_val,
                                         outcome=outcome)
      }
      
      attr(out_predict,'model') <- m$model_id
      attr(out_predict,"order_id") <- NA
      attr(out_predict,'item') <- m$item_point
      attr(out_predict,'this_obs') <- this_obs
      
      if(m$model_id %in% c(1,2,3,4,5,6,7,8,13,14)) {
        attr(out_predict,"output_type") <- "discrete"
      } else {
        attr(out_predict,"output_type") <- "continuous"
      }
      
      if(type %in% c('predict',"epred")) {
        class(out_predict) <- c('matrix','ppd')
      } else if(type=='log_lik') {
        class(out_predict) <- c('matrix','log_lik')
      }
      
      }
      
      
      return(out_predict)
      
  })
    
  #if(any(c(3,4,5,6) %in% unique(modelpoints))) {
  #  return(unlist(out_mods,recursive = F))
  #} else {
    #return(out_mods)
  #}
    
    class(out_mods) <- c(class(out_mods), "id_pred_obj")
    
    return(out_mods)
    
})

#' Plot Posterior Predictive Distribution for \code{idealstan} Objects
#' 
#' This function is the generic method for generating posterior distributions 
#' from a fitted \code{idealstan} model. Functions are documented in the 
#' actual method.
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
#' If you specify a value for \code{group} that is either a person ID or a group ID 
#' (depending on whether a person or group-level model was fit), then you can see the 
#' posterior distributions for those specific persons. Similarly, if an item ID is passed
#' to \code{item}, you can see how well the model predictions compare to the true values
#' for that specific item.
#' 
#' @param object A fitted \code{idealstan} object
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setGeneric('id_plot_ppc',signature='object',
           function(object,...) standardGeneric('id_plot_ppc'))

#' Plot Posterior Predictive Distribution for \code{idealstan} Objects
#' 
#' This function is the actual method for generating posterior distributions 
#' from a fitted \code{idealstan} model.
#' 
#' This function is a wrapper around \code{\link[bayesplot]{ppc_bars}},
#' \code{\link[bayesplot]{ppc_dens_overlay}} and 
#' \code{\link[bayesplot]{ppc_violin_grouped} that plots the posterior predictive distribution
#' derived from \code{\link{id_post_pred}} against the original data. 
#' Because \code{idealstan} allows for different distributions for each item,
#' this function can either produce one predictive distribution for all items 
#' (the default) or it can produce one distribution for each item 
#' (set \code{combine_item} to \code{FALSE}). The latter is helpful if you have mixed 
#' distributions between items, such as continuous and dichotomous values. 
#' You can also subset the 
#' posterior predictions over
#' legislators/persons or
#' bills/item sby specifying the ID of each in the original data as a character vector. 
#' Only persons or items can be specified,
#' not both.
#' 
#' If you specify a value for \code{group} that is either a person ID or a group ID 
#' (depending on whether a person or group-level model was fit), then you can see the 
#' posterior distributions for those specific persons. Similarly, if an item ID is passed
#' to \code{item}, you can see how well the model predictions compare to the true values
#' for that specific item.
#' 
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the \code{\link{id_post_pred}} function on a fitted idealstan object
#' @param combine_item Whether to combine all items together (TRUE) or create one plot for each item (FALSE)
#' @param group A character vector of the person or group IDs 
#' over which to subset the predictive distribution
#' @param type Whether to plot "continuous" or "discrete" responses 
#' @param prompt_plot Whether to expect a user prompt for each plot if multiple plots 
#' are produced (defaults to TRUE)
#' If NULL (default), will use the type specified in the data. 
#' However, if both continuous and discrete items are present, will
#' throw an error if NULL.
#' @param item A character vector of the item IDs
#' over which to subset the predictive distribution
#' @param which_mod If you are producing one plot aggregating data across multiple items and 
#' you have different item distributions,
#' then you need to specify the item type number to plot (see function documentation in
#' \code{\link{id_estimate}}).
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                 group=NULL,
                                                                    item=NULL,
                                                               combine_item=TRUE,
                                                               type=NULL,
                                                               only_observed=NULL,
                                                               which_mod=NULL,
                                                               prompt_plot=TRUE,
                                                               ...) {
  
  if(is.null(ppc_pred)) {
    stop("Please first use the function id_post_pred and pass the result to the ppc_pred argument to use this function.")
  }
  
  # subset by item
  
  if(!is.null(item)) {
    
    all_items <- levels(object@score_data@score_matrix$item_id)
    
    item <- which(all_items %in% as.character(item))
    if(length(item)==0) stop("You specified an item for subsetting that is not in the data used to fit the model.")
    ppc_pred <- ppc_pred[item]
    
  }
    
  
  if(combine_item) {
    
    # combine into one plot
    
        models <- sapply(ppc_pred, function(x) attr(x, "model"))
        output_types <- sapply(ppc_pred, function(x) attr(x, "output_type"))
        
        if(length(unique(output_types))>1 && is.null(type)) stop("Please specify type if there are both discrete and continuous items.")
        if(length(unique(models))>1 && is.null(which_mod)) stop("Please specify item model type if there is more than one type of item distribution.")
        
        if(!is.null(which_mod)) {
          
          ppc_pred <- ppc_pred[which(models==as.numeric(which_mod))]
          
          if(length(ppc_pred)==0) stop("Please specify an item model type that was passed to id_estimate. Item model type not found.")
          
        } else {
          
          which_mod <- unique(models)
          
        }
        
        if(!is.null(type)) {
          
          ppc_pred <- ppc_pred[which(output_types==as.character(type))]
          
          if(length(ppc_pred)==0) stop('Please specify a type of output that is either "continuous" or "discrete".')
          
        } else {
          
          type <- unique(output_types)
          
        }
        
        # only one type of output is possible
        outputs <- unique(sapply(ppc_pred, function(x) attr(x, "output")))
        
        samples <- lapply(ppc_pred, function(x) attr(x, "this_sample"))
        datas <- lapply(ppc_pred, function(x) attr(x, "data"))
        
        # combine datas/samples
        
        this_sample <- unlist(samples)
        all_data <- bind_rows(datas)
        
        # reshape if necessary
        
        if(dim(ppc_pred[[1]])[2]==length(unique(all_data$person_id))) {
          
          this_plot <- do.call(cbind, ppc_pred)
          
        } else {
          
          this_plot <- t(do.call(rbind, ppc_pred))
          
        }

        mod <- which_mod
        group_id <- all_data$group_id
        person_points <- all_data$person_id
        bill_points <- all_data$item_id
        time_points <- all_data$time_id
        
        # get missing values 
        
        miss_val <- unique(all_data$miss_val)
        
        y <- all_data$outcome
        
        # create grouping variable
        if(!is.null(group)) {
          if(object@use_groups) {
            group_var <- group_id
          } else {
            group_var <- person_points
          }
          grouped <- T
        } else {
          grouped <- F
        }
        
        if(outputs=='all') {
          y <- as.numeric(y)
          if(grouped) {
            
            out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=this_plot,
                                                    group=group_var,...)
            
          } else {
            out_plot <- bayesplot::ppc_bars(y=y,yrep=this_plot,...)
            
            
          }
        } else if(outputs=='observed') {
          
          # only show observed data for yrep

            to_remove <- y!=miss_val
          
          y <- y[to_remove]
          if(!is.null(group)) {
            group_var <- group_var[to_remove]
          }
          
          this_plot <- this_plot[,to_remove]
          
          if(type=='continuous') {
            
            #unbounded observed outcomes (i.e., continuous)
            if(grouped) {
              out_plot <- bayesplot::ppc_violin_grouped(y=y,yrep=this_plot,
                                                        group=group_var,
                                                        ...)
            } else {
              out_plot <- bayesplot::ppc_dens_overlay(y=y,yrep=this_plot,...)
            }
            
          } else if(type=='discrete') {
            
            if(grouped) {
              
              out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=this_plot,
                                                      group=group_var,...)
              
            } else {
              out_plot <- bayesplot::ppc_bars(y=y,yrep=this_plot,...)
            }
          }
          
          
        } else if(outputs=='missing') {
          
            y <- as.numeric(y==miss_val)
          
          if(grouped) {
            
            out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=this_plot,
                                                    group=group_var,...)
            
          } else {
            
            out_plot <- bayesplot::ppc_bars(y=y,yrep=this_plot,...)
            
          }
        }
        
        # start over with new posterior prediction object
        
        return(out_plot)
        
      } else {
        
        # likely ordered models that have multiple components
        
        lapply(ppc_pred, function(p) {
          
          all_data <- attr(p,"data")
          mod <- attr(p,"model")
          group_id <- all_data$group_id
          person_points <- all_data$person_id
          bill_points <- all_data$item_id
          time_points <- all_data$time_id
          miss_val <- unique(all_data$miss_val)
          
          # reshape if necessary
          
          if(dim(p)[2]!=length(unique(all_data$person_id))) {
            
            p <- t(p)
            
          }
          
          y <- all_data$outcome
          
          # only one model, create a standard plot
          
          this_sample <- attr(p,'this_sample')
          
          # create grouping variable
          if(!is.null(group)) {
            if(object@use_groups) {
              group_var <- group_id
            } else {
              group_var <- person_points
            }
            grouped <- T
          } else {
            grouped <- F
          }
          
          if(attr(p,'output')=='all') {
            y <- as.numeric(y)
            if(grouped) {
              
              out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=p,
                                                      group=group_var,...)
              
            } else {
              out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=p,...)
              
              
            }
          } else if(attr(this_plot,'output')=='observed' && attr(this_plot,'output_type')!='continuous') {
            # only show observed data for yrep

            to_remove <- y==miss_val
            
            y <- y[to_remove]
            if(!is.null(group)) {
              group_var <- group_var[to_remove]
            }
            y <- as.numeric(y)
            
            save_att <- attributes(p)
            save_att$dim <- NULL
            p <- p[,to_remove]
            save_att$dim <- dim(p)
            attributes(p) <- save_att
            
            if(attr(p,'output_type')=='continuous') {
              
              #unbounded observed outcomes (i.e., continuous)
              if(grouped) {
                out_plot <- bayesplot::ppc_violin_grouped(y=y,yrep=p,
                                                          group=group_var,
                                                          ...)
              } else {
                out_plot <- bayesplot::ppc_dens_overlay(y=y,yrep=p,...)
              }
              
            } else if(attr(p,'output_type')=='discrete') {
              
              if(grouped) {
                
                out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=p,
                                                        group=group_var,...)
                
              } else {
                out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=p,...)
              }
            }
            
            
          } else if(attr(p,'output')=='missing') {
            
              y <- as.numeric(y==miss_val)
            
            if(grouped) {
              
              out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=p,
                                                      group=group_var,...)
              
            } else {
              
              out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=p,...)
              
            }
          }
          
          out_plot <- out_plot + ggtitle(paste0("Item ",unique(all_data$item_id)))
          
          print(out_plot)
          
          if(prompt_plot) invisible(readline(prompt="Press [enter] to see next plot: "))
          
        })
        
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