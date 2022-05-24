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
  

  n_iters <- nrow(as_draws_matrix(object@stan_samples$draws("L_full")))


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

  if(length(object@score_data@Y_cont)>1) {
    Y_cont <- object@score_data@Y_cont[this_sample]
  } else {
    Y_cont <- object@score_data@Y_cont
  }
  
  if(length(object@score_data@Y_int)>1) {
    Y_int <- object@score_data@Y_int[this_sample]
  } else {
    Y_int <- object@score_data@Y_int
  }
  
  modelpoints <- object@score_data@score_matrix$model_id[this_sample]
  discrete <- object@score_data@score_matrix$discrete[this_sample]
  ordered_id <- object@score_data@score_matrix$ordered_id[this_sample]
  
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  
  remove_nas_cont <- !is.na(Y_cont)
  remove_nas_int <- !is.na(Y_int)
  
  if(length(Y_cont)>1 && length(Y_int)>1) {
    
    remove_nas <- remove_nas_int & remove_nas_cont
    
  } else if(length(Y_cont)>1) {
    remove_nas <- remove_nas_cont
  } else {
    remove_nas <- remove_nas_int
  }
  
  if(length(Y_cont)>1) {
    Y_cont <- Y_cont[remove_nas_cont]
    N_cont <- length(Y_cont)
  }
  
  if(length(Y_int)>1) {
    Y_int <- Y_int[remove_nas_int]
    N_int <- length(Y_int)
  }
  
  if(object@use_groups) {
    person_points <- as.numeric(object@score_data@score_matrix$group_id)[this_sample]
  } else {
    person_points <- as.numeric(object@score_data@score_matrix$person_id)[this_sample]
  }

  bill_points <- as.numeric(object@score_data@score_matrix$item_id)[this_sample]
  time_points <- as.numeric(factor(object@score_data@score_matrix$time_id))[this_sample]
  
  bill_points <- bill_points[remove_nas]
  time_points <- time_points[remove_nas]
  modelpoints <- modelpoints[remove_nas]
  ordered_id <- ordered_id[remove_nas]
  discrete <- discrete[remove_nas]
  person_points <- person_points[remove_nas]
  
  # we can do the initial processing here
  
  # loop over posterior iterations
  if(length(unique(object@score_data@score_matrix$time_id))>1) {
    L_tp1 <- .get_varying(object)
  }
  L_full <- object@stan_samples$draws('L_full') %>% as_draws_matrix()
  A_int_free <- object@stan_samples$draws('A_int_free') %>% as_draws_matrix()
  B_int_free <- object@stan_samples$draws('B_int_free') %>% as_draws_matrix()
  sigma_abs_free <- object@stan_samples$draws('sigma_abs_free') %>% as_draws_matrix()
  sigma_reg_free <- object@stan_samples$draws('sigma_reg_free') %>% as_draws_matrix()

  # loop over model IDs
  
  out_prs <- lapply(unique(modelpoints), function(m) {
    
    latent_space <- m %in% c(13,14)
    
    inflate <- m %in% c(2,4,6,8,10,12,14)
    
    print(paste0("Now on model ",m))
    
    if(length(unique(object@score_data@score_matrix$time_id))>1) {
      
      pr_absence_iter <- sapply(these_draws, function(d) {
        if(latent_space) {
          # use latent-space formulation for likelihood
          pr_absence <- sapply(which(modelpoints==m),function(n) {
            -sqrt((L_tp1[d,time_points[n],person_points[n]] - A_int_free[d,bill_points[n]])^2)
          }) %>% plogis()
        } else {
          # use IRT formulation for likelihood
          pr_absence <- sapply(which(modelpoints==m),function(n) {
            L_tp1[d,time_points[n],person_points[n]]*sigma_abs_free[d,bill_points[n]] - A_int_free[d,bill_points[n]]
          }) %>% plogis()
          
        }
        return(pr_absence)
      })
      
      pr_vote_iter <- sapply(these_draws, function(d) {
        if(latent_space) {
          if(inflate) {
            pr_vote <- sapply(which(modelpoints==m),function(n) {
              -sqrt((L_tp1[d,time_points[n],person_points[n]] - B_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          } else {
            # latent space non-inflated formulation is different
            pr_vote <- sapply(which(modelpoints==m),function(n) {
              sigma_reg_free[d,bill_points[n]] + sigma_abs_free[d,bill_points[n]] -
                sqrt((L_tp1[d,time_points[n],person_points[n]] - B_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          }
          
        } else {
          pr_vote <- sapply(which(modelpoints==m),function(n) {
            L_tp1[d,time_points[n],person_points[n]]*sigma_reg_free[d,bill_points[n]] - B_int_free[d,bill_points[n]]
          }) %>% plogis()
        }
        
        return(pr_vote)
      })
    } else {
      
      pr_absence_iter <- sapply(these_draws, function(d) {
        if(latent_space) {
          # use latent-space formulation for likelihood
          pr_absence <- sapply(which(modelpoints==m),function(n) {
            -sqrt((L_full[d,person_points[n]] - A_int_free[d,bill_points[n]])^2)
          }) %>% plogis()
        } else {
          # use IRT formulation for likelihood
          pr_absence <- sapply(which(modelpoints==m),function(n) {
            L_full[d,person_points[n]]*sigma_abs_free[d,bill_points[n]] - A_int_free[d,bill_points[n]]
          }) %>% plogis()
          
        }
        return(pr_absence)
      })
      
      pr_vote_iter <- sapply(these_draws, function(d) {
        if(latent_space) {
          if(inflate) {
            pr_vote <- sapply(which(modelpoints==m),function(n) {
              -sqrt((L_full[d,person_points[n]] - B_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          } else {
            # latent space non-inflated formulation is different
            pr_vote <- sapply(which(modelpoints==m),function(n) {
              sigma_reg_free[d,bill_points[n]] + sigma_abs_free[d,bill_points[n]] -
                sqrt((L_full[d,person_points[n]] - B_int_free[d,bill_points[n]])^2)
            }) %>% plogis()
          }
          
        } else {
          pr_vote <- sapply(which(modelpoints==m),function(n) {
            L_full[d,person_points[n]]*sigma_reg_free[d,bill_points[n]] - B_int_free[d,bill_points[n]]
          }) %>% plogis()
        }
        
        return(pr_vote)
      })
      
    }
    
    return(list(pr_vote=pr_vote_iter,pr_absence=pr_absence_iter,model_id=m))
  })

    out_mods <- lapply(out_prs, function(m) {
      
      rep_func <- switch(as.character(m$model_id),
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

      if(m$model_id %in% c(3,4,5,6)) {
        
       if(m$model_id %in% c(3,4)) {
         
         over_id <- object@score_data@n_cats_rat
       } else {
         
         over_id <- object@score_data@n_cats_grm
       }
         
         over_ordered <- lapply(1:length(over_id), function(i) {
           
           if(object@score_data@n_cats_rat[i]>1) {
             cuts <- object@score_data@n_cats_rat[i]
           } else {
             return(NULL)
           }

             outcome <- Y_int[modelpoints==m$model_id && ordered_id==cuts]
             miss_val <- object@score_data@miss_val[1]
           
           if(m$model_id %in% c(3,4)) {
             cutpoints <- object@stan_samples$draws(paste0('steps_votes',cuts)) %>% as_draws_matrix()
             cutpoints <- cutpoints[these_draws,]
           } else if(m$model_id %in% c(5,6)) {
             cutpoints <- object@stan_samples$draws(paste0('steps_votes_grm',cuts)) %>% as_draws_matrix()
             cutpoints <- cutpoints[these_draws,]
           } 
             
             out_predict <- rep_func(pr_absence=m$pr_absence,
                                     pr_vote=m$pr_vote,
                                     N=length(person_points[modelpoints==m$model_id]),
                                     ordinal_outcomes=length(unique(object@score_data@score_matrix$outcome[modelpoints==m$model_id])),
                                     inflate=m$model_id %in% c(2,4,6,8,10,12,14),
                                     latent_space=m$model_id %in% c(13,14),
                                     time_points=time_points[modelpoints==m$model_id],
                                     item_points=bill_points[modelpoints==m$model_id],
                                     max_val=max_val,
                                     outcome=outcome,
                                     miss_val=miss_val,
                                     person_points=person_points[modelpoints==m$model_id],
                                     sigma_sd=as_draws_matrix(object@stan_samples$draws('extra_sd'))[these_draws,],
                                     cutpoints=cutpoints,
                                     type=type,
                                     output=output)
             
             # set attributes to pass along sample info
             #attr(out_predict,'chain_order') <- attr(L_tp1,'chain_order')[these_draws]
             attr(out_predict,'this_sample') <- this_sample
              
             attr(out_predict,"data") <- list(person_id=person_points[modelpoints==m$model_id],
                                                group_id=person_points[modelpoints==m$model_id],
                                                item_id=bill_points[modelpoints==m$model_id],
                                                time_id=time_points[modelpoints==m$model_id],
                                                Y_int=Y_int[modelpoints==m$model_id],
                                                cutpoints=cutpoints,
                                                Y_cont=Y_cont[modelpoints==m$model_id])

             attr(out_predict,'model') <- m$model_id
             attr(out_predict,"order_id") <- cuts
             attr(out_predict,"output_type") <- "discrete"
             
             
             if(type=='predict') {
               class(out_predict) <- c('matrix','ppd')
             } else if(type=='log_lik') {
               class(out_predict) <- c('matrix','log_lik')
             }
             
             return(out_predict)
         })
         
         # remove NULLs
         
         over_ordered <- Filter(Negate(is.null), over_ordered)
         
         # return embedded list, flatten later
         
         return(over_ordered)
         
      } else {
        
        if(m$model_id %in% c(1,2,3,4,5,6,7,8,13,14)) {
          outcome <- Y_int[modelpoints==m$model_id]
          miss_val <- object@score_data@miss_val[1]
        } else {
          outcome <- Y_cont[modelpoints==m$model_id]
          miss_val <- object@score_data@miss_val[2]
        }
        
        browser()
      
      out_predict <- rep_func(pr_absence=m$pr_absence,
                              pr_vote=m$pr_vote,
                              N=length(person_points[modelpoints==m$model_id]),
                              ordinal_outcomes=length(unique(object@score_data@score_matrix$outcome[modelpoints==m$model_id])),
                              inflate=m$model_id %in% c(2,4,6,8,10,12,14),
                              latent_space=m$model_id %in% c(13,14),
                              time_points=time_points[modelpoints==m$model_id],
                              item_points=bill_points[modelpoints==m$model_id],
                              max_val=max_val,
                              outcome=outcome,
                              miss_val=miss_val,
                              person_points=person_points[modelpoints==m$model_id],
                              sigma_sd=as_draws_matrix(object@stan_samples$draws('extra_sd'))[these_draws,],
                              cutpoints=NULL,
                              type=type,
                              output=output)
      
      # set attributes to pass along sample info
      #attr(out_predict,'chain_order') <- attr(L_tp1,'chain_order')[these_draws]
      attr(out_predict,'this_sample') <- this_sample
      
      if(length(Y_int)>1) {
        attr(out_predict,"data") <- list(person_id=person_points[modelpoints==m$model_id],
                                         group_id=person_points[modelpoints==m$model_id],
                                         item_id=bill_points[modelpoints==m$model_id],
                                         time_id=time_points[modelpoints==m$model_id],
                                         Y_int=Y_int[modelpoints==m$model_id],
                                         Y_cont=Y_cont[modelpoints==m$model_id])
      } else {
        attr(out_predict,"data") <- list(person_id=person_points[modelpoints==m$model_id],
                                         group_id=person_points[modelpoints==m$model_id],
                                         item_id=bill_points[modelpoints==m$model_id],
                                         time_id=time_points[modelpoints==m$model_id],
                                         Y_cont=Y_cont[modelpoints==m$model_id],
                                         Y_int=Y_int[modelpoints==m$model_id])
      }
      
      attr(out_predict,'model') <- m$model_id
      attr(out_predict,"order_id") <- NULL
      
      if(m$model_id %in% c(1,2,3,4,5,6,7,8,13,14)) {
        attr(out_predict,"output_type") <- "discrete"
      } else {
        attr(out_predict,"output_type") <- "continuous"
      }
      
      if(type=='predict') {
        class(out_predict) <- c('matrix','ppd')
      } else if(type=='log_lik') {
        class(out_predict) <- c('matrix','log_lik')
      }
      
      return(out_predict)
      
      }
      
  })
    
  #if(any(c(3,4,5,6) %in% unique(modelpoints))) {
  #  return(unlist(out_mods,recursive = F))
  #} else {
    #return(out_mods)
  #}
    
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
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the \code{\link{id_post_pred}} function on a fitted idealstan object
#' @param group A character vector of the person or group IDs 
#' over which to subset the predictive distribution
#' @param ... Other arguments passed on to \code{\link[bayesplot]{ppc_bars}}
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                 group=NULL,
                                                                    item=NULL,...) {
  
  if(is.null(ppc_pred)) {
    stop("Please first use the function id_post_pred and pass the result to the ppc_pred argument to use this function.")
  }
  
  if(length(ppc_pred)==1) {
    
    this_plot <- ppc_pred[[1]]
    
    all_data <- attr(this_plot,"data")
    
    group_id <- all_data$group_id
    person_points <- all_data$person_id
    bill_points <- all_data$item_id
    time_points <- all_data$time_id
    
    if(length(all_data$Y_int)>1) {
      y <- all_data$Y_int
    } else {
      y <- all_data$Y_cont
    }
    
    # only one model, create a standard plot
    
    this_sample <- attr(this_plot,'this_sample')
    
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
  
    if(attr(this_plot,'output')=='all') {
      y <- as.numeric(y)
      if(grouped) {
        
       out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                    group=group_var,...)
        
      } else {
        out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
        
        
      }
    } else if(attr(this_plot,'output')=='observed') {
      # only show observed data for yrep
      
      if(length(all_data$Y_int)>1) {
        to_remove <- !(y==object@score_data@miss_val[1])
      } else {
        to_remove <- !(y==object@score_data@miss_val[2])
      }
      
      y <- y[to_remove]
      if(!is.null(group)) {
        group_var <- group_var[to_remove]
      }
      y <- as.numeric(y)

      save_att <- attributes(this_plot)
      save_att$dim <- NULL
      this_plot <- this_plot[,to_remove]
      save_att$dim <- dim(this_plot)
      attributes(this_plot) <- save_att

      if(attr(this_plot,'output_type')=='continuous') {
        
        #unbounded observed outcomes (i.e., continuous)
        if(grouped) {
          out_plot <- bayesplot::ppc_violin_grouped(y=y,yrep=this_plot,
                                        group=group_var,
                                        ...)
        } else {
          out_plot <- bayesplot::ppc_dens_overlay(y=y,yrep=this_plot,...)
        }
        
      } else if(attr(this_plot,'output_type')=='discrete') {
        
        if(grouped) {
          
          out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                      group=group_var,...)
          
        } else {
          out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
        }
      }
      
      
    } else if(attr(this_plot,'output')=='missing') {
      
      if(length(all_data$Y_int)>1) {
        y <- as.numeric(y==object@score_data@miss_val[1])
      } else {
        y <- as.numeric(y==object@score_data@miss_val[2])
      }

      if(grouped) {
        
        out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                    group=group_var,...)
        
      } else {
        
        out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
        
      }
    }
    
    print(out_plot)
    
    return(invisible(out_plot))
    
  } else {
    
    # many models, loop over plots
    
    all_plots <- lapply(ppc_pred, function (this_plot) {
      browser()
      all_data <- attr(this_plot,"data")
      mod <- attr(this_plot,"model")
      group_id <- all_data$group_id
      person_points <- all_data$person_id
      bill_points <- all_data$item_id
      time_points <- all_data$time_id
      
      if(mod %in% c(1,2,3,4,5,6,7,8,13,14)) {
        y <- all_data$Y_int
      } else {
        y <- all_data$Y_cont
      }
      
      # only one model, create a standard plot
      
      this_sample <- attr(this_plot,'this_sample')
      
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
      
      if(attr(this_plot,'output')=='all') {
        y <- as.numeric(y)
        if(grouped) {
          
          out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                                  group=group_var,...)
          
        } else {
          out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
          
          
        }
      } else if(attr(this_plot,'output')=='observed') {
        # only show observed data for yrep
        if(length(all_data$Y_int>1)) {
          to_remove <- y==object@score_data@miss_val[1]
        } else {
          to_remove <- y==object@score_data@miss_val[2]
        }
        
        y <- y[to_remove]
        if(!is.null(group)) {
          group_var <- group_var[to_remove]
        }
        y <- as.numeric(y)
        
        save_att <- attributes(this_plot)
        save_att$dim <- NULL
        this_plot <- this_plot[,to_remove]
        save_att$dim <- dim(this_plot)
        attributes(this_plot) <- save_att
        
        if(attr(this_plot,'output_type')=='continuous') {
          
          #unbounded observed outcomes (i.e., continuous)
          if(grouped) {
            out_plot <- bayesplot::ppc_violin_grouped(y=y,yrep=this_plot,
                                                      group=group_var,
                                                      ...)
          } else {
            out_plot <- bayesplot::ppc_dens_overlay(y=y,yrep=this_plot,...)
          }
          
        } else if(attr(this_plot,'output_type')=='discrete') {
          
          if(grouped) {
            
            out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                                    group=group_var,...)
            
          } else {
            out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
          }
        }
        
        
      } else if(attr(this_plot,'output')=='missing') {
        
        if(length(all_data$Y_int>1)) {
          y <- as.numeric(y==object@score_data@miss_val[1])
        } else {
          y <- as.numeric(y==object@score_data@miss_val[2])
        }
        
        if(grouped) {
          
          out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=this_plot,
                                                  group=group_var,...)
          
        } else {
          
          out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=this_plot,...)
          
        }
      }
      
      print(out_plot)
      
      invisible(readline(prompt="Press [enter] to see next plot"))
      
    })
    
  }
  
  return(invisible(all_plots))
  
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