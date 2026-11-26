# These functions are implemented for compatibility with the 
# rstantools package (and rstanarm)

#' Generic Method for Obtaining Posterior Predictive Distribution from Stan Objects
#' 
#' This function is a generic that is used to match the functions used with [bayesplot::ppc_bars()] to calculate
#' the posterior predictive distribution of the data given the model.
#' 
#' @param object A fitted `idealstan` object
#' @param ... All other parameters passed on to the underlying function.
#' @return `posterior_predict` methods should return a \eqn{D} by \eqn{N}
#'   matrix, where \eqn{D} is the number of draws from the posterior predictive
#'   distribution and \eqn{N} is the number of data points being predicted per
#'   draw.
#' @examples
#' \donttest{
#' sim <- id_sim_gen()
#' est <- id_estimate(sim, model_type=1, fixtype='vb_full',
#'                    use_method="pathfinder", nchains=2, ncores=2)
#' ppc <- id_post_pred(est, draws=50)
#' ll  <- id_post_pred(est, type='log_lik')
#' }
#' @return An object of class \code{id_pred_obj} (a list of matrices of posterior-predicted draws, one
#'   per model type). If \code{type="log_lik"}, a draws-by-observations \code{matrix} of class
#'   \code{log_lik} with a \code{chain_order} attribute for use with \code{\link{derive_chain}}.
#' @export
setGeneric('id_post_pred',signature='object',
           function(object,...) standardGeneric('id_post_pred'))


#' Posterior Prediction for `idealstan` objects
#' 
#' This function will draw from the posterior distribution, whether in terms of the outcome (prediction)
#' or to produce the log-likelihood values.  
#'  
#'  This function can also produce either distribution of the 
#'  outcomes (i.e., predictions) or the log-likelihood values of the posterior (set option 
#'  `type` to `'log_lik'`.
#'  For more information, see the package vignette How to Evaluate Models.
#'  
#'  You can then use functions such as 
#'  [id_plot_ppc()] to see how well the model does returning the correct number of categories
#'  in the score/vote matrix. 
#'  Also see `help("posterior_predict", package = "rstanarm")`
#'
#' @param object A fitted `idealstan` object
#' @param newdata Optional: pass a data frame that must have all of the predictors that
#' were given to the id_make function. Used to generate predictions from person or item
#' covariates on to items.
#' @param draws The number of draws to use from the total number of posterior draws (default is 100). Set to "all" to use all draws in the chains.
#' For reproducibility, you can also pass a vector of specific draws to use.
#' @param sample_scores In addition to reducing the number of posterior draws used to 
#' calculate the posterior predictive distribution, which will reduce computational overhead.
#' Only available for calculating predictive distributions, not log-likelihood values.
#' @param item_subset Whether to calculate marginal effects for only a subset of 
#' items. Should be item IDs that match the `item_id` column passed to the `id_make`
#'  function.
#' @param pred_outcome In the case of ordinal responses, the number of the category to
#' predict. Defaults to top category.
#' @param type Whether to produce posterior predictive values (`'predict'`, the default),
#' the posterior expected (average) values (`'epred'`),
#' or log-likelihood values (`'log_lik'`). See the How to Evaluate Models vignette for more info.
#' The expected value is the mean of the outcome: the probability of a 1 for binary 
#' models, the expected category (1 = lowest) for ordinal models, and the mean count, 
#' number of successes or value for the other models. For models with missing-data 
#' inflation it is multiplied by the probability that the outcome is observed, and the 
#' log-likelihood includes the probability of the outcome being observed or missing.
#' @param output If the model has an unbounded outcome (Poisson, continuous, etc.), then
#' specify whether to show the `'observed'` data (the default) or the binary 
#' output `'missing'` showing whether an observation was predicted as missing or not
#' @param covar What kind of covariates to include as part of the prediction -- either
#' "person" (the default) or "items" if you included predictors for item discriminations.
#' @param use_cores Number of cores to use for computing the predictions of the
#' items in parallel. Defaults to 1 (no parallelization).
#' The work runs in parallel through the \pkg{future} package: with more than one
#' core and no plan set, a \code{future::multisession} plan with that many
#' workers (separate R sessions, which work everywhere, including Windows,
#' RStudio and Positron). To use another backend, set it with
#' \code{future::plan()} before calling, for example
#' \code{future::plan(future::multicore, workers = 4)}; that plan is then used
#' whatever the number of cores.
#' @param use_chain ID of MCMC chain to use rather than all chains (the default). 
#' @param skip_cov Whether to skip adding in hierarchical person-level covariates. Defaults to FALSE.
#' @param ... Any other arguments passed on to posterior_predict (currently none available)
#' @examples
#' \donttest{
#' sim <- id_sim_gen()
#' est <- id_estimate(sim, model_type=1, fixtype='vb_full',
#'                    use_method="pathfinder", nchains=2, ncores=2)
#' # posterior predictive distribution (100 draws)
#' ppc <- id_post_pred(est, draws=50)
#' # log-likelihood matrix (for loo)
#' ll <- id_post_pred(est, type='log_lik')
#' }
#' @return An object of class \code{id_pred_obj} (a list of matrices of posterior-predicted draws, one
#'   per model type). If \code{type="log_lik"}, a draws-by-observations \code{matrix} of class
#'   \code{log_lik} with a \code{chain_order} attribute for use with \code{\link{derive_chain}}.
#' @export
setMethod('id_post_pred',signature(object='idealstan'),function(object,
                                                                newdata=NULL,
                                                                draws=100,
                                                                output='observed',
                                                                type='predict',
                                                                covar="person",
                                                                sample_scores=NULL,
                                                                item_subset=NULL,
                                                                pred_outcome=NULL,
                                                                use_cores=1,
                                                                use_chain=NULL,
                                                                skip_cov=FALSE,
                                                                ...) {
  
  # need to regenerate data if newdata is not NULL
  
  if(!is.null(newdata)) {
    
    # first run the data through id_make
    
    func_args <- object@score_data@func_args
    
    func_args$score_data <- newdata
    
    # now generate new data, plug it into object
    
    eval_data_args <- object@eval_data_args
    
    new_idealdata <- do.call(id_make, func_args)
    # Carry the compiled Stan model forward so id_model / .vb_fix can use it
    new_idealdata@stanmodel_map <- object@score_data@stanmodel_map
    eval_data_args$idealdata <- new_idealdata

    new_stan_data <- do.call(.make_stan_data,eval_data_args)
    
  } else {
    
    # recreate the data just to keep it similar syntax
    
    new_stan_data <- do.call(.make_stan_data,object@eval_data_args)
    
    
  }
  

  n_votes <- new_stan_data$stan_data$N
  
  if(object@use_method %in% c("laplace", "pathfinder")) use_chain <- 1

  if(is.null(use_chain)) {
    use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  }

  n_iters <- nrow(as_draws_matrix(as_draws_array(object@stan_samples$draws("L_full"))[,use_chain,]))
  
  
  if(!is.null(sample_scores) && type!='log_lik') {
    this_sample <- sample(1:n_votes,sample_scores)
  } else {
    this_sample <- 1:n_votes
  }
  
  if(type!='log_lik') {
    
    if(length(draws)==1) {
      
      if(draws=="all") {
        
        these_draws <- 1:n_iters
        
      } else {
        
        these_draws <- sample(1:n_iters,draws)
      }
      
    } else {
      
      these_draws <- draws
      draws <- length(draws)
      
    }
    
    
  } else {
    these_draws <- 1:n_iters
    draws <- n_iters
  }
    
    if(!is.null(item_subset)) {
      
      # need to get item parameters
      
      item_subset <- which(levels(new_stan_data$idealdata@score_data@score_matrix$item_id) %in% item_subset)
      
    }
  
  
  print(paste0('Processing posterior replications for ',n_votes,' scores using ',draws,
               ' posterior samples out of a total of ',n_iters, ' samples.'))

  if(length(new_stan_data$stan_data$Y_cont)>1) {
    Y_cont <- new_stan_data$stan_data$Y_cont[this_sample]
  } else {
    Y_cont <- new_stan_data$stan_data$Y_cont
  }
  
  if(length(object@this_data$Y_int)>1) {
    Y_int <- new_stan_data$stan_data$Y_int[this_sample]
    trials <- new_stan_data$stan_data$trials[this_sample]
  } else {
    Y_int <- new_stan_data$stan_data$Y_int
    trials <- new_stan_data$stan_data$trials
  }
  
  modelpoints <- new_stan_data$stan_data$mm[this_sample]
  #discrete <- object@score_data@score_matrix$discrete[this_sample]
  ordered_id <- new_stan_data$stan_data$order_cats_rat[this_sample]
  
  person_points <- new_stan_data$stan_data$ll[this_sample]

  bill_points <- new_stan_data$stan_data$bb[this_sample]
  time_points <- new_stan_data$stan_data$time[this_sample]
  
  # check for covariates & newdata
  
  if(length(new_stan_data$idealdata@person_cov_formula)>0) {
    
      
    legis_x <- new_stan_data$stan_data$legis_pred[this_sample,]
    
    cov_type <- "persons"
    
  } else if(length(new_stan_data$idealdata@item_cov_formula)>0) {
    
    cov_type <- "items"
    
  } else if(length(new_stan_data$idealdata@item_cov_miss_formula)>0) {
    
    cov_type <- "items_missing"
    
  } else {
    
    cov_type <- "none"
    
  }
  
  # linear predictors of the outcome and of missingness for each draw and
  # observation, summed over the dimensions as in the Stan likelihood

  lin <- .post_linear(object, new_stan_data$stan_data, obs=this_sample,
                      these_draws=these_draws, use_chain=use_chain, skip_cov=skip_cov)

  # loop over model IDs

  out_prs <- lapply(unique(modelpoints), function(m) {

    print(paste0("Now on model ",m))

    these_bills <- unique(bill_points[modelpoints==m])

    # check if any are in the item subset

    if(!is.null(item_subset)) {

      these_bills <- these_bills[these_bills %in% item_subset]

    }

    if(length(these_bills)>0) {

      lapply(these_bills, function(i) {

        this_obs <- which(bill_points==i)

        list(pr_vote=plogis(lin$obs[, this_obs, drop=FALSE]),
             pr_absence=plogis(lin$abs[, this_obs, drop=FALSE]),
             model_id=m,
             item_point=i,
             this_obs=this_obs)

      })

    }

  })

   # need to flatten the list one level so it is at the item level, not the model level

    # gather the inputs of each item here, so that a parallel worker only gets
    # its own item (see .id_lapply and .predict_item)

    # draws of item-specific parameters, from the same chains and draws as the
    # other parameters (draws x variables matrix)
    item_draws <- function(vars) {
      out <- as_draws_array(object@stan_samples$draws(vars))[,use_chain,] %>% as_draws_matrix()
      matrix(as.numeric(out[these_draws,]), nrow=length(these_draws))
    }

    score_ordered_id <- object@score_data@score_matrix$ordered_id

    item_inputs <- lapply(out_prs, function(m) {

      lapply(m, function(item) {

        this_obs <- item$this_obs
        mid <- item$model_id

        item$discrete <- mid %in% c(1,2,3,4,5,6,7,8,13,14,17,18)
        item$outcome <- if(item$discrete) Y_int[this_obs] else Y_cont[this_obs]
        item$miss_val <- if(item$discrete) new_stan_data$stan_data$y_int_miss else new_stan_data$stan_data$y_cont_miss
        item$ordinal_outcomes <- 1:unique(score_ordered_id[this_obs])
        item$person_points <- person_points[this_obs]
        item$time_points <- time_points[this_obs]
        item$bill_points <- bill_points[this_obs]
        item$trials <- if(mid %in% c(3,4,5,6)) NULL else trials[this_obs]
        item$cuts <- if(mid %in% c(3,4,5,6)) unique(ordered_id[this_obs]) else NA

        if(mid %in% c(3,4)) {
          cutpoints <- as_draws_array(object@stan_samples$draws(paste0('steps_votes',item$cuts)))[,use_chain,] %>% as_draws_matrix()
          item$cutpoints <- cutpoints[these_draws,]
        } else if(mid %in% c(5,6)) {
          cutpoints <- as_draws_array(object@stan_samples$draws(paste0('steps_votes_grm',item$cuts,
                                                                       '[',item$item_point,
                                                                       ",",1:(item$cuts-1),"]")))[,use_chain,] %>% as_draws_matrix()
          item$cutpoints <- cutpoints[these_draws,]
        } else if(mid %in% c(15,16)) {
          # each ordered beta item has its own cutpoints and phi
          this_ordbeta <- new_stan_data$stan_data$ordbeta_id[item$item_point]
          item$cutpoints <- item_draws(paste0("ordbeta_cut[",this_ordbeta,",",1:2,"]"))
          item$phi <- item_draws(paste0("phi[",this_ordbeta,"]"))[,1]
        } else if(mid %in% c(9,10,11,12)) {
          # each normal/log-normal item has its own residual SD
          this_sigma <- new_stan_data$stan_data$type_het_var[item$item_point]
          item$sigma_sd <- item_draws(paste0("extra_sd[",this_sigma,"]"))[,1]
        }

        item

      })

    })

    preds <- .id_lapply(unlist(item_inputs, recursive=FALSE), .predict_item,
                        pred_outcome=pred_outcome, type=type,
                        output=output, this_sample=this_sample, cores=use_cores)

    # back into a list of models, each a list of items
    out_mods <- unname(split(preds, factor(rep(seq_along(item_inputs), lengths(item_inputs)),
                                           levels=seq_along(item_inputs))))

    # add model ID
    
    out_mods <- lapply(1:length(out_mods), function(m) {
      
      attr(out_mods[[m]], "model") <- unique(modelpoints)[m]
      
      if(unique(modelpoints)[m] %in% c(1,2,3,4,5,6,7,8,13,14,17,18)) {
        attr(out_mods[[m]],"output_type") <- "discrete"
      } else {
        attr(out_mods[[m]],"output_type") <- "continuous"
      }
      
      out_mods[[m]]
      
    })
    
    
    # for log_lik, return a single combined matrix (draws x observations) with
    # chain_order attribute, matching what derive_chain and loo::loo expect
    if(type == 'log_lik') {
      all_item_mats <- unlist(out_mods, recursive = FALSE)
      ll_mat <- do.call(cbind, all_item_mats)
      n_chains <- length(use_chain)
      iters_per_chain <- nrow(ll_mat) / n_chains
      attr(ll_mat, 'chain_order') <- rep(use_chain, each = iters_per_chain)
      class(ll_mat) <- c('matrix', 'log_lik')
      return(ll_mat)
    }

    # this is now a list of lists with model ID at the top

    class(out_mods) <- c(class(out_mods), "id_pred_obj")

    return(out_mods)

})

#' Plot Posterior Predictive Distribution for `idealstan` Objects
#' 
#' This function is the generic method for generating posterior distributions 
#' from a fitted `idealstan` model. Functions are documented in the 
#' actual method.
#' 
#' This function is a wrapper around [bayesplot::ppc_bars()],
#' [bayesplot::ppc_dens_overlay()] and 
#' [bayesplot::ppc_violin_grouped()] that plots the posterior predictive distribution
#' derived from [id_post_pred()] against the original data. You can also subset the 
#' posterior predictions over
#' legislators/persons or
#' bills/item sby specifying the ID of each in the original data as a character vector. 
#' Only persons or items can be specified,
#' not both.
#' 
#' If you specify a value for `group` that is either a person ID or a group ID 
#' (depending on whether a person or group-level model was fit), then you can see the 
#' posterior distributions for those specific persons. Similarly, if an item ID is passed
#' to `item`, you can see how well the model predictions compare to the true values
#' for that specific item.
#' 
#' @param object A fitted `idealstan` object
#' @param ... Other arguments passed on to [bayesplot::ppc_bars()]
#' @examples
#' \donttest{
#' sim <- id_sim_gen()
#' est <- id_estimate(sim, model_type=1, fixtype='vb_full',
#'                    use_method="pathfinder", nchains=2, ncores=2)
#' ppc <- id_post_pred(est, draws=50)
#' id_plot_ppc(est, ppc_pred=ppc)
#' id_plot_ppc(est, ppc_pred=ppc, combine_item=FALSE, prompt_plot=FALSE)
#' }
#' @return A \code{ggplot2} object (from the \code{bayesplot} package) comparing observed and
#'   posterior-predicted outcome distributions.
#' @export
setGeneric('id_plot_ppc',signature='object',
           function(object,...) standardGeneric('id_plot_ppc'))

#' Plot Posterior Predictive Distribution for `idealstan` Objects
#' 
#' This function is the actual method for generating posterior distributions 
#' from a fitted `idealstan` model.
#' 
#' This function is a wrapper around [bayesplot::ppc_bars()],
#' [bayesplot::ppc_dens_overlay()] and 
#' [bayesplot::ppc_violin_grouped()] that plots the posterior predictive distribution
#' derived from [id_post_pred()] against the original data. 
#' Because `idealstan` allows for different distributions for each item,
#' this function can either produce one predictive distribution for all items 
#' (the default) or it can produce one distribution for each item 
#' (set `combine_item` to `FALSE`). The latter is helpful if you have mixed 
#' distributions between items, such as continuous and dichotomous values. 
#' You can also subset the 
#' posterior predictions over
#' legislators/persons or
#' bills/item sby specifying the ID of each in the original data as a character vector. 
#' Only persons or items can be specified,
#' not both.
#' 
#' If you specify a value for `group` that is either a person ID or a group ID 
#' (depending on whether a person or group-level model was fit), then you can see the 
#' posterior distributions for those specific persons. Similarly, if an item ID is passed
#' to `item`, you can see how well the model predictions compare to the true values
#' for that specific item.
#' 
#' @param object A fitted idealstan object
#' @param ppc_pred The output of the [id_post_pred()] function on a fitted idealstan object
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
#' [id_estimate()]).
#' @param observed_only If the outcome is discrete and has missing data inflation, 
#' set to TRUE to only see the observed responses in the plot or FALSE to see all of the
#' responses (missing data category will be the largest).
#' @param ... Other arguments passed on to [bayesplot::ppc_bars()]
#' @examples
#' \donttest{
#' sim <- id_sim_gen()
#' est <- id_estimate(sim, model_type=1, fixtype='vb_full',
#'                    use_method="pathfinder", nchains=2, ncores=2)
#' ppc <- id_post_pred(est, draws=50)
#' id_plot_ppc(est, ppc_pred=ppc)
#' # per-item plots
#' id_plot_ppc(est, ppc_pred=ppc, combine_item=FALSE, prompt_plot=FALSE)
#' }
#' @return A \code{ggplot2} object (from the \code{bayesplot} package) comparing observed and
#'   posterior-predicted outcome distributions.
#' @export
setMethod('id_plot_ppc',signature(object='idealstan'),function(object,
                                                                  ppc_pred=NULL,
                                                                 group=NULL,
                                                                    item=NULL,
                                                               combine_item=TRUE,
                                                               type=NULL,
                                                               which_mod=NULL,
                                                               prompt_plot=TRUE,
                                                               observed_only=FALSE,
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
        
        if(!is.null(which_mod) && (length(which_mod)>1 || !(which_mod %in% models))) {
          
          stop("Please specify only one model ID for the which_mod parameter that is in the models/item types in the idealstan object.")
        } else {
          
          if(length(unique(models))>1)
              stop("Please specify a model type to use (item type) as there are multiple models/outcomes in the idealstan object.")
          
          which_mod <- unique(models)
          
        }
        
        print(paste0("Predicting outcomes for model ID ",which_mod,"."))
        
        ppc_pred <- ppc_pred[[which(models==as.numeric(which_mod))]]
        
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
        
        # if(dim(ppc_pred[[1]])[2]==length(unique(all_data$person_id))) {
        #   
          this_plot <- do.call(cbind, ppc_pred)
          
        # } else {
        #   
        #   this_plot <- t(do.call(rbind, ppc_pred))
        #   
        # }

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
          grouped <- TRUE
        } else {
          grouped <- FALSE
        }
        
        if(outputs=='all' && !observed_only) {
          y <- as.numeric(y)
          if(grouped) {
            
            out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=this_plot,
                                                    group=group_var,...)
            
          } else {
            out_plot <- bayesplot::ppc_bars(y=y,yrep=this_plot,...)
            
            
          }
          
        } else if(observed_only || outputs=='observed') {
          
          # only show observed data for yrep
          
          if(!is.null(miss_val)) {
            
            to_remove <- y!=miss_val
            
            y <- y[to_remove]
            
            if(!is.null(group)) {
              group_var <- group_var[to_remove]
            }
            
            this_plot <- this_plot[,to_remove]
            
          }
          
          
          
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
            
            # subtract away the minimum as the prediction is always at 0
            
            y <- y - min(y, na.rm=TRUE)
          
          if(grouped) {
            
            out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=this_plot,
                                                    group=group_var,...)
            
          } else {
            
            out_plot <- bayesplot::ppc_bars(y=y,yrep=this_plot,...)
            
          }
        }
        
        # start over with new posterior prediction object
        
        return(out_plot)
        
        # END COMBINED ITEM
        
      } else {
        
        # BEGIN DISAGGREGATED ITEM PLOT
        
        # likely ordered models that have multiple components
        
        lapply(ppc_pred, function(p) {
          
          # loop over items
          
          lapply(p, function(psub) {
            
            all_data <- attr(psub,"data")
            mod <- attr(psub,"model")
            group_id <- all_data$group_id
            person_points <- all_data$person_id
            bill_points <- all_data$item_id
            time_points <- all_data$time_id
            miss_val <- unique(all_data$miss_val)
            
            # reshape if necessary
            
            if(dim(psub)[2]!=length(unique(all_data$person_id))) {
              
              psub <- t(psub)
              
            }
            
            y <- all_data$outcome
            
            # only one model, create a standard plot
            
            this_sample <- attr(psub,'this_sample')
            
            # create grouping variable
            if(!is.null(group)) {
              if(object@use_groups) {
                group_var <- group_id
              } else {
                group_var <- person_points
              }
              grouped <- TRUE
            } else {
              grouped <- FALSE
            }
            
            if(attr(psub,'output')=='all') {
              y <- as.numeric(y)
              if(grouped) {
                
                out_plot <- bayesplot::ppc_bars_grouped(y=as.numeric(factor(y)),yrep=psub,
                                                        group=group_var,...)
                
              } else {
                out_plot <- bayesplot::ppc_bars(y=as.numeric(factor(y)),yrep=psub,...)
                
                
              }
            } else if(attr(psub,'output')=='observed' && attr(psub,'output_type')!='continuous') {
              # only show observed data for yrep
              
              to_remove <- y==miss_val
              
              y <- y[to_remove]
              if(!is.null(group)) {
                group_var <- group_var[to_remove]
              }
              y <- as.numeric(y)
              
              save_att <- attributes(p)
              save_att$dim <- NULL
              psub <- psub[,to_remove]
              save_att$dim <- dim(psub)
              attributes(psub) <- save_att
              
                if(grouped) {
                  
                  out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=psub,
                                                          group=group_var,...)
                  
                } else {
                  out_plot <- bayesplot::ppc_bars(y=y,yrep=psub,...)
                }
              
            } else if(attr(psub,'output')=='observed') {
              
              y <- as.numeric(y)
              
              #unbounded observed outcomes (i.e., continuous)
              if(grouped) {
                out_plot <- bayesplot::ppc_violin_grouped(y=y,yrep=psub,
                                                          group=group_var,
                                                          ...)
              } else {
                out_plot <- bayesplot::ppc_dens_overlay(y=y,yrep=psub,...)
              }
              
            } else if(attr(psub,'output')=='missing') {
              
              y <- as.numeric(y==miss_val)
              
              if(grouped) {
                
                out_plot <- bayesplot::ppc_bars_grouped(y=y,yrep=psub,
                                                        group=group_var,...)
                
              } else {
                
                out_plot <- bayesplot::ppc_bars(y=y,yrep=psub,...)
                
              }
            }
            
            out_plot <- out_plot + ggtitle(paste0("Item ",unique(all_data$item_id)))
            
            print(out_plot)
            
            if(prompt_plot) invisible(readline(prompt="Press [enter] to see next plot: "))
            
            
          })
        
          
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
#' @param ll_matrix A log-likelihood matrix as produced by the [id_post_pred()]
#' function
#' @examples
#' \donttest{
#' sim <- id_sim_gen()
#' est <- id_estimate(sim, model_type=1, fixtype='vb_full',
#'                    use_method="pathfinder", nchains=2, ncores=2)
#' ll <- id_post_pred(est, type='log_lik')
#' chain_ids <- derive_chain(ll)
#' # use with loo
#' loo::loo(exp(ll), r_eff=loo::relative_eff(exp(ll), chain_id=chain_ids))
#' }
#' @return An integer vector of MCMC chain IDs extracted from the log-likelihood matrix, for use as the
#'   \code{chain_id} argument to \code{\link[loo]{relative_eff}}.
#' @export
derive_chain <- function(ll_matrix=NULL) {
  attr(ll_matrix,'chain_order')
}

#' Linear predictors of the outcome and of missingness for posterior draws
#'
#' Matches the Stan likelihood: for IRT models the sum over dimensions of
#' (discrimination + item covariate effect) x (ideal point + person covariate
#' effect), minus the item intercept; for latent-space models (13 and 14) the
#' person intercept plus the item intercept minus the distance between the person
#' and the item across the dimensions.
#'
#' @param stan_data Stan data for the observations to predict.
#' @param obs Indices of the observations in `stan_data`.
#' @param these_draws Rows of the (chain-subset) draws to use.
#' @param skip_cov Whether to leave out the person covariates.
#' @return A list with draws x observations matrices `obs` (outcome) and `abs`
#'   (missingness), on the logit scale.
#' @noRd
.post_linear <- function(object, stan_data, obs, these_draws, use_chain, skip_cov=FALSE) {

  p <- stan_data$ll[obs]
  b <- stan_data$bb[obs]
  tt <- stan_data$time[obs]
  ls <- stan_data$mm[obs] %in% c(13, 14)
  n_obs <- length(obs)

  # draws x columns matrix of one dimension's parameter, columns named as in dimension 1
  get <- function(v, d=1) {
    unclass(as_draws_matrix(.dim_draws(object, v, d, use_chain=use_chain)))[these_draws, , drop=FALSE]
  }

  # covariate effects on the observations: draws x observations
  cov_effect <- function(X, v, d) {
    if(is.null(X) || NCOL(X) == 0) return(0)
    t(as.matrix(X)[obs, , drop=FALSE] %*% t(get(v, d)))
  }

  B_int <- get("B_int_free")[, paste0("B_int_free[", b, "]"), drop=FALSE]
  A_int <- get("A_int_free")[, paste0("A_int_free[", b, "]"), drop=FALSE]

  lin_obs <- -B_int
  lin_abs <- -A_int
  dist_obs <- dist_abs <- matrix(0, length(these_draws), n_obs)

  for(d in seq_len(.num_dim(object))) {

    if(stan_data$`T` > 1) {
      L <- if(d == 1) {
        .get_varying(object, time_id=stan_data$time, person_id=stan_data$ll, use_chain=use_chain)
      } else {
        .get_varying(object, use_chain=use_chain, dim=d)
      }
      ideal <- unclass(L)[these_draws, paste0("L_tp1[", tt, ",", p, "]"), drop=FALSE]
    } else {
      ideal <- get("L_full", d)[, paste0("L_full[", p, "]"), drop=FALSE]
    }

    if(!skip_cov) ideal <- ideal + cov_effect(stan_data$legis_pred, "legis_x", d)

    reg <- get("sigma_reg_free", d)[, paste0("sigma_reg_free[", b, "]"), drop=FALSE] +
      cov_effect(stan_data$srx_pred, "sigma_reg_x", d)
    abs_d <- get("sigma_abs_free", d)[, paste0("sigma_abs_free[", b, "]"), drop=FALSE] +
      cov_effect(stan_data$sax_pred, "sigma_abs_x", d)

    lin_obs[, !ls] <- lin_obs[, !ls] + reg[, !ls] * ideal[, !ls]
    lin_abs[, !ls] <- lin_abs[, !ls] + abs_d[, !ls] * ideal[, !ls]
    dist_obs[, ls] <- dist_obs[, ls] + (ideal[, ls] - reg[, ls])^2
    dist_abs[, ls] <- dist_abs[, ls] + (ideal[, ls] - abs_d[, ls])^2

  }

  if(any(ls)) {
    ls_int <- get("ls_int")[, paste0("ls_int[", p[ls], "]"), drop=FALSE]
    ls_int_abs <- get("ls_int_abs")[, paste0("ls_int_abs[", p[ls], "]"), drop=FALSE]
    lin_obs[, ls] <- ls_int + B_int[, ls] - sqrt(dist_obs[, ls])
    lin_abs[, ls] <- ls_int_abs + A_int[, ls] - sqrt(dist_abs[, ls])
  }

  list(obs=lin_obs, abs=lin_abs)

}
