#' Data and Identification for \code{id_estimate}
#' 
#' \code{idealdata} objects contain the relevant legislator/bill (person/item) matrix of data along with slots containing information
#' about the kind of identification used in the estimation.
#' 
#' @seealso \code{\link{id_make}} to create an \code{idealdata} object suitable for estimation with \code{id_estimate}.
#' @export
setClass('idealdata',
         slots=list(score_matrix='ANY',
                    person_data='data.frame',
                    group_vals='ANY',
                    group_varying='logical',
                    person_vals='ANY',
                    item_data='data.frame',
                    person_cov='character',
                    item_cov='character',
                    item_cov_miss='character',
                    person_cov_formula="formula",
                    item_cov_formula="formula",
                    item_cov_miss_formula="formula",
                    time='ANY',
                    exog_data='vector',
                    time_vals='vector',
                    vote_labels='ANY',
                    vote_count='integer',
                    miss_val='ANY',
                    restrict_count='numeric',
                    restrict_data='list',
                    stanmodel='ANY',
                    stanmodel_map="ANY",
                    #stanmodel_gpu='ANY',
                    n_cats_rat="ANY",
                    n_cats_grm="ANY",
                    order_cats_rat="ANY",
                    order_cats_grm="ANY",
                    param_fix='numeric',
                    constraint_type='character',
                    restrict_vals='ANY',
                    subset_group='character',
                    subset_person='character',
                    to_sample='numeric',
                    unrestricted='matrix',
                    restrict_num_high='numeric',
                    restrict_num_low='numeric',
                    restrict_ind_high='ANY',
                    restrict_ind_low='ANY',
                    vote_int='numeric',
                    simul_data='list',
                    simulation='logical',
                    diff='numeric',
                    diff_high='numeric',
                    restrict_var='logical',
                    restrict_var_high='numeric',
                    restrict_mean_val='numeric',
                    restrict_mean_ind='numeric',
                    restrict_mean='logical',
                    person_start='numeric',
                    Y_int="ANY",
                    Y_cont="ANY",
                    func_args="list"))


#' Results of \code{\link{id_estimate}} function
#' 
#' The \code{idealstan} objects store the results of estimations carried out by the \code{\link{id_estimate}} function. 
#' These objects include the full results of Bayesian sampling performed by the \code{\link[rstan]{stan}} function in the \pkg{rstan}
#' package.
#' @export
setClass('idealstan',
         slots=list(score_data='idealdata',
                    to_fix='list',
                    summary="ANY",
                    model_type='numeric',
                    time_proc='numeric',
                    model_code='character',
                    test_model_code='character',
                    map_over_id="character",
                    time_fix_sd="numeric",
                    diagnostics="ANY",
                    time_varying="ANY",
                    restrict_var="logical",
                    stan_samples='ANY',
                    keep_param='ANY',
                    use_vb='logical',
                    orig_order="ANY",
                    use_groups='logical',
                    this_data="ANY",
                    simulation='logical',
                    time_center_cutoff="numeric",
                    remove_nas="ANY",
                    eval_data_args="list"))

setGeneric('subset_ideal',signature='object',
           function(object,...) standardGeneric('subset_ideal'))

setMethod('subset_ideal',signature(object='idealdata'),
          function(object,use_subset=FALSE,sample_it=FALSE,subset_group=NULL,subset_person=NULL,sample_size=20) {
            
            
            # Functions for subsetting data and sampling
            
            x <- object@score_matrix
            parliament <- object@person_data
            
            if(use_subset==TRUE & !is.null(subset_group)) {
              if(!all(subset_group %in% parliament$group)) stop('The specified parliament bloc/party must be in the list of blocs/parties in the legislature data.')
              x <- x[parliament$group %in% subset_group,]
              
              object@subset_group <- subset_group
            } 
            if(use_subset==TRUE & !is.null(subset_person)) {
              if(!all(subset_person %in% parliament$person.names[parliament$group%in% subset_group])) {
                stop('The legislators to subset must be members of the subsetted bloc as well.')
              }
              x <- x[parliament$person.names %in% subset_person,]
              object@subset_person <- subset_person
            }
            
            if(sample_it==TRUE) {
              object@to_sample <- sample(1:nrow(x),sample_size)
              x <- x[object@to_sample,]
            }
            object@score_matrix <- x
            
            return(object)
          })

setGeneric('clean_bills',signature='object',
           function(object,...) standardGeneric('clean_bills'))

setMethod('clean_bills',signature(object='idealdata'),
          function(object) {
            x <- object@score_matrix
            
            if(grepl('absence_inflate|ordinal',x@model_type)) {
              select_cols <- apply(x,2, {
                if(length(table(x))<(vote_count-1)) {
                  FALSE
                } else {
                  TRUE
                }
              })
              x <- x[,select_cols]
            } else {
              select_cols <- apply(x,2, {
                if(length(table(x))<(vote_count)) {
                  FALSE
                } else {
                  TRUE
                }
              })
              x <- x[,select_cols]
            }
            object@score_matrix <- x
            return(object)
          })

setGeneric('sample_model',signature='object',
           function(object,...) standardGeneric('sample_model'))

setMethod('sample_model',signature(object='idealdata'),
          function(object,nchains=4,niters=2000,warmup=floor(niters/2),ncores=NULL,
                   to_use=to_use,this_data=this_data,use_vb=FALSE,within_chain=NULL,
                   keep_param=NULL,
                   save_files=NULL,
                   init_pathfinder=TRUE,
                   ...) {
            
            # need init values for pathfinder & other algos that work
            
            init_vals <- lapply(1:nchains,.init_stan,
                                this_data=this_data)

            init_vals_orig <- lapply(1:nchains,.init_stan,
                                     this_data=this_data)
            
            if(init_pathfinder) {
              
              # try pathfinder first, if that fails try laplace
              # turn off debug mode
              
              print("Running pathfinder to find starting values")
              
              debug_orig <- this_data$debug_mode
              
              this_data$debug_mode <- FALSE
              
              init_vals <- try(object@stanmodel_map$pathfinder(data=this_data,
                                          refresh=this_data$id_refresh,num_threads=ncores,
                                          num_paths=1,
                                          single_path_draws = 1000,history_size=25,
                                          init=init_vals_orig[1],psis_resample=FALSE))
              
              # init_vals <- try(object@stanmodel_map$laplace(data=this_data,
              #                                               refresh=0,threads=ncores,
              #                                               draws=1000,
              #                                               init=init_vals_orig[1]))

              # if fitting fails, we won't get variance in the draws
              
              c1 <- init_vals$draws()
              
              if(is.null(c1) || sd(init_vals$draws()[,1])==0) {
                
                print("Pathfinder failed; attempting without PSIS resampling.")
                
                init_vals <- try(object@stanmodel_map$laplace(data=this_data,
                                                          refresh=0,threads=ncores,
                                                          draws=1000,
                                                          init=init_vals_orig[1]))
                
              }
              
              this_data$debug_mode <- debug_orig
              
              # manually extract init values and check for good inits
              # return inits as list
              
              draws_init <- process_init_pathfinder(init_vals,num_procs=nchains)

              # check for very high/low for discrimination parameters
              # that might lead to overflow/underflow/log(0) errors
              # on init

              draws_init <- lapply(draws_init, function(td) {

                if(any(td$sigma_reg_free>0.9999)) {

                  td$sigma_reg_free[td$sigma_reg_free> 0.9999] <- td$sigma_reg_free[td$sigma_reg_free > 0.9999] - 0.001

                }

                if(any(td$sigma_reg_free < -0.9999)) {

                  td$sigma_reg_free[td$sigma_reg_free < -0.9999] <- td$sigma_reg_free[td$sigma_reg_free < -0.9999] + 0.001

                }
                
                if(any("sigma_abs_free") %in% names(td)) {
                  
                  if(any(td$sigma_abs_free>0.9999)) {
                    
                    td$sigma_reg_free[td$sigma_reg_free> 0.9999] <- td$sigma_reg_free[td$sigma_reg_free > 0.9999] - 0.001
                    
                  }
                  
                  if(any(td$sigma_abs_free < -0.9999)) {
                    
                    td$sigma_reg_free[td$sigma_reg_free < -0.9999] <- td$sigma_reg_free[td$sigma_reg_free < -0.9999] + 0.001
                    
                  }
                  
                  
                }

                return(td)
              })
              
              if(this_data$debug_mode) {
                
                saveRDS(draws_init, paste0("~/draws_init_",as.numeric(Sys.time()),".rds"))
                
              }
              
              # if it still doesn't work, do random inits
              
              c1 <- try(init_vals$draws())
              
              if(is.null(c1) || 'try-error' %in% class(c1) || sd(init_vals$draws()[,1])==0) {
                
                print("Both pathfinder and laplace algorithms failed to find starting values. Doing random inits.")
                
                init_vals <- lapply(1:nchains,.init_stan,
                                    this_data=this_data)
                
              } else {

                init_vals <- draws_init

              }
              
            }
            
            if(!is.null(keep_param)) {
              
              # check for logical vectors
              
              check_type <- sapply(keep_param, is.logical)
              
              stopifnot("Please only use TRUE/FALSE values for keep parameter option"=check_type)
              
              keep_vars <- "lp__"
              
              if(!is.null(keep_param$person_vary)) {
                
                if(keep_param$person_vary) {
                  
                  if(this_data$time_proc==2) {
                    
                    if(this_data$S_type!=0) {
                      keep_vars <- c(keep_vars,"L_tp1_var","time_var_free",
                                     "L_full")
                    } else {
                      keep_vars <- c(keep_vars,"L_tp1","time_var_full","time_var_free",
                                     "L_full")
                      
                    }

                    
                  } else if(this_data$time_proc==3) {
                    
                    if(this_data$S_type!=0) {
                      keep_vars <- c(keep_vars,"L_tp1_var","time_var_free","L_AR1",
                                     "L_full")
                    } else {
                      keep_vars <- c(keep_vars,"L_tp1","time_var_full","time_var_free",
                                     "L_full","L_AR1")
                      
                    }
                    
                  } else if(this_data$time_proc==4) {
                    
                    if(this_data$S_type!=0) {
                      
                      keep_vars <- c(keep_vars,"L_tp1_var","time_var_free","L_AR1",
                                     "time_var_gp_free","m_sd_free","gp_sd_free",
                                     "L_full")
                      
                    } else {
                      
                      keep_vars <- c(keep_vars,"L_tp1","L_tp1_var","time_var_free","L_AR1","time_var_full",
                                     "m_sd_full","m_sd_free","gp_sd_free",
                                     "time_var_gp_free",
                                     "L_full")
                      
                      
                    }
                  }
                  
                }
                
              } else if(!is.null(keep_param$person_int)) {
                
                # figure out which people to keep in the estimation
                
                if(keep_param$person_int && (is.null(keep_param$person_vary) || !keep_param$person_vary)) {
                  
                  keep_vars <- c(keep_vars,"L_full")
                  
                }
                
              } 
              
              if(!is.null(keep_param$item)) {
                
                if(keep_param$item) {
                  
                  keep_vars <- c(keep_vars,"sigma_reg_full","B_int_free")
                  
                }
                
              } 
              
              if(!is.null(keep_param$item_miss)) {
                
                if(keep_param$item_miss) {
                  
                  keep_vars <- c(keep_vars,"sigma_abs_free","A_int_free")
                  
                }
                
              } 
              
              if(!is.null(keep_param$extra)) {
                
                if(keep_param$extra) {
                  
                  if(object@person_cov!="personcov0") {
                    
                    keep_vars <- c(keep_vars,"legis_x")
                    
                  }
                  
                  if(object@item_cov!="itemcov0") {
                    
                    keep_vars <- c(keep_vars,"sigma_reg_x")
                    
                  }
                  
                  if(object@item_cov_miss!="itemcovmiss0") {
                    
                    keep_vars <- c(keep_vars,"sigma_abs_x")
                    
                  }
                  
                }
                
              }
               
            }


            if(is.null(ncores)) {
              ncores <- 1
            }
            if(use_vb==FALSE) {
              print("Estimating model with full Stan MCMC sampler.")

                # if(gpu) {
                #   out_model <- object@stanmodel_gpu$sample(data=this_data,chains=nchains,iter_sampling=niters,
                #                                            parallel_chains=nchains,
                #                                            threads_per_chain=ifelse(floor(ncores/nchains)>0,floor(ncores/nchains),1),
                #                                            iter_warmup=warmup,
                #                                            init=init_vals,
                #                                            output_dir=save_files,
                #                                            refresh=this_data$id_refresh,
                #                                            ...)
                # } else {
                  
                  # give informative starting values a shot, if they fail then just do 
                  # pure random
              
                  out_model <- try(object@stanmodel_map$sample(data=this_data,chains=nchains,iter_sampling=niters,
                                                           parallel_chains=nchains,
                                                           threads_per_chain=ifelse(floor(ncores/nchains)>0,floor(ncores/nchains),1),
                                                           iter_warmup=warmup,
                                                           init=init_vals,
                                                           output_dir=save_files,
                                                           refresh=this_data$id_refresh,
                                                           ...))
                  if('try-error' %in% class(out_model)) {
                    
                    print("Finding initialization with pathfinder/laplace failed, using random inits on (-2,2).")
                    
                    out_model <- try(object@stanmodel_map$sample(data=this_data,chains=nchains,iter_sampling=niters,
                                                                 parallel_chains=nchains,
                                                                 threads_per_chain=ifelse(floor(ncores/nchains)>0,floor(ncores/nchains),1),init=init_vals,
                                                                 iter_warmup=warmup,
                                                                 output_dir=save_files,
                                                                 refresh=this_data$id_refresh,
                                                                 ...))
                    
                  }
                  
                # }

              } else {
                
              print("Estimating model with Pathfinder for inference (approximation of true posterior).")
              out_model <- object@stanmodel_map$pathfinder(data=this_data,
                              draws=niters,
                              init=init_vals,
                              num_paths=nchains,num_threads=ncores,
                              refresh=this_data$id_refresh,
                              ...)
            }
            
            outobj <- new('idealstan',
                          score_data=object,
                          model_code=object@stanmodel_map$code(),
                          use_vb=use_vb)
            
            # add safe summaries
            
            # need to use keep_param to filter items/persons in case we need to
            # remove some of them
            
            if(is.null(keep_param)) {
              
              keep_vars <- NULL
              
            }
            
            to_sum <- out_model$summary(variables=keep_vars,
                                                  ~quantile(.x, probs = c(0.05, 0.95),na.rm=T),
                                                  rhat=rhat,~mean(.x,na.rm=T),
                                                  median=median,ess_bulk,ess_tail) 
            
            names(to_sum) <- c("variable","upper","lower","rhat",
                               "mean","median","ess_bulk","ess_tail")
            
            to_sum <- select(to_sum,variable,lower,mean,median,upper,rhat,ess_bulk,ess_tail)
            
            outobj@summary <- to_sum
            
            if(!use_vb) outobj@diagnostics <- out_model$sampler_diagnostics()
            
            outobj@stan_samples <- out_model
            
            outobj@keep_param <- keep_param
            
            return(outobj)
          })

setGeneric('id_model',
           signature='object',
           function(object,...) standardGeneric('id_model'))

setMethod('id_model',signature(object='idealdata'),
          function(object,fixtype='vb',model_type=NULL,this_data=NULL,
                   restrict_ind_high=NULL,
                   restrict_ind_low=NULL,
                   num_restrict_high=NULL,
                   num_restrict_low=NULL,
                   fix_high=NULL,
                   fix_low=NULL,
                   ncores=NULL,
                   const_type=NULL,
                   use_groups=NULL) {
            
            x <- object@score_matrix
            
            if(fixtype %in% c("prefix") && is.null(restrict_ind_high)) {
              
              print("Interactively selecting which items or persons to constrain as they were not pre-specified.")
              
              if(const_type=="persons") {
                restrict_ind_high <- .select_const(object,
                                                   const_type=const_type,
                                                   multiple=F,
                                                   title="Select one person in your data to constrain their ideal point to high values of the latent scale.")$res
                restrict_ind_low <- .select_const(object,
                                                   const_type=const_type,
                                                   multiple=F,
                                                   title="Select one person in your data to constrain their ideal point to low values of the latent scale.")$res
              } else {
                restrict_ind_high <- .select_const(object,
                                                   const_type=const_type,
                                                   multiple=F,
                                                   title="Select one item in your data to constrain its discrimination to high values of the latent scale.")$res
                restrict_ind_low <- .select_const(object,
                                                   const_type=const_type,
                                                   multiple=F,
                                                   title="Select one item in your data to constrain its discrimination to low values of the latent scale.")$res
                
              }

              if(any(restrict_ind_low %in% restrict_ind_high)) {
                stop("Please do not select the same items or persons to constrain both high and low on the latent scale.")
              }
              
            }
            
            if(fixtype=="vb_full") {
              object <- .vb_fix(object=object,this_data=this_data,
                               restrict_ind_high=restrict_ind_high,
                               restrict_ind_low=restrict_ind_low,
                               ncores=ncores,
                               const_type=const_type,
                               model_type=model_type,
                               use_groups=use_groups,
                               num_restrict_high=num_restrict_high,
                               num_restrict_low=num_restrict_low,
                               fixtype=fixtype)
            } else {
              
              # need to convert character to IDs
              
              if(is.character(restrict_ind_high)) {
                if(const_type=="items") {
                  n_old <- names(restrict_ind_high)
                  restrict_ind_high <- as.numeric(factor(restrict_ind_high,
                                                  levels=levels(object@score_matrix$item_id)))
                  names(restrict_ind_high) <- n_old
                } else {
                  if(use_groups) {
                    n_old <- names(restrict_ind_high)
                    restrict_ind_high <- as.numeric(factor(restrict_ind_high,
                                                    levels=levels(object@score_matrix$group_id)))
                    names(restrict_ind_high) <- n_old
                  } else {
                    n_old <- names(restrict_ind_high)
                    restrict_ind_high <- as.numeric(factor(restrict_ind_high,
                                                    levels=levels(object@score_matrix$person_id)))
                    names(restrict_ind_high) <- n_old
                  }
                  
                }
                
              }
              
              if(is.character(restrict_ind_low)) {
                if(const_type=="items") {
                  n_old <- names(restrict_ind_low)
                  restrict_ind_low <- as.numeric(factor(restrict_ind_low,
                                                 levels=levels(object@score_matrix$item_id)))
                  names(restrict_ind_low) <- n_old
                } else {
                  if(use_groups) {
                    n_old <- names(restrict_ind_low)
                    restrict_ind_low <- as.numeric(factor(restrict_ind_low,
                                                   levels=levels(object@score_matrix$group_id)))
                    names(restrict_ind_low) <- n_old
                  } else {
                    n_old <- names(restrict_ind_low)
                    restrict_ind_low <- as.numeric(factor(restrict_ind_low,
                                                   levels=levels(object@score_matrix$person_id)))
                    names(restrict_ind_low) <- n_old
                  }
                  
                }
                
              }
              
              object@restrict_num_high <- fix_high
              object@restrict_num_low <- fix_low
              object@restrict_ind_high <- restrict_ind_high
              object@restrict_ind_low <- restrict_ind_low
              object@constraint_type <- const_type
              
              
            }

            
            

            return(object)
          })

#' Posterior Summaries for fitted \code{idealstan} object
#' 
#' This function produces quantiles and standard deviations for the posterior samples of \code{idealstan} objects.
#' 
#' @param object An \code{idealstan} object fitted by \code{\link{id_estimate}}
#' @param pars Either \code{'ideal_pts'} for person ideal points, 
#' \code{'items'} for items/bills difficulty and discrimination parameters,
#' and \code{'all'} for all parameters in the model, including incidental parameters.
#' @param high_limit A number between 0 and 1 reflecting the upper limit of the 
#' uncertainty interval (defaults to 0.95).
#' @param low_limit A number between 0 and 1 reflecting the lower limit of the 
#' uncertainty interval (defaults to 0.05).
#' @param aggregated Whether to return summaries of the posterior values or the 
#' full posterior samples. Defaults to \code{TRUE}.
#' @param use_chain ID of a specific MCMC chain to use. Default (NULL) is all the chains
#' and is recommended.
#' @return A \code{\link[dplyr]{tibble}} data frame with parameters as rows and descriptive statistics as columns
#' 
#' @export
setMethod('summary',signature(object='idealstan'),
          function(object,pars='ideal_pts',
                   high_limit=0.95,
                   low_limit=0.05,
                   aggregated=TRUE,
                   use_chain=NULL) {
            
            options(tibble.print_max=1000,
                    tibble.print_min=100)


            if(pars=='ideal_pts') {
              ideal_pts <- .prepare_legis_data(object,
                                               high_limit=high_limit,
                                               low_limit=low_limit,
                                               aggregated=aggregated,
                                               use_chain=use_chain)
              if(is.null(ideal_pts$time_id)) {
                ideal_pts$time_id=1
              }
              if(aggregated) {
                ideal_pts <- select(ideal_pts,
                                    Person=person_id,
                                    Group=group_id,
                                    Time_Point=time_id,
                                    `Low Posterior Interval`=low_pt,
                                    `Posterior Median`=median_pt,
                                    `High Posterior Interval`=high_pt,
                                    `Parameter Name`=legis)
              } else {
                # add in iteration numbers
                ideal_pts <- group_by(ideal_pts,person_id,time_id) %>% 
                  mutate(Iteration=1:n())
                ideal_pts <- select(ideal_pts,
                                    Person=person_id,
                                    Group=group_id,
                                    Time_Point=time_id,
                                    Ideal_Points=ideal_pts,
                                    Iteration,
                                    `Parameter Name`=legis)
              }
              return(ideal_pts)
            }
            
            if(pars=='items') {

              # a bit trickier with item points
              item_plot <- levels(object@score_data@score_matrix$item_id)
              if(object@model_type %in% c(1,2) || (object@model_type>6 && object@model_type<13)) {
                # binary models and continuous
                item_points <- lapply(item_plot,.item_plot_binary,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregated=aggregated,
                                      use_chain=use_chain) %>% bind_rows()
              } else if(object@model_type %in% c(3,4)) {
                # rating scale
                item_points <- lapply(item_plot,.item_plot_ord_rs,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregated=aggregated,
                                      use_chain=use_chain) %>% bind_rows()
              } else if(object@model_type %in% c(5,6)) {
                # grm
                item_points <- lapply(item_plot,.item_plot_ord_grm,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregated=aggregated,
                                      use_chain=use_chain) %>% bind_rows()
              } else if(object@model_type %in% c(13,14)) {
                # latent space
                item_points <- lapply(item_plot,.item_plot_ls,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregated=aggregated,
                                      use_chain=use_chain) %>% bind_rows()
              }
              return(item_points)
            }

            
            if(pars=='all') {
              
              return(object@summary)
            }
            
            if(pars %in% c('person_cov','discrim_reg_cov','discrim_infl_cov')) {

              param_name <- switch(pars,person_cov='legis_x',
                                   discrim_reg_cov='sigma_reg_x',
                                   discrim_infl_cov='sigma_abs_x')
              
              to_sum <- object@stan_samples$draws(param_name)
              
              # reset names of parameters
              new_names <- switch(pars,person_cov=object@score_data@person_cov,
                                  discrim_reg=object@score_data@item_cov,
                                  discrim_abs=object@score_data@item_cov_miss)
              
              attributes(to_sum)$dimnames$variable <- new_names
              
              if(!aggregated) {
                return(to_sum)
              } else {
                out_d <- data_frame(Covariate=new_names,
                                    `Posterior Median`=apply(to_sum,3,median),
                                    `Posterior High Interval`=apply(to_sum,3,quantile,high_limit),
                                    `Posterior Low Interval`=apply(to_sum,3,quantile,low_limit),
                                    Parameter=param_name)
                return(out_d)
              }
              
            }
            
})

#' Generic Method for Extracting Posterior Samples
#' 
#' This generic will extract the full \code{\link[rstan]{stan}} posterior samples from \code{idealstan} objects.
#' 
#' See the corresponding method definition for more information about what you can acccess with this generic.
#' 
#' @description This is a generic function.
#' 
#' @param object A fitted \code{idealstan} object
#' @param ... Other arguments passed on to underlying functions
#' @export
setGeneric('id_extract',signature='object',
           function(object,...) standardGeneric('id_extract'))


#' Extract \code{\link[rstan]{stan}} joint posterior distribution from \code{idealstan} object
#' 
#' This convenience function allows you to extract the underlying \code{\link[rstan]{rstan}} posterior estimates for the full parameters
#'   estimates of the \code{idealstan} model object. See \code{\link[rstan]{extract}} for the underlying function and more options.
#'   
#' You can use this function to access a matrix or array of the full posterior estimates of each of the parameters in an 
#'  \code{idealstan} object. There are available options to pick certain parameters of the model, such as the person (legislator)
#'  ideal points or item (bill) discrimination scores. Alternatively, you can leave the \code{extract_type} option blank and 
#'  receive a list of all of the available parameters. Please note that the list of parameters do not have particularly
#'  informative names. 
#'  
#'  All parameters are returned in the order in which they were input into the \code{\link{id_make}} function.
#'  
#' @param object A fitted \code{idealstan} object (see \code{\link{id_estimate}})
#' @param extract_type Can be one of \code{'persons'} for person/legislator ideal points,
#' \code{'obs_discrim'} for non-inflated item (bill) discrimination scores,
#' \code{'obs_diff'} for non-inflated item (bill) difficulty scores,
#' \code{'miss_discrim'} for inflated item (bill) discrimination scores,
#' and \code{'miss_diff'} for inflated item (bill) difficulty scores.
#' @param ... Any additional arguments passed on to the \code{\link[rstan]{extract}} function.
#' 
#' @export
setMethod(id_extract,signature(object='idealstan'),
          function(object,extract_type='persons',...) {
              
            .extract_samples(obj=object,extract_type=extract_type)
            
          })
#' Generic Method to Use \code{shinystan} with \code{idealstan}
#' 
#' A generic function for launching \code{\link[shinystan]{launch_shinystan}}.
#' 
#' @param object A fitted \code{idealstan} object.
#' @param ... Other arguments passed on to underlying function 
#' @export
setGeneric('launch_shinystan',signature='object',
           function(object,...) standardGeneric('launch_shinystan')) 

#' Function to Launch Shinystan with an \code{idealstan} Object
#' 
#' This wrapper will pull the \code{rstan} samples out of a fitted \code{idealstan} model and then launch
#' \code{\link[shinystan]{launch_shinystan}}. This function is useful for examining convergence statistics of the 
#' underlying MCMC sampling.
#' 
#' @seealso \code{\link[shinystan]{shinystan}}
#' @param object A fitted \code{idealstan} object
#' @param pars A character vector of parameters to select from the underlying \code{rstan} model object
#' @param ... Other parameters passed on to \code{\link[shinystan]{shinystan}}
#' @export
setMethod(launch_shinystan,signature(object='idealstan'),
          function(object,pars=c('L_full',
                                 'sigma_reg_full',
                                 'sigma_abs_free',
                                 "A_int_free",
                                 "B_int_free",
                                 'steps_votes',
                                 'steps_votes_grm'),...) {
            if(requireNamespace("shinystan", quietly = FALSE) && packageDescription("shinystan")$Version=="3.0.0") {
              shinystan::launch_shinystan(object@stan_samples,...)
            } else {
              stop("You need to install version 3.0.0 of package shinystan. To do so, use remotes::install_github('stan-dev/shinystan', ref='v3-alpha') ")
            }
            
          })

#' Calculate ideal point marginal effects
#' 
#' This function allows you to calculate ideal point marginal effects for a
#' given person-level hierarchical covariate.
#' 
#' This function will calculate item-level ideal point marginal effects
#' for a given covariate that was passed to the `id_make` function using the
#' `person_cov` option. The function will iterate over all items in the model
#' and use numerical differentiation to calculate responses in the scale of the
#' outcome for each item. Note: if the covariate is binary (i.e., only has two values),
#' then the function will calculate the difference between these two values instead of
#' using numerical differentation.
#' 
#' @returns Returns a tibble that has one row per posterior draw per item-specific
#' marginal effect in the scale of the outcome.
#' @param object A fitted \code{idealstan} model
#' @param ... Other values passed on to methods.
#' @export
setGeneric('id_me',
           signature='object',
           function(object,...) standardGeneric('id_me'))

#' Calculate ideal point marginal effects
#' 
#' This function allows you to calculate ideal point marginal effects for a
#' given person-level hierarchical covariate.
#' 
#' This function will calculate item-level ideal point marginal effects
#' for a given covariate that was passed to the `id_make` function using the
#' `person_cov` option. The function will iterate over all items in the model
#' and use numerical differentiation to calculate responses in the scale of the
#' outcome for each item. Note: if the covariate is binary (i.e., only has two values),
#' then the function will calculate the difference between these two values instead of
#' using numerical differentation.
#' 
#' @returns A list with two objects, \code{ideal_effects} with one estimate of the 
#' marginal effect per item and posterior draw and \code{sum_ideal_effects} with 
#' one row per item with that item's median ideal point marginal effect with the quantiles
#' defined by the \code{upb} and \code{lb} parameters.
#' @param object A fitted \code{idealstan} model
#' @param covariate The character value for a covariate passed to the
#' `id_make` function before model fitting. Only one covariate can be processed
#' at a time.
#' @param group_effects character value of a covariate included in the formula passed
#' to `id_make` for which marginal effect summaries should be grouped by. Useful
#' when looking at the marginal effect of an interaction. Note that grouping by a covariate
#' with many values will result in slow performance.
#' @param pred_outcome Numeric value for level of outcome to predict for ordinal responses.
#' Defaults to top level.
#' @param eps The value used for numerical differentiation. Default is 1e-4. Usually 
#' does not need to be changed.
#' @param draws The total number of draws to use when calculating the marginal effects.
#' Defaults to 100. Use option "all" to use all available MCMC draws.
#' @param cores The total number of cores to use when calculating the marginal effects.
#' Defaults to 1.
#' @param lb The quantile for the lower bound of the aggregated effects (default is 0.05)
#' @param upb The quantile for the upper bound of the aggregated effects (default is 0.95)
#' @param eps Parameter for numerical differentiation (usually does not need to be changed)
#' passed on to [id_post_pred]
#' @param ... Additional arguments passed on to [id_post_pred]
#' @export
setMethod('id_me',signature(object='idealstan'),
          function(object,
                   covariate=NULL,
                   group_effects=NULL,
                   pred_outcome=NULL,
                   eps=1e-4,
                   draws=100,
                   cores=1,
                   lb=0.05,
                   upb=0.95,
                   ...) {
          
            # first need new data with the covariate differenced by eps
            # if binary
            # otherwise set to 1s and 0s
            
            # calc two separate datasets
            # check if it's binary first
            
            # get original data
            
            func_args <- object@score_data@func_args
            
            is_binary <- length(unique(pull(func_args$score_data, {{covariate}} )))==2
            
            if(is_binary) {
              
              data1 <- func_args$score_data
              
              data0 <- mutate(func_args$score_data, {{covariate}} := 1 - .data[[covariate]])
              
            } else {
              
              data1 <- mutate(func_args$score_data, {{covariate}} := .data[[covariate]] + eps / 2)
              
              data0 <- mutate(func_args$score_data, {{covariate}} := .data[[covariate]] - eps / 2)
              
            }
            
            # now create predictions
            
            message("Creating predictions")
            
            # need to sample draws
            
            n_iters <- nrow(as_draws_matrix(object@stan_samples$draws("L_full")))
            
            if(draws!="all") {
              
              draws <- sample(1:n_iters,draws)
              
            }
            
            pred1 <- id_post_pred(object,newdata=data1,
                                          use_cores=cores,
                                          draws=draws,
                                          pred_outcome=pred_outcome,
                                          type="epred",...)
            
            pred0 <- id_post_pred(object,newdata=data0,
                                          use_cores=cores,
                                          draws=draws,
                                          pred_outcome=pred_outcome,
                                          type="epred",...)
            
            message("Differencing")
            
            c1 <- purrr::map2(pred0[[1]],
                              pred1[[1]],
                              function(small,big) {
                                
                                # difference the effects
                                
                                (big - small)/eps
                                
                              })
            
            # combine into datasets for each item with item id + person (senator) id
            
            c2 <- lapply(c1, function(mat) {
              
              out_data <- attr(mat, "data")
              colnames(mat) <- out_data$person_id
              
              as_tibble(mat) %>% 
                mutate(draws=1:n(),
                       item_id=unique(out_data$item_id)) %>% 
                gather(key="person_id",value="estimate",-draws,-item_id) %>% 
                mutate(person_id=as.numeric(person_id),
                       estimate=as.numeric(estimate))
              
            }) %>% bind_rows
            
            # merge in some original data
            to_merge <- mutate(object@score_data@score_matrix, 
                               item_orig=item_id,
                               person_orig=person_id,
                               person_id=as.numeric(person_id),
                               item_id=as.numeric(item_id)) %>% 
              select(person_id, item_id, group_id,item_orig, person_orig, model_id,
                     all_of(group_effects)) %>% 
              distinct
            
            # add in party data so we can calculate party-specific effects
            
            c2 <- left_join(c2, to_merge, 
                            by=c("item_id","person_id"))
            
            # get effect separately by each item 
            
            if(!is.null(group_effects)) {
              
              message(paste0("Grouping marginal effect summaries by ", group_effects))
              
              group_effects <- rlang::sym(group_effects)
              
              sum_vals <- c2 %>% 
                        group_by(draws, !! group_effects , item_id, item_orig, model_id) %>% 
                summarize(mean_est1=mean(estimate)) %>% 
                group_by( !! group_effects, item_id, item_orig, model_id) %>% 
                summarize(mean_est=mean(mean_est1),
                          low_est=quantile(mean_est1, lb),
                          high_est=quantile(mean_est1, upb)) %>% 
                ungroup
              
            } else {
              
              sum_vals <- group_by(c2, item_id, item_orig, model_id) %>% 
                summarize(mean_est=mean(estimate),
                          low_est=quantile(estimate, lb),
                          high_est=quantile(estimate, upb)) %>% 
                ungroup
              
            }
            
            
            
            # merge in item discrimination
            
            item_discrim <- filter(object@summary,
                                   grepl(x=variable, pattern="sigma\\_reg\\_free")) %>% 
              mutate(item_id=as.numeric(stringr::str_extract(variable, "[0-9]+")))
            
            sum_vals <- left_join(sum_vals,
                                  select(item_discrim, item_discrimination='median', item_id))
            
            return(list(sum_ideal_effects=sum_vals,
                        ideal_effects=c2))
            
          
          })

#' Plot the MCMC posterior draws by chain
#' 
#' This function allows you to produce trace plots for assessing the quality
#' and convergence of MCMC chains. 
#' 
#' To use this function, you must pass a fitted \code{idealstan} object
#' along with the name of a parameter in the model. To determine these
#' parameter names, use the \code{summary} function or obtain the data
#' from a plot by passing the \code{return_data=TRUE} option to 
#' \code{id_plog_legis} or \code{id_plot_legis_dyn} to find the 
#' name of the parameter in the Stan model.
#' 
#' This function is a simple wrapper around \code{\link[bayesplot]{mcmc_trace}}. 
#' Please refer to that function's documentation for further options.
#' 
#' @param object A fitted \code{idealstan} model
#' @importFrom bayesplot mcmc_trace
#' @param ... Other options passed on to \code{\link[rstan]{stan_trace}}
#' @export
setGeneric('stan_trace',
           signature='object',
           function(object,...) standardGeneric('stan_trace'))

#' Plot the MCMC posterior draws by chain
#' 
#' This function allows you to produce trace plots for assessing the quality
#' and convergence of MCMC chains. 
#' 
#' To use this function, you must pass a fitted \code{idealstan} object
#' along with the name of a parameter in the model. To determine these
#' parameter names, use the \code{summary} function or obtain the data
#' from a plot by passing the \code{return_data=TRUE} option to 
#' \code{id_plog_legis} or \code{id_plot_legis_dyn} to find the 
#' name of the parameter in the Stan model.
#' 
#' This function is a simple wrapper around \code{\link[bayesplot]{mcmc_trace}}. 
#' Please refer to that function's documentation for further options.
#' 
#' @param object A fitted \code{idealstan} model
#' @param par The character string  name of a parameter in the model 
#' @param ... Other options passed on to \code{\link[bayesplot]{mcmc_trace}}
#' @export
setMethod('stan_trace',signature(object='idealstan'),
          function(object,par='L_full[1]',...) {
            
        mcmc_trace(object@stan_samples$draws(par),...)
          })

