#' Data and Identification for \code{id_estimate}
#' 
#' \code{idealdata} objects contain the relevant legislator/bill (person/item) matrix of data along with slots containing information
#' about the kind of identification used in the estimation.
#' 
#' @seealso \code{\link{id_make}} to create an \code{idealdata} object suitable for estimation with \code{id_estimate}.
#' @export
setClass('idealdata',
         slots=list(score_matrix='data.frame',
                    person_data='data.frame',
                    group_vals='ANY',
                    group_varying='logical',
                    person_vals='ANY',
                    item_data='data.frame',
                    person_cov='character',
                    item_cov='character',
                    item_cov_miss='character',
                    time='ANY',
                    exog_data='vector',
                    time_vals='vector',
                    vote_labels='ANY',
                    vote_count='integer',
                    miss_val='ANY',
                    restrict_count='numeric',
                    restrict_data='list',
                    stanmodel='stanmodel',
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
                    person_start='numeric'))


#' Results of \code{\link{id_estimate}} function
#' 
#' The \code{idealstan} objects store the results of estimations carried out by the \code{\link{id_estimate}} function. 
#' These objects include the full results of Bayesian sampling performed by the \code{\link[rstan]{stan}} function in the \pkg{rstan}
#' package.
#' @export
setClass('idealstan',
         slots=list(score_data='idealdata',
                    to_fix='list',
                    model_type='numeric',
                    time_proc='numeric',
                    model_code='character',
                    test_model_code='character',
                    stan_samples='stanfit',
                    use_vb='logical',
                    use_groups='logical',
                    simulation='logical'))

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
                   to_use=to_use,this_data=this_data,use_vb=FALSE,
                   tol_rel_obj=NULL,...) {
            
            init_vals <- lapply(1:nchains,.init_stan,
                                num_legis=this_data$num_legis,
                                num_cit=this_data$num_bills,
                                restrict_sd=this_data$restrict_sd,
                                person_sd=this_data$legis_sd,
                                T=this_data$T,
                                const_type=this_data$const_type,
                                fix_high=this_data$fix_high,
                                fix_low=this_data$fix_low,
                                restrict_ind_high=this_data$restrict_high,
                                restrict_ind_low=this_data$restrict_low,
                                time_proc=this_data$time_proc,
                                m_sd_par=this_data$m_sd_par,
                                time_range=mean(diff(this_data$time_ind)),
                                num_diff=this_data$num_diff,
                                time_sd=this_data$time_sd,
                                use_ar=this_data$use_ar,
                                person_start=object@person_start,
                                actual=TRUE)
            
            


            if(is.null(ncores)) {
              ncores <- 1
            }
            if(use_vb==FALSE) {
              print("Estimating model with full Stan MCMC sampler.")
              out_model <- sampling(object@stanmodel,data=this_data,chains=nchains,iter=niters,cores=ncores,
                                    warmup=warmup,
                                    init=init_vals,
                                    refresh=this_data$id_refresh,
                                    ...)
            } else {
              if(is.null(tol_rel_obj)) {
                # set to this number for identification runs
                tol_rel_obj <- 1e-02
              }
              if(this_data$time_proc==4) {
                # increase the precision of the gradient ascent when 
                # using the GP as it is more complicated
                elbo_samples <- 100
                grad_samples <- 1
                eval_elbo <- 100
                #tol_rel_obj <- .0005
              } else {
                elbo_samples <- 100
                grad_samples <- 1
                eval_elbo <- 100
              }
              print("Estimating model with variational inference (approximation of true posterior).")
              out_model <- vb(object@stanmodel,data=this_data,
                              tol_rel_obj=tol_rel_obj,
                              iter=20000,
                              init=init_vals[[1]],
                              elbo_samples=elbo_samples,
                              grad_samples=grad_samples,
                              eval_elbo=eval_elbo,
                              refresh=this_data$id_refresh,
                              ...)
            }
            outobj <- new('idealstan',
                          score_data=object,
                          model_code=object@stanmodel@model_code,
                          stan_samples=out_model,
                          use_vb=use_vb)
            
            return(outobj)
          })

setGeneric('id_model',
           signature='object',
           function(object,...) standardGeneric('id_model'))

setMethod('id_model',signature(object='idealdata'),
          function(object,fixtype='vb',model_type=NULL,this_data=NULL,nfix=10,
                   prior_fit=NULL,
                   tol_rel_obj=NULL,
                   restrict_ind_high=NULL,
                   restrict_ind_low=NULL,
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

              if(all(restrict_ind_low==restrict_ind_high)) {
                stop("Please do not select the same items or persons to constrain both high and low on the latent scale.")
              }
              
            }
            
            if(fixtype=="vb_full") {
              object <- .vb_fix(object=object,this_data=this_data,nfix=nfix,
                               restrict_ind_high=restrict_ind_high,
                               restrict_ind_low=restrict_ind_low,
                               ncores=ncores,
                               const_type=const_type,
                               model_type=model_type,
                               use_groups=use_groups,
                               fixtype=fixtype,
                               prior_fit=prior_fit,
                               tol_rel_obj=tol_rel_obj)
            } else {
              
              # need to convert character to IDs
              
              if(is.character(restrict_ind_high)) {
                if(const_type=="items") {
                  restrict_ind_high <- which(levels(object@score_matrix$item_id)==restrict_ind_high)
                } else {
                  if(use_groups) {
                    restrict_ind_high <- which(levels(object@score_matrix$group_id)==restrict_ind_high)
                  } else {
                    restrict_ind_high <- which(levels(object@score_matrix$person_id)==restrict_ind_high)
                  }
                  
                }
                
              }
              
              if(is.character(restrict_ind_low)) {
                if(const_type=="items") {
                  restrict_ind_low <- which(levels(object@score_matrix$item_id)==restrict_ind_low)
                } else {
                  if(use_groups) {
                    restrict_ind_low <- which(levels(object@score_matrix$group_id)==restrict_ind_low)
                  } else {
                    restrict_ind_low <- which(levels(object@score_matrix$person_id)==restrict_ind_low)
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
#' @param aggregate Whether to return summaries of the posterior values or the 
#' full posterior samples. Defaults to \code{TRUE}.
#' @return A \code{\link[dplyr]{tibble}} data frame with parameters as rows and descriptive statistics as columns
#' 
#' @export
setMethod('summary',signature(object='idealstan'),
          function(object,pars='ideal_pts',
                   high_limit=0.95,
                   low_limit=0.05,
                   aggregate=TRUE) {
            
            options(tibble.print_max=1000,
                    tibble.print_min=100)


            if(pars=='ideal_pts') {
              ideal_pts <- .prepare_legis_data(object,
                                               high_limit=high_limit,
                                               low_limit=low_limit,
                                               aggregate=aggregate)
              if(is.null(ideal_pts$time_id)) {
                ideal_pts$time_id=1
              }
              if(aggregate) {
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
                ideal_pts <- group_by(ideal_pts,person_id) %>% 
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
                                      aggregate=aggregate) %>% bind_rows()
              } else if(object@model_type %in% c(3,4)) {
                # rating scale
                item_points <- lapply(item_plot,.item_plot_ord_rs,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregate=aggregate) %>% bind_rows()
              } else if(object@model_type %in% c(5,6)) {
                # grm
                item_points <- lapply(item_plot,.item_plot_ord_grm,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregate=aggregate) %>% bind_rows()
              } else if(object@model_type %in% c(13,14)) {
                # latent space
                item_points <- lapply(item_plot,.item_plot_ls,object=object,
                                      low_limit=low_limit,
                                      high_limit=high_limit,
                                      all=T,
                                      aggregate=aggregate) %>% bind_rows()
              }
              return(item_points)
            }

            
            if(pars=='all') {
              if(!is.null(pars)) {
                sumobj <- rstan::summary(object@stan_samples,pars=pars)
                this_summary <- sumobj[[1]] %>% as_data_frame
              } else {
                sumobj <- rstan::summary(object@stan_samples)
                this_summary <- sumobj[[1]] %>% as_data_frame
              }
              
              this_summary <- mutate(this_summary,
                                     parameters=row.names(sumobj[[1]]),
                                     par_type=stringr::str_extract(parameters,'[A-Za-z_]+')) %>% 
                rename(posterior_mean=`mean`,
                       posterior_sd=`sd`,
                       posterior_median=`50%`,
                       Prob.025=`2.5%`,
                       Prob.25=`25%`,
                       Prob.75=`75%`,
                       Prob.975=`97.5%`) %>% 
                select(parameters,par_type,posterior_mean,posterior_median,posterior_sd,Prob.025,
                       Prob.25,Prob.75,Prob.975)
              return(this_summary)
            }
            
            if(pars %in% c('person_cov','discrim_reg_cov','discrim_infl_cov')) {

              param_name <- switch(pars,person_cov='legis_x',
                                   discrim_reg_cov='sigma_reg_x',
                                   discrim_infl_cov='sigma_abs_x')
              
              to_sum <- as.array(object@stan_samples,
                                  pars=param_name)
              
              # reset names of parameters
              new_names <- switch(pars,person_cov=object@score_data@person_cov,
                                  discrim_reg=object@score_data@item_cov,
                                  discrim_abs=object@score_data@item_cov_miss)
              
              attributes(to_sum)$dimnames$parameters <- new_names
              
              if(!aggregate) {
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

#' Generic Function for Plotting \code{idealstan} objects
#' 
#' This generic function will run all the plotting functions associated with fitted \code{idealstan} objects.
#' 
#' @param object An \code{idealstan} object
#' @param ... Other options passed onto the underlying plot function
#' 
#' @export
setGeneric('id_plot',
           signature='object',
           function(object,...) standardGeneric('id_plot'))

#' Plot Results of \code{\link{id_estimate}}
#' 
#' This function allows you to access the full range of plotting options for fitted \code{idealstan} models.
#' 
#' \code{id_plot} is a wrapper function that can access the various plotting functions available in the \code{idealstan} package. 
#'    Currently, the options are limited to a plot of legislator/person ideal points with bills/item midpoints as an optional overlay.
#'    Additional plots will be available in future versions of \code{idealstan}.
#' @param object A fitted \code{idealstan} object
#' @param plot_type Specify the plot as a character string. Currently 'persons' for legislator/person ideal point plot and 
#'    'histogram' for a histogram of model estimates for given parameters.
#' @param ... Additional arguments passed on to the underlying functions. See individual function documentation for details.
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @seealso \code{\link{id_plot_legis}} for a legislator/person ideal point plot, 
#' \code{\link{id_plot_all_hist}} for a standard histogram plot,
#' \code{\link{id_plot_compare}} for an ideal point plot of two different models of the same data,
#' \code{\link{id_plot_rhats}} for a histogram of \code{Rhat} values,
#' \code{\link{id_plot_sims}} for plotting true versus estimated values,
#' \code{\link{id_estimate}} for how to estimate an \code{idealstan} object.
#' @export
setMethod(id_plot, signature(object='idealstan'),
          function(object,plot_type='persons',...) {
            if(plot_type=='persons') {
              id_plot_legis(object,...)
            } else if(plot_type=='histogram') {
              id_plot_all_hist(object,...)
            }
            
          })

#' Generic Method for Extracting Posterior Samples
#' 
#' This generic will extract the full \code{\link[rstan]{stan}}} posterior samples from \code{idealstan} objects.
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
#' @importFrom shinystan as.shinystan launch_shinystan
#' @export
setMethod(launch_shinystan,signature(object='idealstan'),
          function(object,pars=c('L_free',
                                 'sigma_reg_free',
                                 'sigma_abs_free',
                                 'restrict_high',
                                 'restrict_low',
                                 'restrict_ord',
                                 'steps_votes',
                                 'steps_votes_grm'),...) {
            to_shiny <- as.shinystan(object@stan_samples)
            launch_shinystan(to_shiny,...)
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
#' This function is a simple wrapper around \code{\link[rstan]{stan_trace}}. 
#' Please refer to that function's documentation for further options.
#' 
#' @param object A fitted \code{idealstan} model
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
#' This function is a simple wrapper around \code{\link[rstan]{stan_trace}}. 
#' Please refer to that function's documentation for further options.
#' 
#' @param object A fitted \code{idealstan} model
#' @param par The character string  name of a parameter in the model 
#' @param ... Other options passed on to \code{\link[rstan]{stan_trace}}
#' @export
setMethod('stan_trace',signature(object='idealstan'),
          function(object,par='L_full[1]') {
            
        rstan::stan_trace(object@stan_samples,pars = par)
          })

