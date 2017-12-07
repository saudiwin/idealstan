#' Data and Identification for \code{id_estimate}
#' 
#' \code{idealdata} objects contain the relevant legislator/bill (person/item) matrix of data along with slots containing information
#' about the kind of identification used in the estimation.
#' @seealso \code{\link{id_make}} to create an \code{idealdata} object suitable for estimation with \code{id_estimate}.
#' @export
setClass('idealdata',
         slots=list(vote_matrix='matrix',
                    person_data='data.frame',
                    item_data='data.frame',
                    item_cov='matrix',
                    item_cov_miss='matrix',
                    person_cov='array',
                    time='vector',
                    vote_labels='ANY',
                    vote_count='integer',
                    miss_val='ANY',
                    restrict_count='numeric',
                    restrict_data='list',
                    stanmodel='stanmodel',
                    param_fix='numeric',
                    constraint_type='numeric',
                    restrict_vals='ANY',
                    subset_party='character',
                    subset_legis='character',
                    to_sample='numeric',
                    unrestricted='matrix',
                    restrict_num_high='numeric',
                    restrict_num_low='numeric',
                    vote_int='numeric',
                    simul_data='list',
                    simulation='logical'))


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
                    model_code='character',
                    test_model_code='character',
                    stan_samples='stanfit',
                    use_vb='logical',
                    simulation='logical'))

setGeneric('subset_ideal',signature='object',
           function(object,...) standardGeneric('subset_ideal'))

setMethod('subset_ideal',signature(object='idealdata'),
          function(object,use_subset=FALSE,sample_it=FALSE,subset_party=NULL,subset_legis=NULL,sample_size=20) {
            
            
            # Functions for subsetting data and sampling
            
            x <- object@vote_matrix
            parliament <- object@person_data
            
            if(use_subset==TRUE & !is.null(subset_party)) {
              if(!all(subset_party %in% parliament$party)) stop('The specified parliament bloc/party must be in the list of blocs/parties in the legislature data.')
              x <- x[parliament$party %in% subset_party,]
              
              object@subset_party <- subset_party
            } 
            if(use_subset==TRUE & !is.null(subset_legis)) {
              if(!all(subset_legis %in% parliament$legis.names[parliament$bloc %in% subset_party])) {
                stop('The legislators to subset must be members of the subsetted bloc as well.')
              }
              x <- x[parliament$legis.names %in% subset_legis,]
              object@subset_legis <- subset_legis
            }
            
            if(sample_it==TRUE) {
              object@to_sample <- sample(1:nrow(x),sample_size)
              x <- x[object@to_sample,]
            }
            object@vote_matrix <- x
            
            return(object)
          })

setGeneric('clean_bills',signature='object',
           function(object,...) standardGeneric('clean_bills'))

setMethod('clean_bills',signature(object='idealdata'),
          function(object) {
            x <- object@vote_matrix
            
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
            object@vote_matrix <- x
            return(object)
          })

setGeneric('sample_model',signature='object',
           function(object,...) standardGeneric('sample_model'))

setMethod('sample_model',signature(object='idealdata'),
          function(object,nchains=4,niters=2000,warmup=floor(niters/2),ncores=NULL,
                   to_use=to_use,this_data=this_data,use_vb=FALSE,...) {

            if(is.null(ncores)) {
              ncores <- 1
            }
            if(use_vb==FALSE) {
              out_model <- sampling(object@stanmodel,data=this_data,chains=nchains,iter=niters,cores=ncores,
                                    warmup=warmup,...)
            } else {
              out_model <- vb(object@stanmodel,data=this_data)
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
                   restrict_params=NULL,restrict_type=NULL,restrict_ind_high=NULL,
                   restrict_ind_low=NULL,
                   auto_id=FALSE,
                   ncores=NULL) {

            x <- object@vote_matrix
            
            run_id <- switch(fixtype,vb=.vb_fix,pinned=.pinned_fix,constrained=.constrain_fix)

            object <- run_id(object=object,this_data=this_data,nfix=nfix,
                   restrict_params=restrict_params,restrict_type=restrict_type,
                   restrict_ind_high=restrict_ind_high,
                   restrict_ind_low=restrict_ind_low,
                   auto_id=auto_id,
                   ncores=ncores,
                   model_type=model_type)
            

            return(object)
          })

#' Posterior Summaries for fitted \code{idealstan} object
#' 
#' This function produces quantiles and standard deviations for the posterior samples of \code{\link{idealstan}} objects.
#' 
#' @param object An \code{idealstan} object fitted by \code{\link{id_estimate}}
#' 
#' @return A \code{\link[dplyr]{tibble}} data frame with parameters as rows and descriptive statistics as columns
#' 
#' @export
setMethod('summary',signature(object='idealstan'),
          function(object) {
            
            options(tibble.print_max=1000,
                    tibble.print_min=100)
            
            this_summary <- rstan::summary(object@stan_samples)[[1]] %>% as_data_frame
            this_summary <- mutate(this_summary,
                                   parameters=row.names(rstan::summary(object@stan_samples)[[1]]),
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
          })


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
#' @param plot_type Specify the plot as a character string. Currently 'legislators' for legislator/person ideal point plot and 
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
          function(object,plot_type='legislators',...) {
            if(plot_type=='legislators') {
              id_plot_legis(object,...)
            } else if(plot_type=='histogram') {
              id_plot_all_hist(object,...)
            }
            
          })
