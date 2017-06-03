setClass('idealdata',
         slots=list(vote_matrix='matrix',
                    legis_data='data.frame',
                    bill_cov_reg='data.frame',
                    bill_cov_abs='data.frame',
                    legis_cov='array',
                    time='vector',
                    vote_labels='ANY',
                    vote_count='integer',
                    abs_vote='ANY',
                    restrict_count='numeric',
                    restrict_data='list',
                    stanmodel='stanmodel',
                    param_fix='character',
                    restrict_vals='numeric',
                    subset_party='character',
                    subset_legis='character',
                    to_sample='numeric',
                    unrestricted='matrix',
                    restrict_bills='ANY',
                    restrict_legis='ANY',
                    vote_int='numeric',
                    simul_data='list',
                    simulation='logical'))

setClass('idealstan',
         slots=list(vote_data='idealdata',
                    to_fix='list',
                    model_type='character',
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
            parliament <- object@legis_data
            
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
                vote_data=object,
                model_code=object@stanmodel@model_code,
                stan_samples=out_model,
                use_vb=use_vb)
            
            return(outobj)
          })

setGeneric('id_model',
           signature='object',
           function(object,...) standardGeneric('id_model'))

#' @export
setMethod('id_model',signature(object='idealdata'),
          function(object,fixtype='vb',modeltype=NULL,this_data=NULL,nfix=10,
                   restrict_params=NULL,restrict_type=NULL,restrict_rows=NULL,auto_id=FALSE,
                   pin_vals=NULL) {
            
            x <- object@vote_matrix
            
            run_id <- switch(fixtype,vb=.vb_fix,pinned=.pinned_fix,constrained=.constrain_fix)
            
            to_use <- stanmodels[[1]]
            post_modes <- rstan::vb(object=to_use,data =this_data,
                                    algorithm='meanfield')

            lookat_params <- rstan::extract(post_modes,permuted=FALSE)
            lookat_params <- lookat_params[,1,]
            browser()
            run_id(object,this_data,nfix,restrict_params,restrict_type,restrict_names,auto_id,
                   pin_vals)
            
            if(is.null(restrict_names) & restrict_type=='constrain' & absence_inflate==TRUE) {
              all_fixed <- id_params_constrain_guided_inflate(lookat_params=lookat_params,restrict_params=restrict_params,
                                                              nfix=nfix,x=x)
              object@vote_matrix <- all_fixed$matrix
              object@restrict_data <- all_fixed$restrict
              object@restrict_count <- nfix
              object@restrict_vals <- all_fixed$restrict_vals
              object@param_fix <- all_fixed$param_fix
              object@stanmodel <- stanmodels[[paste0(modeltype,'_',restrict_type,'_',paste0(all_fixed$param_fix,collapse='_'))]]
              object@unrestricted <- all_fixed$unrestricted
              object@restrict_bills <- all_fixed$restrict_bills
              object@restrict_legis <- all_fixed$restrict_legis
            } else if(is.null(restrict_names) & restrict_type=='constrain' & absence_inflate==FALSE) {
              all_fixed <- id_params_constrain_guided_2pl(lookat_params=lookat_params,restrict_params=restrict_params,
                                                              nfix=nfix,x=x)
              object@vote_matrix <- all_fixed$matrix
              object@restrict_data <- all_fixed$restrict
              object@restrict_count <- nfix
              object@restrict_vals <- all_fixed$restrict_vals
              object@param_fix <- all_fixed$param_fix
              object@stanmodel <- stanmodels[[paste0(modeltype,'_',restrict_type,'_',paste0(all_fixed$param_fix,collapse='_'))]]
              object@unrestricted <- all_fixed$unrestricted
              object@restrict_bills <- all_fixed$restrict_bills
              object@restrict_legis <- all_fixed$restrict_legis
            } else if(!is.null(restrict_names) & restrict_type=='pin' & absence_inflate==TRUE) {
              all_fixed <- id_params_pin_unguided_inflate(restrict_params=restrict_params,
                                                  nfix=nfix,x=x,person_names=object@vote_data@legis.data$legis.names,
                                                  bill_names=colnames(x),restrict_names=restrict_names)
              object@vote_matrix <- all_fixed$matrix
              object@restrict_vals <- all_fixed$restrict_vals
              object@param_fix <- all_fixed$param_fix
              object@restrict_bills <- all_fixed$restrict_bills
              object@restrict_legis <- all_fixed$restrict_legis
              object@stanmodel <- stanmodels[[paste0(modeltype,'_',restrict_type,'_',paste0(all_fixed$param_fix,collapse='_'))]]
            }
            
            
            return(object)
          })

#' @export
setMethod('summary',signature(object='idealstan'),
          function(object) {
            
            options(tibble.print_max=1000,
                    tibble.print_min=100)
            
            this_summary <- rstan::summary(object@stan_samples)[[1]] %>% as_data_frame
            this_summary <- mutate(this_summary,
                                   parameters=row.names(rstan::summary(object@stan_samples)[[1]])) %>% 
                            rename(posterior_mean=`mean`,
                                   posterior_sd=`sd`,
                                   posterior_median=`50%`,
                                   Prob.025=`2.5%`,
                                   Prob.25=`25%`,
                                   Prob.75=`75%`,
                                   Prob.975=`97.5%`) %>% 
              select(parameters,posterior_mean,posterior_median,posterior_sd,Prob.025,
                     Prob.25,Prob.75,Prob.975)
            return(this_summary)
          })


setGeneric('plot_model',
           signature='object',
           function(object,...) standardGeneric('plot_model'))

#' The base plotting function. Default plot shows legislator ideal points with bills as equiprobability lines 
#' (also called trace contour plots and cutting lines).
#' @export
setMethod(plot_model, signature(object='idealstan'),
          function(object,plot_type='legislators',...) {
            if(plot_type=='legislators') {
              legis_plot(object,...)
            } else if(plot_type=='histogram') {
              all_hist_plot(object,...)
            }

          })
 