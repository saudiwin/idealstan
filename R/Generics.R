setClass('idealdata',
         slots=list(vote_matrix='matrix',
                    legis_data='data.frame',
                    vote_labels='character',
                    vote_count='integer',
                    abs_vote='ANY',
                    restrict_count='integer',
                    param_fix='character'))

setClass('idealstan',
         slots=list(vote_data='idealdata',
                    to_fix='list',
                    model_type='character',
                    model_code='character',
                    test_model_code='character',
                    stan_samples='stanfit'))

setGeneric('subset_ideal',signature='object',
           function(object,...) standardGeneric('subset_ideal'))

setMethod('subset_ideal',signature(object='idealdata'),
          function(object,use_subset=FALSE,sample_it=FALSE,subset_party=NULL,subset_legis=NULL,sample_size=20) {
            
            x <- object@vote_matrix
            parliament <- object@legis_data
            
            if(use_subset==TRUE & !is.null(subset_party)) {
              if(!all(bloc %in% parliament$bloc)) stop('The specified parliament bloc must be in the list of blocs in the legislature data.')
              x <- x[parliament$bloc %in% subset_party,]
            } 
            if(use_subset==TRUE & !is.null(subset_legis)) {
              if(!all(subset_legis %in% parliament$legis.names[parliament$bloc %in% subset_party])) {
                stop('The legislators to subset must be members of the subsetted bloc as well.')
              }
              x <- x[parliament$legis.names %in% subset_legis,]
            }
            
            if(sample_it==TRUE) {
              x <- x[sample(1:nrow(x),sample_size),]
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
            
            this_data$restrict <- object@restrict_count
            
            if(is.null(ncores)) {
              ncores <- 1
            }
            if(use_vb==FALSE) {
            out_model <- sampling(to_use,data=this_data,chains=nchains,iter=niters,cores=ncores,
                                  warmup=warmup,...)
            } else {
            out_model <- vb(to_use,data=this_data,...)
            }
            outobj <- new('idealstan',
                vote_data=object,
                model_code=to_use@model_code,
                stan_samples=out_model)
            
            return(outobj)
          })

setGeneric('id_model',
           signature='object',
           function(object,...) standardGeneric('id_model'))

#' @export
setMethod('id_model',signature(object='idealdata'),
          function(object,fixtype='vb',to_use=NULL,this_data=NULL,nfix=10) {

            x <- object@vote_matrix
           post_modes <- rstan::vb(object=to_use,data =this_data,
                             algorithm='meanfield')
            
            lookat_params <- rstan::extract(post_modes,permuted=FALSE)
            lookat_params <- lookat_params[,1,]
            sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
            sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
              summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
            
            sigmas <- arrange(sigmas_est,avg)
            sigmas_est_abs <- lookat_params[,grepl('sigma_abs',colnames(lookat_params))]
            sigmas_est_abs <- sigmas_est_abs %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
              summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
            
            sigmas_abs <- arrange(sigmas_est_abs,avg)
            if((mean(sigmas$avg[1:nfix])<mean(sigmas_abs$avg[1:nfix])) & (mean(sigmas$interval[1:nfix])<mean(sigmas_abs$interval[1:nfix]))) {
              keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
              param_fix <- 'sigmas'
            } else {
              keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix])
              param_fix <- 'sigmas_abs'
            }
            x <- cbind(x[,-keep_cols],x[,keep_cols])
            object@vote_matrix <- x
            object@restrict_count <- length(keep_cols)
            object@param_fix <- param_fix
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
 