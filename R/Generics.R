setClass('idealdata',
         slots=list(vote_matrix='matrix',
                    legis_data='data.frame',
                    vote_labels='character',
                    vote_count='integer',
                    abs_vote='ANY',
                    restrict_count='integer'))

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
            
            if(use_subset==TRUE) {
                if(!all(bloc %in% parliament$bloc)) stop('The specified parliament bloc must be in the list of blocs in the legislature data.')
                x <- x[parliament$bloc %in% subset_party,]
              } 
              if(subset_legis==TRUE) {
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
                orig <- bind_cols(orig,y)
              }
            })
            x <- x[,select_cols]
          } else {
            select_cols <- apply(x,2, {
              if(length(table(x))<(vote_count)) {
                FALSE
              } else {
                TRUE
                orig <- bind_cols(orig,y)
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
          function(object,nchains=4,niters=2000,warmup=floor(niters/2),ncores=NULL,to_use=to_use,this_data=this_data,...) {
            
            this_data$restrict <- object@restrict_count
            
            if(is.null(ncores)) {
              ncores <- 1
            }
            
            out_model <- sampling(to_use,data=this_data,chains=nchains,iter=niters,cores=ncores,
                                  warmup=warmup,...)
            
            outobj <- new('idealstan',
                vote_data=object,
                model_code=to_use@model_code,
                stan_samples=out_model)
            
            return(outobj)
          })

setGeneric('id_model',
           signature='object',
           function(object,...) standardGeneric('id_model'))

setMethod('id_model',signature(object='idealdata'),
          function(object,fixtype='vb',to_use=NULL,this_data=NULL) {
            
            x <- object@vote_matrix
           post_modes <- vb(object=to_use,data =this_data,
                             algorithm='meanfield')
            
            lookat_params <- rstan::extract(post_modes,permuted=FALSE)
            lookat_params <- lookat_params[,1,]
            sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
            sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
              summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05))
            
            sigmas <- arrange(sigmas_est,avg)
            keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:60])
            x <- cbind(x[,-keep_cols],x[,keep_cols])
            object@vote_matrix <- x
            object@restrict_count <- length(keep_cols)
            return(object)
          })
 