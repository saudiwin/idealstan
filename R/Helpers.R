#' Function that does automatic identification of models using VB
.vb_fix <- function(object=NULL,
                    this_data=NULL,nfix=NULL,auto_id=FALSE,...) {
  
  to_use <- stanmodels[['irt_standard_noid']]
  post_modes <- rstan::vb(object=to_use,data =this_data,
                          algorithm='meanfield')
  
  lookat_params <- rstan::extract(post_modes,permuted=FALSE)
  this_params <- lookat_params[,1,]

   all_args <- list(...) 
   all_params <- attributes(this_params)$dimnames$parameters
   old_matrix <- object@vote_matrix

     if(all_args$restrict_params=='bill') {
       # first we calculate the standardized discrimination parameters for the bills
       # we want to know which distribution has more spread/information
       # we will identify the one that has the greater spread
       sigma_reg <- apply(this_params[,grepl(pattern = 'sigma_reg_full',x=all_params)],2,mean)
       sigma_reg_sd <- apply(this_params[,grepl(pattern = 'sigma_reg_full',x=all_params)],2,sd)
       sigma_abs <- apply(this_params[,grepl(pattern = 'sigma_abs_full',x=all_params)],2,mean)
       sigma_abs_sd <- apply(this_params[,grepl(pattern = 'sigma_abs_full',x=all_params)],2,sd)
       sigma_reg_std <- sigma_reg/sigma_reg_sd
       sigma_abs_std <- sigma_abs / sigma_abs_sd
       if(max(abs(sigma_reg_std))>max(abs(sigma_abs_std))) {
         # Now get nfix number of reg discrim parameters to constrain/fix
         if(all_args$restrict_type=='constrain_oneway') {
           fix_param <- 'sigma_reg'
           to_constrain_high <- sort(abs(sigma_reg_std),index.return=TRUE,decreasing=TRUE)
           to_constrain_high <- to_constrain_high$ix[1:nfix]
           to_constrain_low <- NULL
         } else if(all_args$restrict_type=='constrain_twoway') {
           fix_param <- 'sigma_reg'
           to_constrain_high <- sort(sigma_reg_std,index.return=TRUE,decreasing=TRUE)
           to_constrain_high <- to_constrain_high$ix[1:nfix]
           to_constrain_low <- sort(sigma_reg_std,index.return=TRUE)
           to_constrain_low <- to_constrain_low$ix[1:nfix]
         }
       } else {
         if(all_args$restrict_type=='constrain_oneway') {
           fix_param <- 'sigma_abs'
           to_constrain_high <- sort(abs(sigma_abs_std),index.return=TRUE,decreasing=TRUE)
           to_constrain_high <- to_constrain_high$ix[1:nfix]
           to_constrain_low <- NULL
         } else if(all_args$restrict_type=='constrain_twoway') {
           to_constrain_high <- sort(sigma_abs_std,index.return=TRUE,decreasing=TRUE)
           to_constrain_high <- to_constrain_high$ix[1:nfix]
           to_constrain_low <- sort(sigma_abs_std,index.return=TRUE)
           to_constrain_low <- to_constrain_low$ix[1:nfix]
         }
       }
       object@vote_matrix <- object@vote_matrix[,c((1:ncol(object@vote_matrix))[-c(to_constrain_high,
                                                                              to_constrain_low)],
                                                to_constrain_high,
                                                to_constrain_low)]
       
     } else if(all_args$restrict_params=='legis') {
       legis <- apply(this_params[,grepl(pattern = 'L_full',x=all_params)],2,mean)
       fix_param <- "L_free"
       if(all_args$restrict_type=='constrain_oneway') {
         to_constrain_high <- sort(abs(legis),index.return=TRUE,decreasing=TRUE)
         to_constrain_high <- to_constrain_high$ix[1:nfix]
         to_constrain_low <- NULL
       } else if(all_args$restrict_type=='constrain_twoway') {
         to_constrain_high <- sort(legis,index.return=TRUE,decreasing=TRUE)
         to_constrain_high <- to_constrain_high$ix[1:nfix]
         to_constrain_low <- sort(legis,index.return=TRUE)
         to_constrain_low <- to_constrain_low$ix[1:nfix]
       }
       object@vote_matrix <- object@vote_matrix[c((1:nrow(object@vote_matrix))[-c(to_constrain_high,
                                                                              to_constrain_low)],
                                                to_constrain_high,
                                                   to_constrain_low),]
       
     }
  
   object@restrict_count <- c(to_constrain_high,to_constrain_low)
   
   if(all_args$restrict_params=='bill') {
     this_data$num_fix_high <- nfix
     if(is.null(to_constrain_low)) {
       this_data$num_fix_low <- 1
       this_data$constraint_type <- 2
     } else {
       this_data$num_fix_low <- nfix
       this_data$constraint_type <- 3
     }
   } else if(all_args$restrict_params=='legis') {
     this_data$num_fix_high <- nfix
     if(is.null(to_constrain_low)) {
       this_data$num_fix_low <- 1
       this_data$constraint_type <- 2
     } else {
       this_data$num_fix_low <- nfix
       this_data$constraint_type <- 3
     }
   }
   
   if(fix_param=='sigma_abs') {
     this_data$constrain_par <- 2
   } else if(fix_param=='sigma_reg') {
     this_data$constrain_par <- 3
   } else if(fix_param=='L_free') {
     this_data$constrain_par <- 1
   }
   
   this_data$pin_vals <- rep(1,nfix)
   dim(this_data$pin_vals) <- nfix
   
   if(auto_id==TRUE) {
     # Now re-run ID on 4 VB chains and see if the parameters are stable
     print('Running automatic identification check.')

     all_vbs <- lapply(1:4,function(i,this_data=NULL) {
       vb_out <- vb(object=stanmodels[['irt_standard']],data=this_data,algorithm='meanfield')
       lookat_params <- rstan::extract(vb_out,permuted=FALSE)
       lookat_params <- lookat_params[,1,]
      out_param <- apply(lookat_params,2,mean)
     },this_data=this_data)

     signs <- sapply(all_vbs,sign)
     equal_ratio <- apply(signs,1,function(r) {
       all(r==r[1])
     })
     if(mean(equal_ratio)<0.9) {
       print(paste0('Model is not yet identified. Increasing constraint number to ',nfix+1))
       .vb_fix(object=object,this_params=this_params,this_data=this_data,nfix=nfix+1,auto_id=TRUE,...)
     } else {
       print(paste0('Automatic identification has occurred for ',all_args$restrict_type,
             ' identification with ',nfix,' constraints.'))
     }
   }
   
   object@restrict_num_high <- nfix
   if(is.null(to_constrain_low)) {
     object@restrict_num_low <- 1
   } else {
     object@restrict_num_low <- nfix
   }
   object@constraint_type <- this_data$constraint_type
   object@param_fix <- this_data$constrain_par
   object@unrestricted <- old_matrix
   return(object)
}
  
#' Function that pins certain parameters to fixed points
.pinned_fix <- function(object=NULL,nfix=NULL,restrict_params=NULL,
                        restrict_ind_high=NULL,...) {
  all_args <- list(...) 
  if(is.null(restrict_ind_high)) {
    stop('You must specify indices for pinned paramters as restrict_ind_high.')
  }

  old_matrix <- object@vote_matrix
  to_constrain_high <- restrict_ind_high
  to_constrain_low <- NULL
  
  if(any(restrict_params %in% c('discrim_reg','discrim_abs'))) {

    object@vote_matrix <- object@vote_matrix[,c((1:ncol(object@vote_matrix))[-c(to_constrain_high,
                                                                                to_constrain_low)],
                                                to_constrain_high,
                                                to_constrain_low)]

    param_fix <- switch(restrict_params,discrim_reg='sigma_reg',discrim_abs='sigma_abs')
  } else if(restrict_params=='legis') {
    object@vote_matrix <- object@vote_matrix[c((1:nrow(object@vote_matrix))[-c(to_constrain_high,
                                                                               to_constrain_low)],
                                               to_constrain_high,
                                               to_constrain_low),]
    param_fix <- 'L_free'
  }
  object@restrict_num_high <- length(to_constrain_high)
  object@restrict_num_low <- 1
  object@constraint_type <- 4
  object@param_fix <- switch(param_fix,L_free=1L,sigma_reg=3L,sigma_abs=2L)
  object@unrestricted <- old_matrix
  return(object)
  
}

#' Function that constrains certain known parameters
.constrain_fix <- function(object=NULL,restrict_params=NULL,
                           restrict_ind_high=NULL,
                           restrict_ind_low=NULL,restrict_type=NULL,...) {

  all_args <- list(...) 
  if(is.null(restrict_ind_high)) {
    stop('You must specify at least one bill or legislator to constrain high in restrict_ind_high.')
  }
  old_matrix <- object@vote_matrix
  to_constrain_high <- restrict_ind_high
  to_constrain_low <- restrict_ind_low

  
  if(any(restrict_params %in% c('discrim_reg','discrim_abs'))) {
    object@vote_matrix <- object@vote_matrix[,c((1:ncol(object@vote_matrix))[-c(to_constrain_high,
                                                                                to_constrain_low)],
                                                to_constrain_high,
                                                to_constrain_low)]
    param_fix <- switch(restrict_params,discrim_reg='sigma_reg',discrim_abs='sigma_abs')
  } else if(restrict_params=='legis') {
    object@vote_matrix <- object@vote_matrix[c((1:nrow(object@vote_matrix))[-c(to_constrain_high,
                                                                               to_constrain_low)],
                                               to_constrain_high,
                                               to_constrain_low),]
    param_fix <- 'L_free'
  }
  object@restrict_num_high <- length(to_constrain_high)
  if(is.null(to_constrain_low)) {
    object@restrict_num_low <- 1
  } else {
    object@restrict_num_low <- length(to_constrain_low)
  }
  object@constraint_type <- switch(restrict_type,constrain_oneway=2L,
                                   constrain_twoway=3L)
  object@param_fix <- switch(param_fix,L_free=1L,sigma_reg=3L,sigma_abs=2L)
  object@unrestricted <- old_matrix
  return(object)
  
}

#' Function that works with id_model to re-arrange bills or legislators to constrain for identification
id_params_constrain_guided_inflate <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {

  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  # Use different identifications depending on whether we want to do bills, legislators or both
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    
    # First pull the person fixes
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    x <- rbind(x[-keep_rows,],x[keep_rows,])
    
    # Then do the bill fixes
    
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
      param_fix <- c('person','bill','sigma')
    } else {
      keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix])
      param_fix <- c('person','bill','sigma_abs')
    }
    x <- cbind(x[,-keep_cols],x[,keep_cols])
    
    return(list(restrict=list(restrict_l=nfix,restrict_b=nfix),matrix=x,param_fix=param_fix,
                restrict_vals=c(sigmas_abs$avg[1:nfix],
                                sigmas$avg[1:nfix],
                                legis$avg[1:nfix]),
                unrestricted=lookat_params,
                restrict_legis=keep_rows,
                restrict_bills=keep_cols))
    
  } else if('person' %in% restrict_params) {
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows_low <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    legis <- arrange(legis_est,avg)
    keep_rows_high <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
    param_fix <- 'person'
    return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
           restrict_vals=legis$avg[1:nfix],
           unrestricted=lookat_params,
           restrict_legis=c(keep_rows_high,keep_rows_low),
           restrict_bills='None'))
  } else if('bill' %in% restrict_params) {
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
      param_fix <- c('bill','sigma')
    } else {
      keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix])
      param_fix <- c('bill','sigma_abs')
    }
    x <- cbind(x[,-keep_cols],x[,keep_cols])
    return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
           restrict_vals=c(sigmas$avg[1:nfix],
                           sigmas_abs$avg[1:nfix]),
           unrestricted=lookat_params,
           restrict_bills=keep_cols,
           restrict_legis='None'))
  } else {
    stop('Improper identification parameters passed to the identification function.')
  }
  
  
}

id_params_constrain_guided_2pl <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {
  
  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  # Use different identifications depending on whether we want to do bills, legislators or both
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    
    # First pull the person fixes
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    x <- rbind(x[-keep_rows,],x[keep_rows,])
    
    # Then do the bill fixes
    
    sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
    sigmas <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)

    keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
    param_fix <- c('person','bill','sigma')

    x <- cbind(x[,-keep_cols],x[,keep_cols])
    
    return(list(restrict=list(restrict_l=nfix,restrict_b=nfix),matrix=x,param_fix=param_fix,
                restrict_vals=c(sigmas$avg[1:nfix],
                                legis$avg[1:nfix]),
                unrestricted=lookat_params,
                restrict_legis=keep_rows,
                restrict_bills=keep_cols))
    
  } else if('person' %in% restrict_params) {
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows_low <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    legis <- arrange(legis_est,avg)
    keep_rows_high <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix])
    x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
    param_fix <- 'person'
    return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
                restrict_vals=legis$avg[1:nfix],
                unrestricted=lookat_params,
                restrict_legis=c(keep_rows_high,keep_rows_low),restrict_bills='None'))
  } else if('bill' %in% restrict_params) {
    sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
    sigmas <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)

     keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
      param_fix <- c('bill','sigma')

    x <- cbind(x[,-keep_cols],x[,keep_cols])
    return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
                restrict_vals=sigmas$avg[1:nfix],
                unrestricted=lookat_params,
                restrict_bills=keep_cols,
                restrict_legis='None'))
  } else {
    stop('Improper identification parameters passed to the identification function.')
  }
  
  
}

id_params_pin_unguided_inflate <- function(restrict_params=NULL,nfix=NULL,x=NULL,
                                   restrict_names=NULL,bill_names=NULL,person_names=NULL) {
  
  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    if(length(restrict_names)<2) {
      stop('You must pass both a bill and a person name to pin a parameter for both bills and persons.')
    }
    restrict_pos[1] <- which(restrict_names[1]==person_names)
    restrict_pos[2] <- which(restrict_names[2]==bill_names)
    x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
    x <- cbind(x[,-restrict_pos[2]],x[,restrict_pos[2]])
  } else if('person' %in% restrict_params) {
    restrict_pos[1] <- which(restrict_names[1]==person_names)
    x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
  } else if('bill' %in% restrict_params) {
    restrict_pos[1] <- which(restrict_names[1]==bill_names)
    x <- cbind(x[,-restrict_pos[1]],x[,restrict_pos[1]])
  }
  
  return(list(restrict=list(matrix=x,param_fix=restrict_params,restrict_vals=restrict_pos)))
  
}

#' Function that figures out what kind of hierarchical model (if any) is being run
.get_hier_type <- function(obj) {

  if(all(obj@legis_cov) && all(obj@bill_cov_reg) && all(obj@bill_cov_abs)) {
    return(8)
  } else if(!all(obj@legis_cov) && all(obj@bill_cov_reg) && all(obj@bill_cov_abs)) {
    return(1)
  } else if(all(obj@legis_cov) && !all(obj@bill_cov_reg) && all(obj@bill_cov_abs)) {
    return(2)
  } else if(all(obj@legis_cov) && all(obj@bill_cov_reg) && !all(obj@bill_cov_abs)) {
    return(3)
  } else if(!all(obj@legis_cov) && !all(obj@bill_cov_reg) && all(obj@bill_cov_abs)) {
    return(4)
  } else if(!all(obj@legis_cov) && all(obj@bill_cov_reg) && !all(obj@bill_cov_abs)) {
    return(5)
  } else if(all(obj@legis_cov) && !all(obj@bill_cov_reg) && !all(obj@bill_cov_abs)) {
    return(6)
  } else if(!all(obj@legis_cov) && !all(obj@bill_cov_reg) && !all(obj@bill_cov_abs)) {
    return(7)
  } else {
    stop('Hierarchical modeling undefined.')
  }
}

#' @export
extract_samples <- function(obj,...) {
  rstan::extract(obj@stan_samples,...)
}