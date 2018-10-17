#' Function that does automatic identification of models using VB
#'@importFrom forcats fct_relevel
.vb_fix <- function(object=NULL,
                    this_data=NULL,nfix=NULL,auto_id=FALSE,
                    ncores=NULL,all_args=NULL,
                    restrict_ind_high=NULL,
                    restrict_ind_low=NULL,
                    model_type=NULL,
                    use_groups=NULL,
                    fixtype=NULL,...) {
  
  
  to_use <- stanmodels[['irt_standard_noid']]

  
  post_modes <- rstan::vb(object=to_use,data =this_data,
                          algorithm='meanfield')

  # Test whether there is a lot of missing data
  
  #use_absence <- .det_missing(object=object,model_type=model_type)

  lookat_params <- rstan::extract(post_modes,permuted=FALSE)
  this_params <- lookat_params[,1,]
  if(is.null(all_args)) {
    all_args <- list(...) 
  } 
  
  all_params <- attributes(this_params)$dimnames$parameters
  
  # Or constrain persons instead of discriminations
  # adjust for which time point we are looking at -- use first time point person param
  person <- apply(this_params[,grepl(pattern = 'L_full',x=all_params)],2,mean)
  
  
  if(fixtype=='vb_full') {
    
    # now we know which ones to constrain
    
    to_constrain_high <- sort(person,index.return=TRUE,decreasing=TRUE)
    to_constrain_high <- to_constrain_high$ix[1:nfix]
    to_constrain_low <- sort(person,index.return=TRUE)
    to_constrain_low <- to_constrain_low$ix[1:nfix]
    
    # change to group parameters if index is for groups
    
    # now re-order the factors so that the indices will match  
    
    if(use_groups==T) {
      # reorder group parameters
      # check if there are more than 2
      if(length(unique(object@score_matrix$group_id))>2) {
        object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                      group_id=factor(!! quo(group_id)),
                                      group_id= factor(!! quo(group_id),
                                                       levels=c(levels(!! quo(group_id))[-c(to_constrain_low,
                                                                                            to_constrain_high)],
                                                                levels(!! quo(group_id))[c(to_constrain_low,
                                                                                           to_constrain_high)])))
      } else {
        object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                      group_id=factor(!! quo(group_id)),
                                      group_id= relevel(!! quo(group_id),
                                                        levels(!! quo(group_id))[to_constrain_high]))
      }
      
    } else {
      if(length(unique(object@score_matrix$person_id))>2) {
        object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                      person_id=factor(!! quo(person_id)),
                                      person_id= factor(!! quo(person_id),
                                                        levels=c(levels(person_id)[-c(to_constrain_low,
                                                                                      to_constrain_high)],
                                                                 levels(person_id)[c(to_constrain_low,
                                                                                     to_constrain_high)])))
      } else {
        object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                      person_id=factor(!! quo(person_id)),
                                      person_id= relevel(!! quo(person_id),
                                                         levels(!! quo(person_id))[to_constrain_high]))
      }
      
    }
    
    # what to constrain the difference to given the priors
    diff_high <- person[to_constrain_high[1]] 
    diff <- person[to_constrain_high[1]]  - person[to_constrain_low[1]]
    
  } else {
    # use partial ID (we already know which ones to constrain, just figure out diff)
    if(use_groups) {
      to_constrain_high <- which(levels(object@score_matrix$group_id)==restrict_ind_high)
      to_constrain_low <- which(levels(object@score_matrix$group_id)==restrict_ind_low)
    } else {
      to_constrain_high <- which(levels(object@score_matrix$person_id)==restrict_ind_high)
      to_constrain_low <- which(levels(object@score_matrix$person_id)==restrict_ind_low)
    }
    
    
    diff_high <- abs(person[to_constrain_high])
    diff <- diff_high - sign(person[to_constrain_high])*person[to_constrain_low]
    
    # next we are going to re-order the person IDs around the constraints

    object <- .constrain_fix(object=object,restrict_ind_high = restrict_ind_high,
                             restrict_ind_low=restrict_ind_low,
                             use_groups=use_groups)
  }

  if(this_data$T>1) {
    # do some additional model identification if necessary for time-varying ideal pt models
    
    # need new order of variables
    
    new_order <- c(1:length(person))
    new_order <- c(new_order[-c(to_constrain_low,
                                to_constrain_high)],
                   new_order[c(to_constrain_low,
                               to_constrain_high)])

    # figure out upper limit of estimated variances
    time_var_restrict <-  max(apply(this_params[,grepl(pattern = 'time_var_restrict',x=all_params)],2,quantile,.95)[new_order])
    # constrain any ideal points that are always positive or always negative
    ideal_pts_low <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(3,quantile,.05) %>% .[new_order]
    ideal_pts_high <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(3,quantile,.95) %>% .[new_order]
    ideal_pts_mean <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(3,mean) %>% .[new_order]
    sign_match <- sign(ideal_pts_low) == sign(ideal_pts_high)
    constrain_mean <- which(sign_match)
    if(length(constrain_mean)>1) {
      constrain_mean <- constrain_mean[abs(ideal_pts_mean[constrain_mean])==max(abs(ideal_pts_mean[constrain_mean]))]
    }
    
    restrict_mean <- ideal_pts_mean[constrain_mean]
    
    object@restrict_mean_val <- restrict_mean
    object@restrict_mean_ind <- constrain_mean
    object@restrict_var_high <- time_var_restrict
  }
  
  
  
  this_data$num_fix_high <- 1
  this_data$num_fix_low <- 1
  this_data$constraint_type <- 3
  this_data$constrain_par <- 1
  object@restrict_num_high <- 1
  object@restrict_num_low <- 1
  object@restrict_ind_high <- to_constrain_high
  object@restrict_ind_low <- to_constrain_low
  object@constraint_type <- this_data$constraint_type
  object@diff <- diff
  object@diff_high <- diff_high
  return(object)
}

#' Function that uses a previously-identified model to maintain comparability across model types
.prior_fit <- function(object=NULL,
                       prior_fit=NULL,
                       use_groups=NULL,...) {
  
  if(is.null(prior_fit)) {
    stop('If using "prior_fit" as the identification method, a fitted idealstan object must be passed to the prior_fit option.')
  }
  
  if(!(use_groups==prior_fit@use_groups)) {
    stop('If groups were used (or not used) in the prior fit used for identification, the same must be true of the current model.')
  }
  
  # simply copy over all of the identification, including data
  # assumes data is the same, so first check on that
  if(use_groups) {
    old_levels <- levels(prior_fit@score_data@score_matrix$group_id)
    new_levels <- levels(object@score_matrix$group_id)
  } else {
    old_levels <- levels(prior_fit@score_data@score_matrix$person_id)
    new_levels <- levels(object@score_matrix$person_id)
  }
  
  if(!all(old_levels %in% new_levels)) {
    stop('To use "prior_fit" as an identification option, the prior fitted idealstan object must have the *same* data as the current model being estimated.')
  }
  
  # now just copy everything over & re-arrange data

  object <- prior_fit@score_data
      
  return(object)
}

#' Function that constrains certain known parameters
.constrain_fix <- function(object=NULL,
                           restrict_ind_high=NULL,
                           restrict_ind_low=NULL,
                           use_groups=NULL,...) {

  all_args <- list(...) 
  if(is.null(restrict_ind_high)) {
    stop('You must specify at least one bill or personlator to constrain high in restrict_ind_high.')
  }

  to_constrain_high <- restrict_ind_high
  to_constrain_low <- restrict_ind_low
  
  # change to group parameters if index is for groups
  
  if(use_groups==T) {
    # reorder group parameters
    object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                  group_id=factor(!! quo(group_id)),
                                  group_id=fct_relevel(!!quo(group_id),to_constrain_low,to_constrain_high,
                                                       after=length(levels(!!quo(group_id)))))
  } else {
    object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                  person_id=factor(!! quo(person_id)),
                                  person_id=fct_relevel(!!quo(person_id),to_constrain_low,to_constrain_high,
                                                       after=length(levels(!!quo(person_id)))))
  }
  
  
  object@restrict_num_high <- length(restrict_ind_high)
  
  object@restrict_num_low <- 1
  object@constraint_type <- 3L
  object@param_fix <- 1L
  object@restrict_ind_high <- to_constrain_high
  object@restrict_ind_low <- to_constrain_low
  
  return(object)
  
}

#' #' Function that works with id_model to re-arrange bills or personlators to constrain for identification
#' .id_params_constrain_guided_inflate <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {
#' 
#'   restrict_params <- sort(restrict_params,decreasing=TRUE)
#'   
#'   #Create row.names for the vote matrix to preserve order
#'   
#'   if(is.null(row.names(x))) {
#'     row.names(x) <- as.character(1:nrow(x))
#'   } 
#'   if(is.null(colnames(x))) {
#'     colnames(x) <- as.character(1:ncol(x))
#'   }
#'   
#'   # Use different identifications depending on whether we want to do bills, personlators or both
#'   if(('person' %in% restrict_params) && ('items' %in% restrict_params)) {
#'     
#'     # First pull the person fixes
#'     person_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
#'     person_est <- person_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     person <- arrange(person_est,desc(avg))
#'     keep_rows <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     x <- rbind(x[-keep_rows,],x[keep_rows,])
#'     
#'     # Then do the bill fixes
#'     
#'     sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
#'     sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     sigmas <- arrange(sigmas_est,avg)
#'     sigmas_est_abs <- lookat_params[,grepl('sigma_abs',colnames(lookat_params))]
#'     sigmas_est_abs <- sigmas_est_abs %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     sigmas_abs <- arrange(sigmas_est_abs,avg)
#'     if((mean(sigmas$avg[1:nfix])<mean(sigmas_abs$avg[1:nfix])) & (mean(sigmas$interval[1:nfix])<mean(sigmas_abs$interval[1:nfix]))) {
#'       keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
#'       param_fix <- c('person','items','sigma')
#'     } else {
#'       keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix])
#'       param_fix <- c('person','items','sigma_abs')
#'     }
#'     x <- cbind(x[,-keep_cols],x[,keep_cols])
#'     
#'     return(list(restrict=list(restrict_l=nfix,restrict_b=nfix),matrix=x,param_fix=param_fix,
#'                 restrict_vals=c(sigmas_abs$avg[1:nfix],
#'                                 sigmas$avg[1:nfix],
#'                                 person$avg[1:nfix]),
#'                 unrestricted=lookat_params,
#'                 restrict_person=keep_rows,
#'                 restrict_bills=keep_cols))
#'     
#'   } else if('person' %in% restrict_params) {
#'     person_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
#'     person_est <- person_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     person <- arrange(person_est,desc(avg))
#'     keep_rows_low <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     person <- arrange(person_est,avg)
#'     keep_rows_high <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
#'     param_fix <- 'person'
#'     return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
#'            restrict_vals=person$avg[1:nfix],
#'            unrestricted=lookat_params,
#'            restrict_person=c(keep_rows_high,keep_rows_low),
#'            restrict_bills='None'))
#'   } else if('items' %in% restrict_params) {
#'     sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
#'     sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     sigmas <- arrange(sigmas_est,avg)
#'     sigmas_est_abs <- lookat_params[,grepl('sigma_abs',colnames(lookat_params))]
#'     sigmas_est_abs <- sigmas_est_abs %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     sigmas_abs <- arrange(sigmas_est_abs,avg)
#'     if((mean(sigmas$avg[1:nfix])<mean(sigmas_abs$avg[1:nfix])) & (mean(sigmas$interval[1:nfix])<mean(sigmas_abs$interval[1:nfix]))) {
#'       keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
#'       param_fix <- c('items','sigma')
#'     } else {
#'       keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix])
#'       param_fix <- c('items','sigma_abs')
#'     }
#'     x <- cbind(x[,-keep_cols],x[,keep_cols])
#'     return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
#'            restrict_vals=c(sigmas$avg[1:nfix],
#'                            sigmas_abs$avg[1:nfix]),
#'            unrestricted=lookat_params,
#'            restrict_bills=keep_cols,
#'            restrict_person='None'))
#'   } else {
#'     stop('Improper identification parameters passed to the identification function.')
#'   }
#'   
#'   
#' }
#' 
#' .id_params_constrain_guided_2pl <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {
#'   
#'   restrict_params <- sort(restrict_params,decreasing=TRUE)
#'   
#'   #Create row.names for the vote matrix to preserve order
#'   
#'   if(is.null(row.names(x))) {
#'     row.names(x) <- as.character(1:nrow(x))
#'   } 
#'   if(is.null(colnames(x))) {
#'     colnames(x) <- as.character(1:ncol(x))
#'   }
#'   
#'   # Use different identifications depending on whether we want to do bills, personlators or both
#'   if(('person' %in% restrict_params) && ('items' %in% restrict_params)) {
#'     
#'     # First pull the person fixes
#'     person_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
#'     person_est <- person_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     person <- arrange(person_est,desc(avg))
#'     keep_rows <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     x <- rbind(x[-keep_rows,],x[keep_rows,])
#'     
#'     # Then do the bill fixes
#'     
#'     sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
#'     sigmas <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#' 
#'     keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
#'     param_fix <- c('person','items','sigma')
#' 
#'     x <- cbind(x[,-keep_cols],x[,keep_cols])
#'     
#'     return(list(restrict=list(restrict_l=nfix,restrict_b=nfix),matrix=x,param_fix=param_fix,
#'                 restrict_vals=c(sigmas$avg[1:nfix],
#'                                 person$avg[1:nfix]),
#'                 unrestricted=lookat_params,
#'                 restrict_person=keep_rows,
#'                 restrict_bills=keep_cols))
#'     
#'   } else if('person' %in% restrict_params) {
#'     person_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
#'     person_est <- person_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#'     
#'     person <- arrange(person_est,desc(avg))
#'     keep_rows_low <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     person <- arrange(person_est,avg)
#'     keep_rows_high <- as.numeric(stringr::str_extract(person$param_name,'[0-9]+')[1:nfix])
#'     x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
#'     param_fix <- 'person'
#'     return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
#'                 restrict_vals=person$avg[1:nfix],
#'                 unrestricted=lookat_params,
#'                 restrict_person=c(keep_rows_high,keep_rows_low),restrict_bills='None'))
#'   } else if('items' %in% restrict_params) {
#'     sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
#'     sigmas <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#'       summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
#' 
#'      keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix])
#'       param_fix <- c('items','sigma')
#' 
#'     x <- cbind(x[,-keep_cols],x[,keep_cols])
#'     return(list(restrict=list(restrict=nfix),matrix=x,param_fix=param_fix,
#'                 restrict_vals=sigmas$avg[1:nfix],
#'                 unrestricted=lookat_params,
#'                 restrict_bills=keep_cols,
#'                 restrict_person='None'))
#'   } else {
#'     stop('Improper identification parameters passed to the identification function.')
#'   }
#'   
#'   
#' }
#' 
#' .id_params_pin_unguided_inflate <- function(restrict_params=NULL,nfix=NULL,x=NULL,
#'                                    restrict_names=NULL,bill_names=NULL,person_names=NULL) {
#'   
#'   restrict_params <- sort(restrict_params,decreasing=TRUE)
#'   
#'   #Create row.names for the vote matrix to preserve order
#'   
#'   if(is.null(row.names(x))) {
#'     row.names(x) <- as.character(1:nrow(x))
#'   } 
#'   if(is.null(colnames(x))) {
#'     colnames(x) <- as.character(1:ncol(x))
#'   }
#'   
#'   if(('person' %in% restrict_params) && ('items' %in% restrict_params)) {
#'     if(length(restrict_names)<2) {
#'       stop('You must pass both a bill and a person name to pin a parameter for both bills and persons.')
#'     }
#'     restrict_pos[1] <- which(restrict_names[1]==person_names)
#'     restrict_pos[2] <- which(restrict_names[2]==bill_names)
#'     x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
#'     x <- cbind(x[,-restrict_pos[2]],x[,restrict_pos[2]])
#'   } else if('person' %in% restrict_params) {
#'     restrict_pos[1] <- which(restrict_names[1]==person_names)
#'     x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
#'   } else if('items' %in% restrict_params) {
#'     restrict_pos[1] <- which(restrict_names[1]==bill_names)
#'     x <- cbind(x[,-restrict_pos[1]],x[,restrict_pos[1]])
#'   }
#'   
#'   return(list(restrict=list(matrix=x,param_fix=restrict_params,restrict_vals=restrict_pos)))
#'   
#' }

#' Function that figures out what kind of hierarchical model (if any) is being run
.get_hier_type <- function(obj) {
  
  if(all(obj@person_cov) && all(obj@item_cov) && all(obj@item_cov_miss)) {
    return(8)
  } else if(!all(obj@person_cov) && all(obj@item_cov) && all(obj@item_cov_miss)) {
    return(1)
  } else if(all(obj@person_cov) && !all(obj@item_cov) && all(obj@item_cov_miss)) {
    return(2)
  } else if(all(obj@person_cov) && all(obj@item_cov) && !all(obj@item_cov_miss)) {
    return(3)
  } else if(!all(obj@person_cov) && !all(obj@item_cov) && all(obj@item_cov_miss)) {
    return(4)
  } else if(!all(obj@person_cov) && all(obj@item_cov) && !all(obj@item_cov_miss)) {
    return(5)
  } else if(all(obj@person_cov) && !all(obj@item_cov) && !all(obj@item_cov_miss)) {
    return(6)
  } else if(!all(obj@person_cov) && !all(obj@item_cov) && !all(obj@item_cov_miss)) {
    return(7)
  } else {
    stop('Hierarchical modeling undefined.')
  }
}

.extract_samples <- function(obj=NULL,extract_type=NULL,...) {
  if(!is.null(extract_type)) {
    param <- switch(extract_type,persons='L_full',
                    reg_discrim='sigma_reg_full',
                    miss_discrim='sigma_abs_full',
                    reg_diff='B_int_full',
                    miss_diff='A_int_full',
                    cutpoints='steps_votes')
    rstan::extract(obj@stan_samples,pars=param,...)
  } else {
    rstan::extract(obj@stan_samples,...)
  }
  
  
}


#helper to calculate medians and high/low points of a column given a name 
.calc_bill <- function(df,int_reg,
                       sigma_reg,
                       int_abs=NULL,
                       sigma_abs=NULL,
                       steps_data=NULL,
                       step_num=2,
                       this_num=NULL) {
  
  if(is.null(steps_data)) {
    # non-ordinal
    # int_reg <- enenquo(int_reg)
    # sigma_reg <- enquo(sigma_reg)
    # int_abs <- enquo(int_abs)
    # sigma_abs <- enquo(sigma_abs)
    out_data <- summarize(df,median_bill=median((!!int_reg)/(!!sigma_reg)),
                          high_bill=quantile((!!int_reg)/(!!sigma_reg),0.9),
                          low_bill=quantile((!!int_reg)/(!!sigma_reg),0.1)) %>% 
      mutate(param='Vote Points',
             step=1)
    if(!is.null(int_abs)) {
      
      out_data <- summarize(df,median_bill=median((!!int_abs)/(!!sigma_abs)),
                            high_bill=quantile((!!int_abs)/(!!sigma_abs),0.9),
                            low_bill=quantile((!!int_abs)/(!!sigma_abs),0.1)) %>% 
        mutate(param='Absence Points',
               step=1) %>% 
        bind_rows(out_data)
    }
    
  } else {
    #ordinal
    # int_reg <- enquo(int_reg)
    # sigma_reg <- enquo(sigma_reg)
    # int_abs <- enquo(int_abs)
    # sigma_abs <- enquo(sigma_abs)
    
    out_data <- lapply(1:step_num, function(s,steps_data=NULL) {
      
      out_data <- summarize(df,median_bill=median(((!!int_reg)+steps_data[,s])/(!!sigma_reg)),
                            high_bill=quantile(((!!int_reg)+steps_data[,s])/(!!sigma_reg),0.9),
                            low_bill=quantile(((!!int_reg)+steps_data[,s])/(!!sigma_reg),0.1)) %>% 
        mutate(param='Vote Points',
               step=s)
      
      if(!is.null(int_abs)) {
        
        out_data <- summarize(df,median_bill=median(((!!int_abs)+steps_data[,s])/(!!sigma_abs)),
                              high_bill=quantile(((!!int_abs)+steps_data[,s])/(!!sigma_abs),0.9),
                              low_bill=quantile(((!!int_abs)+steps_data[,s])/(!!sigma_abs),0.1)) %>% 
          mutate(param='Absence Points',
                 step=s) %>% 
          bind_rows(out_data)
        
      }
      return(out_data)
    },steps_data=steps_data) %>% bind_rows
    
  }
  mutate(out_data,bill_num=this_num) %>% return()
}

#' Helper function that determines if there are more missing than non-missing
#' observations
.det_missing <- function(object,model_type=NULL) {
  if(model_type %in% c(2,4,6)) {
    all_data <- object@score_matrix$outcome
    return((sum(all_data==object@miss_val)/length(all_data))>.5)
  } else {
    return(FALSE)
  }
}

#' Helper function for preparing person ideal point plot data
.prepare_legis_data <- function(object) {
  

  # 
  if(length(unique(object@score_data@score_matrix$time_id))>1) {
    
    person_params <- as.data.frame(object@stan_samples,pars='L_tp1')
    
    person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
      group_by(legis) %>% 
      summarize(low_pt=quantile(ideal_pts,0.1),high_pt=quantile(ideal_pts,0.9),
                median_pt=median(ideal_pts)) %>% 
      mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
             param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
             time_point=stringr::str_extract(legis,'\\[[0-9]+'),
             time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
    # get ids out 
    
    person_ids <- select(object@score_data@score_matrix,
                         !!quo(person_id),
                         !!quo(time_id),
                         !!quo(group_id)) %>% 
      distinct %>% 
      mutate(person_id_num=as.numeric(!!quo(person_id)),
             time_id_num=as.numeric(factor(!!quo(time_id))),
             group_id_num=as.numeric(!!quo(group_id)))
    
    if(object@use_groups) {
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(param_id='group_id_num',
                                  time_point='time_id_num'))
    } else {
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(param_id='person_id_num',
                                  time_point='time_id_num'))
    }
    
  } else {
    # need to match estimated parameters to original IDs
    person_params <- as.data.frame(object@stan_samples,pars='L_full')
    person_params <- person_params %>% gather(key = legis,value=ideal_pts) 
    # get ids out 
    
    person_ids <- data_frame(long_name=person_params$legis) %>% 
      distinct
    legis_nums <- stringr::str_extract_all(person_ids$long_name,'[0-9]+',simplify=T)
    person_ids <-   mutate(person_ids,id_num=as.numeric(legis_nums))
    
    person_data <- distinct(select(object@score_data@score_matrix,
                                   person_id,group_id))
    
    
    # add in all data in the person_data object
    if(object@use_groups) {
      person_data <- mutate(person_data,id_num=as.numeric(group_id))

    } else {
      person_data <- mutate(person_data,id_num=as.numeric(person_id))
    }
    
    person_ids <- left_join(person_ids,person_data)
    
    person_params <-  person_params %>% 
      group_by(legis) %>% 
      summarize(low_pt=quantile(ideal_pts,0.1),high_pt=quantile(ideal_pts,0.9),
                median_pt=median(ideal_pts)) %>% 
      left_join(person_ids,by=c(legis='long_name'))
    
  }
  
  person_params 
}

#' Helper function to create arrays
#' 
#' Function takes a data.frame in long mode and converts it to an array. Function can also repeat a 
#' single matrix to fill out an array.
#' 
#' @param input_matrix Either a data.frame in long mode or a single matrix
#' @param arr_dim If \code{input_matrix} is a single matrix, \code{arr_dim} determines the length of the resulting array
#' @param row_var Unquoted variable name that identifies the data.frame column corresponding to the rows (1st dimension) of the array (must be unique)
#' @param col_var_name Unquoted variable name that identifies the data.frame column corresponding names of the columns (2nd dimension) of the array
#' @param col_var_value Unquoted variable name that identifies the data.frame column corresponding to the values that populate the cells of the array
#' @param third_dim_var Unquoted variable name that identifis the data.frame column corresponding to the dimension around which to stack the matrices (3rd dimension of array)
.create_array <- function(input_matrix,arr_dim=2,row_var=NULL,
                          col_var_name=NULL,
                          col_var_value,third_dim_var=NULL) {
  
  if('matrix' %in% class(input_matrix)) {
    
    # if just a matrix, rep it to hit array dims
    rep_matrix <- rep(c(input_matrix),arr_dim)
    out_array <- array(rep_matrix,dim=c(dim(input_matrix),arr_dim))
    
  } else if('data.frame' %in% class(input_matrix)) {
    
    # assuming data is in long form, select and then spread the bugger
    row_var <- enquo(row_var)
    col_var_name <- enquo(col_var_name)
    col_var_value <- enquo(col_var_value)
    third_dim_var <- enquo(third_dim_var)
    to_spread <- ungroup(input_matrix) %>% select(!!row_var,!!col_var_name,!!third_dim_var,!!col_var_value)
    
    # figure out how big this array should be
    arr_dim <- length(unique(pull(to_spread,!!third_dim_var)))
    
    if(!(nrow(distinct(to_spread))==nrow(to_spread))) stop('Each row in the data must be uniquely identified given row_var, col_var and third_dim_var.')
    
    to_array <- lapply(split(to_spread,pull(to_spread,!!third_dim_var)), function(this_data) {
      # spread and stuff into a list
      spread_it <- spread(this_data,key=!!col_var_name,value=!!col_var_value) %>% 
        select(-!!row_var,-!!third_dim_var) %>% as.matrix
      row.names(spread_it) <- unique(pull(this_data,!!row_var))
      return(spread_it)
    })
    # convert to a vector before array-ing it
    long_vec <- c(do.call(c,to_array))
    # BOOM
    out_array <- array(long_vec,
                       dim=c(dim(to_array[[1]]),arr_dim),
                       dimnames=list(row.names=row.names(to_array[[1]]),
                                     colnames=colnames(to_array[[1]]),
                                     stack=unique(pull(to_spread,!!third_dim_var))))
  }
  
  return(out_array)
}

#' Simple function to test for what an input is
#' Default_val should be quoted
.check_quoted <- function(quoted=NULL,default_val) {
  if(is.null(quoted)) {
    quoted <- default_val
  } else if(class(quoted)=='character') {
    quoted <- as.name(quoted)
    quoted <- enquo(quoted)
  } else {
    stop(paste0('Please do not enter a non-character value for ',as.character(default_val)[2]))
  }
}

#' Simple function to provide initial values to Stan given current values of restrict_sd
.init_stan <- function(chain_id=NULL,
                       restrict_sd=NULL,
                        person_sd=NULL,
                       num_legis=NULL,
                       diff_high=NULL,
                       T=NULL,
                       restrict_var_high=NULL,
                       time_sd=NULL,
                       restrict_var=NULL,
                       actual=TRUE,
                       use_ar=NULL) {

  
  if(actual==TRUE) {
    # full run
    if(T>1) {
      if(restrict_var) {
        return(list(restrict_high = array(rnorm(n=1,mean=diff_high,sd=restrict_sd)),
                    L_free = array(rnorm(n=num_legis-2,mean=0,sd=person_sd)),
                    L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                    time_var_restrict = rep(restrict_var_high/2,num_legis)))
      } else {
        return(list(restrict_high = array(rnorm(n=1,mean=diff_high,sd=restrict_sd)),
                    L_free = array(rnorm(n=num_legis-2,mean=0,sd=person_sd)),
                    L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                    time_var = rep(time_sd,num_legis)))
      }
      
    } else {
      return(list(restrict_high = array(rnorm(n=1,mean=diff_high,sd=restrict_sd)),
                  L_free = array(rnorm(n=num_legis-2,mean=0,sd=person_sd)),
                  L_AR1 = array(runif(n = num_legis,min = -.5,max=.5))))
    }

  } else {
    #identification run
    return(list(L_free = rnorm(n=num_legis,mean=0,sd=person_sd),
         L_AR1 = runif(n = num_legis,min = -.5,max=.5)))
  }
  
  
  
}

#' used to calculate the true ideal points
#' given that a non-centered parameterization is used.
.calc_true_pts <- function(obj) {


  over_time <- rstan::extract(obj@stan_samples,'L_tp1')$L_tp1
  drift <- rstan::extract(obj@stan_samples,'L_full')$L_full
  
  save_array <- environment()
  save_array$array_slot <- array(data=NA,dim=dim(over_time))
  if(obj@use_ar) {
    new_pts <- sapply(1:dim(over_time)[2], function(t) {
      sapply(1:dim(over_time)[3], function(i) {
        if(t==1) {
          save_array$array_slot[,t,i] <- drift[,i]
        } else {
          save_array$array_slot[,t,i] <- over_time[,t,i,drop=F] + drift[,i]
        }
        
      })
      
    })
    new_pts <- save_array$array_slot
  } else {
    over_time[,1,] <- drift
    new_pts <- over_time
  }


  return(new_pts)
}

#' Pre-process rollcall objects
.prepare_rollcall <- function(rc_obj=NULL,item_id=NULL,time_id=NULL) {
  
  # make the outcome

  score_data <- as_data_frame(rc_obj$votes) %>% 
    mutate(person_id=row.names(rc_obj$votes))  %>% 
    gather(key = item_id,value = outcome,-person_id)
  
   # merge in other data
  if(is.null(rc_obj$legis.data$legis.names)) {
    rc_obj$legis.data$legis.names <- row.names(rc_obj$legis.data)
  }
  
  score_data <- left_join(score_data,rc_obj$legis.data,by=c(person_id='legis.names'))
  
  score_data <- mutate(score_data,group_id=party)
  
  # extract time from bill labels if it exists
  if(!is.null(time_id)) {

    score_data <- left_join(score_data,as_data_frame(rc_obj$vote.data),by=c(item_id=item_id))
  } else {
    score_data$time_id <- 1
    time_id <- 'time_id'
  }
  
  item_id <- 'item_id'
  
  return(list(score_data=score_data,
              time_id=time_id,
              item_id=item_id))
  
} 