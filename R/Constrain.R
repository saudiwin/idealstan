#' Function that does automatic identification of models using VB
#' @importFrom forcats fct_relevel
#' @noRd
.vb_fix <- function(object=NULL,
                    this_data=NULL,nfix=NULL,
                    ncores=NULL,all_args=NULL,
                    restrict_ind_high=NULL,
                    restrict_ind_low=NULL,
                    tol_rel_obj=NULL,
                    model_type=NULL,
                    use_groups=NULL,
                    fixtype=NULL,...) {
  
  
  . <- NULL
  to_use <- stanmodels[['irt_standard_noid']]
  
  
  eval_elbo <- 100
  
  print("(First Step): Estimating model with variational inference to identify modes to constrain.")
  
  post_modes <- rstan::vb(object=to_use,data =this_data,
                          algorithm='meanfield',
                          refresh=this_data$id_refresh,
                          eval_elbo=eval_elbo,
                          tol_rel_obj=tol_rel_obj, # better convergence criterion than default
                          output_samples=200)
  
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
  item <- apply(this_params[,grepl(pattern = 'sigma_reg_free',x=all_params)],2,mean)
  
  if(fixtype=='vb_full') {
    
    con_out <- .free_constrain(person=person,
                               item=item,
                               const_method=const_method,
                               nfix_low=nfix_low,
                               nfix_high=nfix_high,
                               const_type=const_type,
                               object=object)
    
  } else {
    # use partial ID (we already know which ones to constrain, just figure out diff)
    con_out <- .part_constrain(person=person,
                               item=item,
                               restrict_ind_high=restrict_ind_high,
                               restrict_ind_low=restrict_ind_low,
                               const_type=const_type,
                               const_method=const_method,
                               object=object)
  }
  
  # need new order of variables
  if(!use_groups) {
    new_order <- c(1:length(person))
  } else {
    new_order <- c(1:length(levels(object@score_matrix$group_id)))
  }
  
  new_order <- c(new_order[-c(to_constrain_low,
                              to_constrain_high)],
                 new_order[c(to_constrain_low,
                             to_constrain_high)])
  
  if(this_data$T>1) {
    # do some additional model identification if necessary for time-varying ideal pt models
    # figure out upper limit of estimated variances
    
    ideal_pts_low <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(c(2,3),quantile,.01) %>% .[,new_order]
    ideal_pts_high <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(c(2,3),quantile,.99) %>% .[,new_order]
    ideal_pts_low <- ideal_pts_low*sign_flip
    ideal_pts_high <- ideal_pts_high*sign_flip
    
    # now pull the lowest low and highest high
    ideal_pts_low <- apply(ideal_pts_low,2,function(c) c[which(c==max(c))])
    ideal_pts_high <- apply(ideal_pts_high,2,function(c) c[which(c==min(c))])
    ideal_pts_mean <- rstan::extract(post_modes,'L_tp1')[[1]] %>% apply(c(2,3),median) %>% .[,new_order]
    ideal_pts_mean <- ideal_pts_mean * sign_flip
    time_var_restrict <-  max(apply(this_params[,grepl(pattern = 'time_var_restrict',x=all_params)],2,quantile,.95)[new_order])
    
    if(this_data$time_proc %in% c(2,3)) {
      
      
    } else if(this_data$time_proc==4) {
      
      
      
    }
    
    # need four indices for GP fixing
    restrict_mean_ind <- c(restrict_mean_ind_high_max,
                           restrict_mean_ind_high_min,
                           restrict_mean_ind_low_max,
                           restrict_mean_ind_low_min)
    restrict_mean_val <- c(ideal_pts_mean[restrict_mean_ind[1],restrict_mean_ind[2]] -
                             ideal_pts_mean[restrict_mean_ind[5],restrict_mean_ind[6]],
                           ideal_pts_mean[restrict_mean_ind[3],restrict_mean_ind[4]] -
                             ideal_pts_mean[restrict_mean_ind[7],restrict_mean_ind[8]])
    
    if(fixtype=='vb_partial' && this_data$time_proc==4) {
      if(restrict_mean_val[1]<0) {
        restrict_mean_val <- restrict_mean_val * -1
      }
    } else if(fixtype=='vb_partial' && this_data$time_proc!=4) {
      if(sign(person[to_constrain_high])<0) {
        restrict_mean_val <- restrict_mean_val * -1
      }
    }
    
    
    
    object@restrict_mean_val <- restrict_mean_val
    object@restrict_mean_ind <- restrict_mean_ind
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
  object@person_start <- person[new_order]
  return(object)
}

#' Function that constrains certain known parameters
#' @noRd
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
                                  group_id=fct_relevel(!!quo(group_id),as.character(to_constrain_low),
                                                       as.character(to_constrain_high),
                                                       after=length(levels(!!quo(group_id)))))
  } else {
    object@score_matrix <- mutate(ungroup(object@score_matrix), 
                                  person_id=factor(!! quo(person_id)),
                                  person_id=fct_relevel(!!quo(person_id),as.character(to_constrain_low),
                                                        as.character(to_constrain_high),
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


#' Function to identify constrained variables with no known priors
#' @noRd
.free_constrain <- function(object,
                            person=NULL,
                            item=NULL,
                            const_type=NULL,
                            const_method=NULL,
                            nfix_low=1,
                            nfix_high=1) {
  # now we know which ones to constrain
  
  if(const_type=="item") {
    to_constrain_high <- sort(item,index.return=TRUE,decreasing=TRUE)
    to_constrain_high <- to_constrain_high$ix[1:nfix_high]
    to_constrain_low <- sort(item,index.return=TRUE)
    to_constrain_low <- to_constrain_low$ix[1:nfix_low]
  } else {
    to_constrain_high <- sort(person,index.return=TRUE,decreasing=TRUE)
    to_constrain_high <- to_constrain_high$ix[1:nfix_high]
    to_constrain_low <- sort(person,index.return=TRUE)
    to_constrain_low <- to_constrain_low$ix[1:nfix_low]
  }
  
  
  
  # change to group parameters if index is for groups
  
  # now re-order the factors so that the indices will match  
  if(const_method=="diff") {
    if(object@use_groups) {
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
    sign_flip <- 1 # whether we need to flip signs 
    diff <- person[to_constrain_high[1]]  - person[to_constrain_low[1]]  
    
    return(list(object=object,
                diff=diff))
  } else if(const_method=="constrain") {
    
  }
  
}

#' Function to identify parameters that are specified in advance
#' @noRd
.part_constrain <- function(object,person=NULL,
                            item=NULL,
                            restrict_ind_high=NULL,
                            restrict_ind_low=NULL,
                            const_type=const_type,
                            const_method="person") {
  
  
  
}

#' Function to identify constrained variables when it is known which parameters to constrain
#' @noRd
.prior_constrain <- function(vbfit,object,const_type=NULL,restrict_ind_high=NULL,restrict_ind_low=NULL) {
  
  if(const_type=="person") {
    if(object@use_groups) {
      to_constrain_high <- which(levels(object@score_matrix$group_id) %in% restrict_ind_high)
      to_constrain_low <- which(levels(object@score_matrix$group_id) %in% restrict_ind_low)
    } else {
      to_constrain_high <- which(levels(object@score_matrix$person_id) %in% restrict_ind_high)
      to_constrain_low <- which(levels(object@score_matrix$person_id) %in% restrict_ind_low)
    }
  } else {
      to_constrain_high <- which(levels(object@score_matrix$item_id) %in% restrict_ind_high)
      to_constrain_low <- which(levels(object@score_matrix$item_id) %in% restrict_ind_low)
  }
  
  
  # next we are going to re-order the person IDs around the constraints
  
  object <- .constrain_fix(object=object,restrict_ind_high = restrict_ind_high,
                           restrict_ind_low=restrict_ind_low,
                           use_groups=use_groups)
  
  return(object)
}

#' Function to create more information about identification for time-varying models
#' @noRd
.time_constrain_ar <- function(vbfit,object,const_type=NULL,restrict_ind_high=NULL,restrict_ind_low=NULL) {
  # this is just additional for these models
  col_high <- which(ideal_pts_mean==max(ideal_pts_mean),arr.ind=T)
  col_low <- which(ideal_pts_mean==min(ideal_pts_mean),arr.ind=T)
  restrict_mean_ind_high_max <- col_high
  restrict_mean_ind_high_min <- c(which(ideal_pts_mean[,col_high[,2]]==min(ideal_pts_mean[,col_high[,2]])),
                                  col_high[,2])
  restrict_mean_ind_low_min <- col_low
  restrict_mean_ind_low_max <- c(which(ideal_pts_mean[,col_low[,2]]==max(ideal_pts_mean[,col_low[,2]])),
                                 col_low[,2])
}

#' Function to create more information about identification for time-varying models
#' @noRd
.time_constrain_gp <- function(vbfit,object,const_type=NULL,restrict_ind_high=NULL,restrict_ind_low=NULL) {
  if(fixtype=='vb_full') {
    col_high <- which(ideal_pts_mean==max(ideal_pts_mean),arr.ind=T)
    col_low <- which(ideal_pts_mean==min(ideal_pts_mean),arr.ind=T)
    restrict_mean_ind_high_max <- col_high
    restrict_mean_ind_high_min <- c(which(ideal_pts_mean[,col_high[,2]]==min(ideal_pts_mean[,col_high[,2]])),
                                    col_high[,2])
    restrict_mean_ind_low_min <- col_low
    restrict_mean_ind_low_max <- c(which(ideal_pts_mean[,col_low[,2]]==max(ideal_pts_mean[,col_low[,2]])),
                                   col_low[,2])
  } else {
    # just constrain the lowest one that we fixed
    restrict_mean_ind_high_max <- c(which(ideal_pts_mean[,ncol(ideal_pts_mean)]==max(ideal_pts_mean[,ncol(ideal_pts_mean)])),
                                    ncol(ideal_pts_mean))
    restrict_mean_ind_high_min <- c(which(ideal_pts_mean[,ncol(ideal_pts_mean)]==min(ideal_pts_mean[,ncol(ideal_pts_mean)])),
                                    ncol(ideal_pts_mean))
    restrict_mean_ind_low_min <- c(which(ideal_pts_mean[,ncol(ideal_pts_mean)-1]==min(ideal_pts_mean[,ncol(ideal_pts_mean)-1])),
                                   ncol(ideal_pts_mean)-1)
    restrict_mean_ind_low_max <- c(which(ideal_pts_mean[,ncol(ideal_pts_mean)-1]==max(ideal_pts_mean[,ncol(ideal_pts_mean)-1])),
                                   ncol(ideal_pts_mean)-1)
  }
}

#' Function to select which indices to constrain
#' @importFrom svDialogs dlg_list
#' @noRd
.select_const <- function(object=NULL,
                          const_type='item',
                          multiple=T,
                          title="Select one or more items to constrain high from the following list of items in your data.") {
  
  # given inputs, create a selection window
  
  if(const_type=="items") {
    out_int <- dlg_list(choices = unique(object@score_matrix$item_id),
                        multiple=multiple,
                        title=title)
  } else {
    out_int <- dlg_list(choices = unique(object@score_matrix$person_id),
                        multiple=multiple,
                        title=title)
  }
  
  return(out_int)
  
}