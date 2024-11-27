#' @noRd
.vb_fix <- function(object=NULL,
                    this_data=NULL,
                    ncores=NULL,all_args=NULL,
                    restrict_ind_high=NULL,
                    restrict_ind_low=NULL,
                    model_type=NULL,
                    use_groups=NULL,
                    const_type=NULL,
                    num_restrict_high=NULL,
                    num_restrict_low=NULL,
                    fixtype=NULL,...) {
  
  # collect additional arguments
  if(is.null(all_args)) {
    all_args <- list(...) 
  } 
  
  . <- NULL
  
  print("(First Step): Estimating model with Pathfinder (variational inference) to identify modes to constrain.")
  
  post_modes <- object@stanmodel_map$pathfinder(data =this_data,num_threads=ncores,
                                                num_paths=1,psis_resample=FALSE)
  
  # pull out unidentified parameters
  
  if(const_type=="persons") {
    
    this_params <- post_modes$draws("L_full") %>% as_draws_matrix
    
    person <- apply(this_params,2,mean)
    
    restrict_ind_high <- sort(person,index=T,decreasing=T)$ix[1:num_restrict_high]
    restrict_ind_low <- sort(person,index=T,decreasing=F)$ix[1:num_restrict_low]
    val_high <- person[restrict_ind_high]
    val_low <- person[restrict_ind_low]
    
    # also save values to restrict
    
    fix_high <- sort(person,decreasing=T)[1:num_restrict_high]
    fix_low <- sort(person,decreasing=F)[1:num_restrict_high]
    
  } else if(const_type=="items") {
    
    this_params <- post_modes$draws("sigma_reg_full") %>% as_draws_matrix
    
    items <- apply(this_params,2,mean)
    
    restrict_ind_high <- sort(items,index=T,decreasing=T)$ix[1:num_restrict_high]
    restrict_ind_low <- restrict_ind_low <- sort(items,index=T,decreasing=F)$ix[1:num_restrict_low]
    val_high <- items[restrict_ind_high]
    val_low <- items[restrict_ind_low]
    
  }
  
  object@restrict_num_high <- val_high
  object@restrict_num_low <- val_low
  object@restrict_ind_high <- restrict_ind_high
  object@restrict_ind_low <- restrict_ind_low
  object@constraint_type <- const_type
  object@restrict_num_high <- fix_high
  object@restrict_num_low <- fix_low
  
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