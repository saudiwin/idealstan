#' @noRd
.vb_fix <- function(object=NULL,
                    this_data=NULL,nfix=NULL,
                    ncores=NULL,all_args=NULL,
                    restrict_ind_high=NULL,
                    restrict_ind_low=NULL,
                    tol_rel_obj=NULL,
                    model_type=NULL,
                    use_groups=NULL,
                    const_type=NULL,
                    fixtype=NULL,...) {
  
  # collect additional arguments
  if(is.null(all_args)) {
    all_args <- list(...) 
  } 

  . <- NULL
  to_use <- stanmodels[['irt_standard_noid']]
  
  if(this_data$time_proc==4) {
    tol_rel_obj <- .001
    eval_elbo <- 100
  } else {
    eval_elbo <- 100
    tol_rel_obj <- .001
  }

  print("(First Step): Estimating model with variational inference to identify modes to constrain.")

  post_modes <- rstan::vb(object=to_use,data =this_data,
                          algorithm='meanfield',
                          refresh=this_data$id_refresh,
                          eval_elbo=eval_elbo,
                          tol_rel_obj=tol_rel_obj, # better convergence criterion than default
                          output_samples=200)

  lookat_params <- rstan::extract(post_modes,permuted=FALSE)
  
  this_params <- lookat_params[,1,]
  
  all_params <- attributes(this_params)$dimnames$parameters
  
  # pull out unidentified parameters
  
  if(const_type=="persons") {
    person <- apply(this_params[,grepl(pattern = 'L_full',x=all_params)],2,mean)

    restrict_ind_high <- which(person==max(person))[1]
    restrict_ind_low <- which(person==min(person))[1]
    val_high <- person[restrict_ind_high]
    val_low <- person[restrict_ind_low]
  } else if(const_type=="items") {
    items <- apply(this_params[,grepl(pattern = 'sigma_reg_free',x=all_params)],2,mean)

    restrict_ind_high <- which(items==max(items))[1]
    restrict_ind_low <- which(items==min(items))[1]
    val_high <- items[restrict_ind_high]
    val_low <- items[restrict_ind_low]
    
  }
  
  object@restrict_num_high <- val_high
  object@restrict_num_low <- val_low
  object@restrict_ind_high <- restrict_ind_high
  object@restrict_ind_low <- restrict_ind_low
  object@constraint_type <- const_type

  return(object)

  
}



#' @noRd
.extract_samples <- function(obj=NULL,extract_type=NULL,...) {
  if(!is.null(extract_type)) {
    param <- switch(extract_type,persons='L_full',
                    obs_discrim='sigma_reg_free',
                    miss_discrim='sigma_abs_free',
                    obs_diff='B_int_free',
                    miss_diff='A_int_free',
                    cutpoints='steps_votes3')
    as.data.frame(obj@stan_samples,pars=param,...)
  } else {
    as.data.frame(obj@stan_samples,...)
  }
  
  
}



#' Helper function for preparing person ideal point plot data
#' @noRd
.prepare_legis_data <- function(object,
                                high_limit=NULL,
                                low_limit=NULL,
                                aggregate=TRUE,
                                type='ideal_pts') {
  
  if(length(unique(object@score_data@score_matrix$time_id))>1 && type!='variance') {
    
    person_params <- as.data.frame(object@stan_samples,pars='L_tp1')
    if(aggregate) {
      person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
        group_by(legis) %>% 
        summarize(low_pt=quantile(ideal_pts,low_limit),high_pt=quantile(ideal_pts,high_limit),
                  median_pt=median(ideal_pts)) %>% 
        mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
               param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
               time_point=stringr::str_extract(legis,'\\[[0-9]+'),
               time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
    } else {
      person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
        group_by(legis) %>% 
        mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
               param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
               time_point=stringr::str_extract(legis,'\\[[0-9]+'),
               time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
    }

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
    if(type=='ideal_pts') {
      person_params <- as.data.frame(object@stan_samples,pars='L_full')
    } else if(type=='variance') {
      # load time-varying person variances
        person_params <- as.data.frame(object@stan_samples,pars='time_var_full')
    }
    
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
    
    if(aggregate) {
      person_params <-  person_params %>% 
        group_by(legis) %>% 
        summarize(low_pt=quantile(ideal_pts,low_limit),high_pt=quantile(ideal_pts,high_limit),
                  median_pt=median(ideal_pts)) %>% 
        left_join(person_ids,by=c(legis='long_name'))
    } else {
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(legis='long_name'))
    }

    
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
#' @noRd
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
      spread_it <- try(spread(this_data,key=!!col_var_name,value=!!col_var_value))
      if('try-error' %in% class(spread_it)) {
        print('Failed to find unique covariate values for dataset:')
        print(this_data)
        stop()
      }
      spread_it <- spread_it %>% 
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
#' @noRd
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
#' @importFrom stats optimize
#' @noRd
.init_stan <- function(chain_id=NULL,
                       restrict_sd=NULL,
                        person_sd=NULL,
                       num_legis=NULL,
                       legis_labels=NULL,
                       item_labels=NULL,
                       num_cit=NULL,
                        fix_high=NULL,
                       fix_low=NULL,
                       restrict_ind_high=NULL,
                       restrict_ind_low=NULL,
                       m_sd_par=NULL,
                       num_diff=NULL,
                       time_range=NULL,
                       const_type=NULL,
                       T=NULL,
                       time_proc=NULL,
                       time_sd=NULL,
                       actual=TRUE,
                       use_ar=NULL,
                       person_start=NULL) {

  L_full <- array(rnorm(n=num_legis,mean=0,sd=person_sd))
  sigma_reg_free <- array(rnorm(n=num_cit,mean=0,sd=2))
  sigma_abs_free <- array(rnorm(n=num_cit,mean=0,sd=2))
  A_int_free <- array(rnorm(n=num_cit,mean=0,sd=2))
  B_int_free <- array(rnorm(n=num_cit,mean=0,sd=2))
  
  names(L_full) <- legis_labels
  names(sigma_reg_free) <- paste0("Obs_Discrim_",item_labels)
  names(sigma_abs_free) <- paste0("Miss_Discrim_",item_labels)
  names(A_int_free) <- paste0("Obs_Difficulty_",item_labels)
  names(B_int_free) <- paste0("Miss_Difficulty_",item_labels)
  
  if(const_type==1 && !is.null(const_type)) {
    
    L_full[restrict_ind_high] <- fix_high
    L_full[restrict_ind_low] <- fix_low
    
  } else if(const_type==2 && !is.null(const_type)) {

    sigma_reg_free[restrict_ind_high] <- fix_high
    sigma_reg_free[restrict_ind_low] <- fix_low
  }
  
  # given upper bound on m_sd figure out mean to match real value on the real numbers
  
  # rev_trans <- function(x,m_sd_par) {
  #   m_sd_par[1]*plogis(x) - 1/m_sd_par[2]
  # }

  
  if(actual==TRUE) {
    # full run
    if(T>1) {
      
      # figure out optimized gp par
      
      # m_sd_optim <- optimize(f=rev_trans,
      #                        interval=c(0,m_sd_par[1]),
      #                     m_sd_par=m_sd_par)$objective
      # 
      # if(m_sd_optim<0) {
      #   # shouldn't happen, but just in case
      #   m_sd_optim <- m_sd_par[1]/2
      # }
      if(time_proc==4) {
        return(list(L_full = L_full,
                    sigma_reg_free=sigma_reg_free,
                    sigma_abs_free=sigma_abs_free,
                    A_int_free=A_int_free,
                    B_int_free=B_int_free,
                    m_sd=rep(m_sd_par,num_legis),
                    time_var_gp_free=log(rep(num_diff*time_range,num_legis-1))))
      } else if(time_proc==3) {
        return(list(L_full = L_full,
                    L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                    time_var_free = rexp(rate=1/time_sd,n=num_legis-1),
                    sigma_reg_free=sigma_reg_free,
                    sigma_abs_free=sigma_abs_free,
                    A_int_free=A_int_free,
                    B_int_free=B_int_free))
        
        } else if(time_proc==2) {
          return(list(L_full = L_full,
                      time_var_free = rexp(rate=1/time_sd,n=num_legis-1),
                      sigma_reg_free=sigma_reg_free,
                      sigma_abs_free=sigma_abs_free,
                      A_int_free=A_int_free,
                      B_int_free=B_int_free))
          } else {
            
        return(list(L_full = L_full,
                    sigma_reg_free=sigma_reg_free,
                    sigma_abs_free=sigma_abs_free,
                    A_int_free=A_int_free,
                    B_int_free=B_int_free))
        
      }
          

  } else {
    #identification run
    return(list(L_full = L_full,
                sigma_reg_free=sigma_reg_free,
                sigma_abs_free=sigma_abs_free,
                A_int_free=A_int_free,
                B_int_free=B_int_free))
  }
  
  
  
  }
  
}

#' used to calculate the true ideal points
#' given that a non-centered parameterization is used.
#' @noRd
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
#' @noRd
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
  if(!is.null(rc_obj$vote.data)) {

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

#' Generate item-level midpoints for binary IRT outcomes
#' @noRd
.item_plot_binary <- function(param_name,object,
                       high_limit=NULL,
                       low_limit=NULL,
                       all=FALSE,
                       aggregate=FALSE) {
  
  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as.data.frame(object@stan_samples,pars=paste0('B_int_free[',param_num,']'))[[1]]
  reg_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_reg_free[',param_num,']'))[[1]]
  abs_diff <- as.data.frame(object@stan_samples,pars=paste0('A_int_free[',param_num,']'))[[1]]
  abs_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_abs_free[',param_num,']'))[[1]]
  
  reg_mid <- reg_diff/reg_discrim
  abs_mid <- abs_diff/abs_discrim
  
  if(class(object@score_data@score_matrix$outcome)=='factor') {
    cut_names <- levels(object@score_data@score_matrix$outcome)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome))
  }
  if(!all) {
    reg_data <- data_frame(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[2],
                           item_name=param_name)
    
    abs_data <- data_frame(item_median=quantile(abs_mid,0.5),
                           item_high=quantile(abs_mid,high_limit),
                           item_low=quantile(abs_mid,low_limit),
                           item_type='Inflated\nDiscrimination',
                           Outcome='Missing',
                           item_name=param_name)
    
    out_d <- bind_rows(reg_data,abs_data)
    
    return(out_d)
    
  } else if(all && aggregate) {
    reg_data_mid <- data_frame(`Posterior Median`=quantile(reg_mid,0.5),
                               `High Posterior Interval`=quantile(reg_mid,high_limit),
                               `Low Posterior Interval`=quantile(reg_mid,low_limit),
                           `Item Type`='Non-Inflated Item Midpoint',
                           `Predicted Outcome`=cut_names[2],
                           `Item Name`=param_name,
                           `Parameter`=paste0('A function of other parameters'))
    
    abs_data_mid <- data_frame(`Posterior Median`=quantile(abs_mid,0.5),
                               `High Posterior Interval`=quantile(abs_mid,high_limit),
                               `Low Posterior Interval`=quantile(abs_mid,low_limit),
                           `Item Type`='Inflated Item Midpoint',
                           `Item Name`=param_name,
                           `Predicted Outcome`='Missing',
                           `Parameter`=paste0('A function of other parameters'))
    
    reg_data_discrim <- data_frame(`Posterior Median`=quantile(reg_discrim,0.5),
                                   `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                   `Item Name`=param_name,
                               `Item Type`='Non-Inflated Discrimination',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=paste0('sigma_reg_free[',param_name,']'))
    
    abs_data_discrim <- data_frame(`Posterior Median`=quantile(abs_discrim,0.5),
                                   `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                   `Item Name`=param_name,
                               `Item Type`='Inflated Discrimination',
                               `Predicted Outcome`='Missing',
                               `Parameter`=paste0('sigma_abs_free[',param_name,']'))
    
    reg_data_diff <- data_frame(`Posterior Median`=quantile(reg_diff,0.5),
                                `High Posterior Interval`=quantile(reg_diff,high_limit),
                                `Low Posterior Interval`=quantile(reg_diff,low_limit),
                                `Item Name`=param_name,
                                   `Item Type`='Non-Inflated Difficulty',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('B_int_free[',param_name,']'))
    
    abs_data_diff <- data_frame(`Posterior Median`=quantile(abs_diff,0.5),
                                `High Posterior Interval`=quantile(abs_diff,high_limit),
                                `Low Posterior Interval`=quantile(abs_diff,low_limit),
                                `Item Name`=param_name,
                                   `Item Type`='Inflated Difficulty',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('A_int_free[',param_name,']'))
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  } else if(all && !aggregate) {
    reg_data_mid <- data_frame(Posterior_Sample=reg_mid,
                               `Item Name`=param_name,
                               `Item Type`='Non-Inflated Item Midpoint',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`='A function of other parameters') %>% 
      mutate(Iteration=1:n())
    
    abs_data_mid <- data_frame(`Posterior_Sample`=abs_mid,
                               `Item Name`=param_name,
                               `Item Type`='Inflated Item Midpoint',
                               `Predicted Outcome`='Missing',
                               `Parameter`='A function of other parameters') %>% 
      mutate(Iteration=1:n())
    
    reg_data_discrim <- data_frame(`Posterior_Sample`=reg_discrim,
                                   `Item Name`=param_name,
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('sigma_reg_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data_discrim <- data_frame(`Posterior_Sample`=abs_discrim,
                                   `Item Name`=param_name,
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('sigma_abs_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    reg_data_diff <- data_frame(`Posterior_Sample`=reg_diff,
                                `Item Name`=param_name,
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=paste0('B_int_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data_diff <- data_frame(`Posterior_Sample`=abs_discrim,
                                `Item Name`=param_name,
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=paste0('A_int_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  }

}

#' Generate item-level midpoints for ordinal-rating scale IRT outcomes
#' @noRd
.item_plot_ord_rs <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                              all=FALSE,
                              aggregate=FALSE) {

  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as.data.frame(object@stan_samples,pars=paste0('B_int_free[',param_num,']'))[[1]]
  reg_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_reg_free[',param_num,']'))[[1]]
  abs_diff <- as.data.frame(object@stan_samples,pars=paste0('A_int_free[',param_num,']'))[[1]]
  abs_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_abs_free[',param_num,']'))[[1]]
  cuts <- as.data.frame(object@stan_samples,pars='steps_votes')
  if(class(object@score_data@score_matrix$outcome)=='factor') {
    cut_names <- levels(object@score_data@score_matrix$outcome)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome))
  }
  abs_mid <- abs_diff/abs_discrim
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    
    reg_data <- data_frame(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[c],
                           item_name=param_name)
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- data_frame(item_median=quantile(abs_mid,0.5),
                         item_high=quantile(abs_mid,high_limit),
                         item_low=quantile(abs_mid,low_limit),
                         item_type='Inflated\nDiscrimination',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(abs_data,reg_data)
  
  if(!all) {
    
    return(out_d)
  
} else if(all && aggregate) {
  
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    reg_data <- data_frame(`Posterior Median`=quantile(reg_mid,0.5),
                           `High Posterior Interval`=quantile(reg_mid,high_limit),
                           `Low Posterior Interval`=quantile(reg_mid,low_limit),
                           `Item Type`='Non-Inflated Item Midpoint',
                           `Predicted Outcome`=cut_names[c],
                           `Parameter`=param_name)
    
    
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- data_frame(`Posterior Median`=quantile(abs_mid,0.5),
                         `High Posterior Interval`=quantile(abs_mid,high_limit),
                         `Low Posterior Interval`=quantile(abs_mid,low_limit),
                         `Item Type`='Inflated Item Midpoint',
                         `Predicted Outcome`='Missing',
                         `Parameter`=param_name)
  
  reg_data_discrim <- data_frame(`Posterior Median`=quantile(reg_discrim,0.5),
                                 `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                 `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                 `Item Type`='Non-Inflated Discrimination',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name)
  
  abs_data_discrim <- data_frame(`Posterior Median`=quantile(abs_discrim,0.5),
                                 `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                 `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                 `Item Type`='Inflated Discrimination',
                                 `Predicted Outcome`='Missing',
                                 `Parameter`=param_name)
  
  reg_data_diff <- data_frame(`Posterior Median`=quantile(reg_diff,0.5),
                              `High Posterior Interval`=quantile(reg_diff,high_limit),
                              `Low Posterior Interval`=quantile(reg_diff,low_limit),
                              `Item Type`='Non-Inflated Difficulty',
                              `Predicted Outcome`=cut_names[2],
                              `Parameter`=param_name)
  
  abs_data_diff <- data_frame(`Posterior Median`=quantile(abs_discrim,0.5),
                              `High Posterior Interval`=quantile(abs_discrim,high_limit),
                              `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                              `Item Type`='Inflated Difficulty',
                              `Predicted Outcome`='Missing',
                              `Parameter`=param_name)
  
  out_d <- bind_rows(reg_data,abs_data,reg_data_discrim,
                     abs_data_discrim,
                     reg_data_diff,
                     abs_data_diff)
  
  return(out_d)
} else if(all && !aggregate) {
  
  reg_data_mid <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    reg_data_mid <- data_frame(Posterior_Sample=reg_mid,
                               `Item Type`='Non-Inflated Item Midpoint',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    
    
    return(reg_data_mid)
  }) %>% bind_rows

  
  abs_data_mid <- data_frame(`Posterior_Sample`=abs_mid,
                             `Item Type`='Inflated Item Midpoint',
                             `Predicted Outcome`='Missing',
                             `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  reg_data_discrim <- data_frame(`Posterior_Sample`=reg_discrim,
                                 `Item Type`='Non-Inflated Discrimination',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  abs_data_discrim <- data_frame(`Posterior_Sample`=abs_discrim,
                                 `Item Type`='Inflated Discrimination',
                                 `Predicted Outcome`='Missing',
                                 `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  reg_data_diff <- data_frame(`Posterior_Sample`=reg_diff,
                              `Item Type`='Non-Inflated Difficulty',
                              `Predicted Outcome`=cut_names[2],
                              `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  abs_data_diff <- data_frame(`Posterior_Sample`=abs_discrim,
                              `Item Type`='Inflated Difficulty',
                              `Predicted Outcome`='Missing',
                              `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                     abs_data_discrim,
                     reg_data_diff,
                     abs_data_diff)
  
  return(out_d)
}
  
}

#' Generate item-level midpoints for ordinal-GRM IRT outcomes
#' @noRd
.item_plot_ord_grm <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                              all=FALSE,
                              aggregate=FALSE) {

  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as.data.frame(object@stan_samples,pars=paste0('B_int_free[',param_num,']'))[[1]]
  reg_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_reg_free[',param_num,']'))[[1]]
  abs_diff <- as.data.frame(object@stan_samples,pars=paste0('A_int_free[',param_num,']'))[[1]]
  abs_discrim <- as.data.frame(object@stan_samples,pars=paste0('sigma_abs_free[',param_num,']'))[[1]]
  
  # figure out how many categories we need
  
  total_cat <- ncol(as.data.frame(object@stan_samples,pars='steps_votes'))
  
  cuts <- as.data.frame(object@stan_samples,pars=paste0('steps_votes_grm[',param_num,',',total_cat,']'))
  
  if(class(object@score_data@score_matrix$outcome)=='factor') {
    cut_names <- levels(object@score_data@score_matrix$outcome)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome))
  }
  abs_mid <- abs_diff/abs_discrim
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    
    reg_data <- data_frame(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[c],
                           item_name=param_name)
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- data_frame(item_median=quantile(abs_mid,0.5),
                         item_high=quantile(abs_mid,high_limit),
                         item_low=quantile(abs_mid,low_limit),
                         item_type='Inflated\nDiscrimination',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(abs_data,reg_data)
  
  if(!all) {
    
    return(out_d)
    
  } else if(all && aggregate) {
    
    # need to loop over cuts
    
    reg_data <- lapply(1:ncol(cuts), function(c) {
      reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
      
      reg_data <- data_frame(`Posterior Median`=quantile(reg_mid,0.5),
                             `High Posterior Interval`=quantile(reg_mid,high_limit),
                             `Low Posterior Interval`=quantile(reg_mid,low_limit),
                             `Item Type`='Non-Inflated Item Midpoint',
                             `Predicted Outcome`=cut_names[c],
                             `Parameter`=param_name)
      
      
      
      return(reg_data)
    }) %>% bind_rows
    
    abs_data <- data_frame(`Posterior Median`=quantile(abs_mid,0.5),
                           `High Posterior Interval`=quantile(abs_mid,high_limit),
                           `Low Posterior Interval`=quantile(abs_mid,low_limit),
                           `Item Type`='Inflated Item Midpoint',
                           `Predicted Outcome`='Missing',
                           `Parameter`=param_name)
    
    reg_data_discrim <- data_frame(`Posterior Median`=quantile(reg_discrim,0.5),
                                   `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=param_name)
    
    abs_data_discrim <- data_frame(`Posterior Median`=quantile(abs_discrim,0.5),
                                   `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=param_name)
    
    reg_data_diff <- data_frame(`Posterior Median`=quantile(reg_diff,0.5),
                                `High Posterior Interval`=quantile(reg_diff,high_limit),
                                `Low Posterior Interval`=quantile(reg_diff,low_limit),
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=param_name)
    
    abs_data_diff <- data_frame(`Posterior Median`=quantile(abs_discrim,0.5),
                                `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=param_name)
    
    out_d <- bind_rows(reg_data,abs_data,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  } else if(all && !aggregate) {
    
    reg_data_mid <- lapply(1:ncol(cuts), function(c) {
      reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
      
      reg_data_mid <- data_frame(Posterior_Sample=reg_mid,
                                 `Item Type`='Non-Inflated Item Midpoint',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name) %>% 
        mutate(Iteration=1:n())
      
      
      
      return(reg_data_mid)
    }) %>% bind_rows
    
    
    abs_data_mid <- data_frame(`Posterior_Sample`=abs_mid,
                               `Item Type`='Inflated Item Midpoint',
                               `Predicted Outcome`='Missing',
                               `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    reg_data_discrim <- data_frame(`Posterior_Sample`=reg_discrim,
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    abs_data_discrim <- data_frame(`Posterior_Sample`=abs_discrim,
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    reg_data_diff <- data_frame(`Posterior_Sample`=reg_diff,
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    abs_data_diff <- data_frame(`Posterior_Sample`=abs_discrim,
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  }  
}

#' Generate item-level midpoints for binary latent-space outcomes
#' @noRd
.item_plot_ls <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                          aggregate=F) {

  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as.data.frame(object@stan_samples,pars=paste0('B_int_free[',param_num,']'))[[1]]
  abs_diff <- as.data.frame(object@stan_samples,pars=paste0('A_int_free[',param_num,']'))[[1]]
  item_int <- as.data.frame(object@stan_samples,pars=paste0('sigma_abs_free[',param_num,']'))[[1]]
  ideal_int <- as.data.frame(object@stan_samples,pars=paste0('ls_int[',param_num,']'))[[1]]
  
  if(class(object@score_data@score_matrix$outcome)=='factor') {
    cut_names <- levels(object@score_data@score_matrix$outcome)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome))
  }
  
  reg_data <- data_frame(item_median=quantile(reg_diff,0.5),
                         item_high=quantile(reg_diff,high_limit),
                         item_low=quantile(reg_diff,low_limit),
                         item_type='Non-Inflated\nItem\nIdeal Point',
                         Outcome=cut_names[2],
                         item_name=param_name)
  
  abs_data <- data_frame(item_median=quantile(abs_diff,0.5),
                         item_high=quantile(abs_diff,high_limit),
                         item_low=quantile(abs_diff,low_limit),
                         item_type='Inflated\nItem\nIdeal Point',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(reg_data,abs_data)
  
  
  
  if(!all) {
    
    return(out_d)
    
  } else if(all && aggregate) {
    reg_data <- data_frame(item_median=quantile(reg_diff,0.5),
                           item_high=quantile(reg_diff,high_limit),
                           item_low=quantile(reg_diff,low_limit),
                           item_type='Non-Inflated Item Ideal Point',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('B_int_free[',param_num,']'))
    
    abs_data <- data_frame(item_median=quantile(abs_diff,0.5),
                           item_high=quantile(abs_diff,high_limit),
                           item_low=quantile(abs_diff,low_limit),
                           item_type='Inflated Item Ideal Point',
                           Outcome='Missing',
                           item_name=param_name,
                           Parameter=paste0('A_int_free[',param_num,']'))
    
    ideal_int <- data_frame(item_median=quantile(ideal_int,0.5),
                           item_high=quantile(ideal_int,high_limit),
                           item_low=quantile(ideal_int,low_limit),
                           item_type='Ideal Point Intercept',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('sigma_reg_free[',param_num,']'))
    
    item_int <- data_frame(item_median=quantile(item_int,0.5),
                           item_high=quantile(item_int,high_limit),
                           item_low=quantile(item_int,low_limit),
                           item_type='Item Intercept',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('sigma_abs_free[',param_num,']'))
    
    out_d <- bind_rows(reg_data,abs_data,ideal_int,item_int)
    
    return(out_d)
  } else if(all && !aggregate) {
    reg_data <- data_frame(Posterior_Sample=reg_diff,
                               `Item Name`=param_name,
                               `Item Type`='Non-Inflated Item Ideal Point',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=paste0('B_int_free[',param_num,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data <- data_frame(`Posterior_Sample`=abs_diff,
                               `Item Name`=param_name,
                               `Item Type`='Inflated Item Ideal Point',
                               `Predicted Outcome`='Missing',
                               `Parameter`=paste0('A_int_free[',param_num,']')) %>% 
      mutate(Iteration=1:n())
    
    ideal_int <- data_frame(`Posterior_Sample`=ideal_int,
                                   `Item Name`=param_name,
                                   `Item Type`='Ideal Point Intercept',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('sigma_reg_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    item_int<- data_frame(`Posterior_Sample`=item_int,
                                   `Item Name`=param_name,
                                   `Item Type`='Item Intercept',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('sigma_abs_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data,abs_data,
                       ideal_int,item_int)
    
    return(out_d)
  }
  
}

#' a slightly hacked function to extract parameters as I want to
#' @noRd
.extract_nonp <- function(object, pars, permuted = TRUE, 
                                inc_warmup = FALSE, include = TRUE) {
            # Extract the samples in different forms for different parameters. 
            #
            # Args:
            #   object: the object of "stanfit" class 
            #   pars: the names of parameters (including other quantiles) 
            #   permuted: if TRUE, the returned samples are permuted without
            #     warming up. And all the chains are merged. 
            #   inc_warmup: if TRUE, warmup samples are kept; otherwise, 
            #     discarded. If permuted is TRUE, inc_warmup is ignored. 
            #   include: if FALSE interpret pars as those to exclude
            #
            # Returns:
            #   If permuted is TRUE, return an array (matrix) of samples with each
            #   column being the samples for a parameter. 
            #   If permuted is FALSE, return array with dimensions
            #   (# of iter (with or w.o. warmup), # of chains, # of flat parameters). 
            
            if (object@mode == 1L) {
              cat("Stan model '", object@model_name, "' is of mode 'test_grad';\n",
                  "sampling is not conducted.\n", sep = '')
              return(invisible(NULL)) 
            } else if (object@mode == 2L) {
              cat("Stan model '", object@model_name, "' does not contain samples.\n", sep = '') 
              return(invisible(NULL)) 
            } 
            
            if(!include) pars <- setdiff(object@sim$pars_oi, pars)
            pars <- if (missing(pars)) object@sim$pars_oi else .check_pars_second(object@sim, pars) 
            pars <- .remove_empty_pars(pars, object@sim$dims_oi)
            tidx <- .pars_total_indexes(object@sim$pars_oi, 
                                       object@sim$dims_oi, 
                                       object@sim$fnames_oi, 
                                       pars) 
            
            n_kept <- object@sim$n_save - object@sim$warmup2
            fun1 <- function(par_i) {
              # sss <- sapply(tidx[[par_i]], get_kept_samples2, object@sim)
              # if (is.list(sss))  sss <- do.call(c, sss)
              # the above two lines are slower than the following line of code
              sss <- do.call(cbind, lapply(tidx[[par_i]], .get_kept_samples2, object@sim)) 
              dim(sss) <- c(sum(n_kept), object@sim$dims_oi[[par_i]]) 
              dimnames(sss) <- list(iterations = NULL)
              attr(sss,'num_chains') <- object@sim$chains
              attr(sss,'chain_order') <- rep(1:object@sim$chains,each=dim(sss)[1]/object@sim$chains)

              sss 
            } 
            
            if (permuted) {
              slist <- lapply(pars, fun1) 
              names(slist) <- pars 
              return(slist) 
            } 
            
            tidx <- unlist(tidx, use.names = FALSE) 
            tidxnames <- object@sim$fnames_oi[tidx] 
            sss <- lapply(tidx, .get_kept_samples2, object@sim, inc_warmup) 
            sss2 <- lapply(sss, function(x) do.call(c, x))  # concatenate samples from different chains
            sssf <- unlist(sss2, use.names = FALSE) 
            
            n2 <- object@sim$n_save[1]  ## assuming all the chains have equal iter 
            if (!inc_warmup) n2 <- n2 - object@sim$warmup2[1] 
            dim(sssf) <- c(n2, object@sim$chains, length(tidx)) 
            cids <- sapply(object@stan_args, function(x) x$chain_id)
            dimnames(sssf) <- list(iterations = NULL, chains = paste0("chain:", cids), parameters = tidxnames)
            sssf 
          }


#' we are going to modify this rstan function so that it no longer permutes
#' just delete the last term -- maybe submit PR to rstan
#' @noRd
.get_kept_samples2 <- function(n, sim) {

  # a different implementation of get_kept_samples 
  # It seems this one is faster than get_kept_samples 
  # TODO: to understand why it is faster? 
  lst <- vector("list", sim$chains)
  for (ic in 1:sim$chains) { 
    if (sim$warmup2[ic] > 0) 
      lst[[ic]] <- sim$samples[[ic]][[n]][-(1:sim$warmup2[ic])]
    else 
      lst[[ic]] <- sim$samples[[ic]][[n]]
  } 
  out <- do.call(c, lst)
}

#' another hacked function
#' @noRd
.check_pars_second <- function(sim, pars) {
  #
  # Check if all parameters in pars are parameters for which we saved
  # their samples
  #
  # Args:
  #   sim: The sim slot of class stanfit
  #   pars: a character vector of parameter names
  #
  # Returns:
  #   pars without white spaces, if any, if all are valid
  #   otherwise stop reporting error
  if (missing(pars)) return(sim$pars_oi)
  allpars <- c(sim$pars_oi, sim$fnames_oi)
  .check_pars(allpars, pars)
}

#' another hacked function
#' @noRd
.check_pars <- function(allpars, pars) {
  pars_wo_ws <- gsub('\\s+', '', pars)
  m <- which(match(pars_wo_ws, allpars, nomatch = 0) == 0)
  if (length(m) > 0)
    stop("no parameter ", paste(pars[m], collapse = ', '))
  if (length(pars_wo_ws) == 0)
    stop("no parameter specified (pars is empty)")
  unique(pars_wo_ws)
}

#' yet another hacked function
#' @noRd
.remove_empty_pars <- function(pars, model_dims) {
  #
  # Remove parameters that are actually empty, which
  # could happen when for exmample a user specify the
  # following stan model code:
  #
  # transformed data { int n; n <- 0; }
  # parameters { real y[n]; }
  #
  # Args:
  #   pars: a character vector of parameters names
  #   model_dims: a named list of the parameter dimension
  #
  # Returns:
  #   A character vector of parameter names with empty parameter
  #   being removed.
  #
  ind <- rep(TRUE, length(pars))
  model_pars <- names(model_dims)
  if (is.null(model_pars)) stop("model_dims need be a named list")
  for (i in seq_along(pars)) {
    p <- pars[i]
    m <- match(p, model_pars)
    if (!is.na(m) && prod(model_dims[[p]]) == 0)  ind[i] <- FALSE
  }
  pars[ind]
}

#' yet another hacked function
#' @noRd
.pars_total_indexes <- function(names, dims, fnames, pars) {
# Obtain the total indexes for parameters (pars) in the
# whole sequences of names that is order by 'column major.'
# Args:
#   names: all the parameters names specifying the sequence of parameters
#   dims:  the dimensions for all parameters, the order for all parameters
#          should be the same with that in 'names'
#   fnames: all the parameter names specified by names and dims
#   pars:  the parameters of interest. This function assumes that
#     pars are in names.
# Note: inside each parameter (vector or array), the sequence is in terms of
#   col-major. That means if we have parameter alpha and beta, the dims
#   of which are [2,2] and [2,3] respectively.  The whole parameter sequence
#   are alpha[1,1], alpha[2,1], alpha[1,2], alpha[2,2], beta[1,1], beta[2,1],
#   beta[1,2], beta[2,2], beta[1,3], beta[2,3]. In addition, for the col-majored
#   sequence, an attribute named 'row_major_idx' is attached, which could
#   be used when row major index is favored.

starts <- .calc_starts(dims)
par_total_indexes <- function(par) {
  # for just one parameter
  #
  p <- match(par, fnames)
  # note that here when `par' is a scalar, it would
  # match one of `fnames'
  if (!is.na(p)) {
    names(p) <- par
    attr(p, "row_major_idx") <- p
    return(p)
  }
  p <- match(par, names)
  np <- .num_pars(dims[[p]])
  if (np == 0) return(NULL)
  idx <- starts[p] + seq(0, by = 1, length.out = np)
  names(idx) <- fnames[idx]
  attr(idx, "row_major_idx") <- starts[p] + .idx_col2rowm(dims[[p]]) - 1
  idx
}
idx <- lapply(pars, FUN = par_total_indexes)
nulls <- sapply(idx, is.null)
idx <- idx[!nulls]
names(idx) <- pars[!nulls]
idx
}

#yet another hacked function
#' @noRd
.calc_starts <- function(dims) {
  len <- length(dims)
  s <- sapply(unname(dims), function(d)  .num_pars(d), USE.NAMES = FALSE)
  cumsum(c(1, s))[1:len]
}

#' yet another hacked function
#' @noRd
.num_pars <- function(d) prod(d)

#' yet another hacked function
#' @noRd
.idx_col2rowm <- function(d) {
# Suppose an iteration of samples for an array parameter is ordered by
# col-major. This function generates the indexes that can be used to change
# the sequences to row-major.
# Args:
#   d: the dimension of the parameter
len <- length(d)
if (0 == len) return(1)
if (1 == len) return(1:d)
idx <- aperm(array(1:prod(d), dim = d))
return(as.vector(idx))
}

#' A wrapper around na_if that also works on factors
#' @noRd
.na_if <- function(x,to_na=NULL,discrete_mods=NULL,pad_id=NULL) {
  
  if(is.null(pad_id)) {
    if(is.factor(x)) {
      levels(x)[levels(x)==to_na] <- NA
    } else {
      x <- na_if(x,to_na)
    }
    return(x)
  } else {
    if(is.factor(x)) {
      levels(x)[levels(x)==to_na] <- NA
    } else {
      x <- na_if(x,to_na)
    }
    pad_id <- ifelse(is.na(x),1L,pad_id)
    return(pad_id)
  }
  
}

#' Calculate priors for Gaussian processes
#' @noRd
.gp_prior <- function(time_points=NULL) {

  # need to calculate minimum and maximum difference between *any* two points
  diff_time <- diff(time_points)
  min_diff <- min(diff_time)+1
  # divide max_diff by 2 to constrain the prior away from very large values
  max_diff <- abs(time_points[1]-time_points[2])*2
  
  # now run the stan program with the data
  
  fit <- sampling(object = stanmodels[['gp_prior_tune']], iter=1, warmup=0, chains=1,
              seed=5838298, algorithm="Fixed_param")
  params <- extract(fit)
  
  return(list(a=c(params$a),
              b=c(params$b)))
  
}

#' Function to calculate IRFs
#' @noRd
.irf <- function( time=1,shock=1,
                  adj_in=NULL,
                  y_1=0,
                  total_t=10,
                  old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1) {
    x_1 <- shock
  } else {
    x_1 <- 0
  }
  
  print(paste0('Now processing time point ',time))
  
  # Calculate current values of y and x given posterior uncertainty
  
  output <- data_frame(y_shock= adj_in*y_1 + x_1,
                       time=time,
                       iter=1:length(adj_in))
  
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    .irf(time=time+1,
         adj_in=adj_in,
         y_1=output$y_shock,
         total_t=total_t,
         old_output=new_output)
  } else {
    return(new_output)  
  }
}

#' Function to create table/matrix of which rows of the data
#' correspond to which model types.
#' @noRd
.count_cats <- function(modelpoints=NULL,
                        billpoints=NULL,
                        Y_int=NULL,
                        discrete=NULL,
                        within_chain=NULL,
                        pad_id=NULL) {

  
  if(within_chain=="none") {
    if(length(Y_int)>1 && any(unique(modelpoints) %in% c(3,4,5,6))) {
      
      # count cats for ordinal models 
      
      get_counts <- group_by(tibble(modelpoints=modelpoints[discrete==1],
                                    billpoints=billpoints[discrete==1],
                                    Y_int),billpoints) %>% 
        group_by(modelpoints,billpoints) %>% 
        summarize(num_cats=length(unique(Y_int))) %>% 
        mutate(num_cats_rat=if_else(modelpoints==3 & num_cats<3,
                                3L,
                                num_cats),
               num_cats_rat=if_else(modelpoints==4 & num_cats<4,
                                3L,
                                num_cats_rat),
               order_cats_rat=as.numeric(factor(num_cats_rat)),
               num_cats_grm=if_else(modelpoints==5 & num_cats<3,
                                    3L,
                                    num_cats),
               num_cats_grm=if_else(modelpoints==6 & num_cats<4,
                                    3L,
                                    num_cats_grm),
               order_cats_grm=as.numeric(factor(num_cats_grm))) 
      
      num_cats_rat <- sort(unique(get_counts$num_cats_rat))
      num_cats_grm <- sort(unique(get_counts$num_cats_grm))
      
      # need to zero out non-present categories
      
      n_cats_rat <- ifelse(3:10 %in% num_cats_rat,3:10,1L)
      n_cats_grm <- ifelse(3:10 %in% num_cats_grm,3:10,1L)
      
      # join the data back together
      
      out_data <- left_join(tibble(modelpoints=modelpoints[discrete==1],
                                   billpoints=billpoints[discrete==1],
                                   Y_int),
                            select(get_counts,
                                   -num_cats_rat,
                                   -num_cats_grm),
                            by=c("modelpoints","billpoints")) 
      out_data$order_cats_grm[is.na(out_data$order_cats_grm)] <- 0L
      out_data$order_cats_rat[is.na(out_data$order_cats_rat)] <- 0L
      
    } else {
      
      out_data <- tibble(order_cats_grm=rep(0L,length(modelpoints[discrete==1])),
                         order_cats_rat=rep(0L,length(modelpoints[discrete==1])))
      n_cats_rat <- rep(1L,length(3:10))
      n_cats_grm <- rep(1L,length(3:10))
    }
  } else {
    if(length(Y_int)>1 && any(unique(modelpoints) %in% c(3,4,5,6))) {
      
      # count cats for ordinal models 
      
      get_counts <- group_by(tibble(modelpoints,
                                    billpoints,
                                    pad_id,
                                    discrete,
                                    Y_int),billpoints) %>% 
        group_by(modelpoints,billpoints) %>% 
        summarize(num_cats=length(unique(Y_int[pad_id==1 & discrete==1]))) %>% 
        mutate(num_cats_rat=if_else(modelpoints==3 & num_cats<3,
                                    3L,
                                    num_cats),
               num_cats_rat=if_else(modelpoints==4 & num_cats<4,
                                    3L,
                                    num_cats_rat),
               order_cats_rat=as.numeric(factor(num_cats_rat)),
               num_cats_grm=if_else(modelpoints==5 & num_cats<3,
                                    3L,
                                    num_cats),
               num_cats_grm=if_else(modelpoints==6 & num_cats<4,
                                    3L,
                                    num_cats_grm),
               order_cats_grm=as.numeric(factor(num_cats_grm))) 
      
      num_cats_rat <- sort(unique(get_counts$num_cats_rat))
      num_cats_grm <- sort(unique(get_counts$num_cats_grm))
      
      # need to zero out non-present categories
      
      n_cats_rat <- ifelse(3:10 %in% num_cats_rat,3:10,1L)
      n_cats_grm <- ifelse(3:10 %in% num_cats_grm,3:10,1L)
      
      # join the data back together
      
      out_data <- left_join(tibble(modelpoints=modelpoints,
                                   billpoints=billpoints,
                                   Y_int),
                            select(get_counts,
                                   -num_cats_rat,
                                   -num_cats_grm),
                            by=c("modelpoints","billpoints")) 
      out_data$order_cats_grm[is.na(out_data$order_cats_grm)] <- 0L
      out_data$order_cats_rat[is.na(out_data$order_cats_rat)] <- 0L
      
      
    } else {
      
      out_data <- tibble(order_cats_grm=rep(0L,length(modelpoints)),
                         order_cats_rat=rep(0L,length(modelpoints)))
      n_cats_rat <- rep(1L,length(3:10))
      n_cats_grm <- rep(1L,length(3:10))
      
    }
  }
  
  return(list(order_cats_rat=out_data$order_cats_rat,
              order_cats_grm=out_data$order_cats_grm,
              n_cats_rat=n_cats_rat,
              n_cats_grm=n_cats_grm))
}

#' Function to figure out how to remove missing values from
#' data before running models.
#' @noRd
.remove_nas <- function(Y_int=NULL,
                        Y_cont=NULL,
                        pad_id=NULL,
                        within_chain=NULL,
                        map_over_id=NULL,
                        discrete=NULL,
                        legispoints=NULL,
                        billpoints=NULL,
                        timepoints=NULL,
                        modelpoints=NULL,
                        ordered_id=NULL,
                        idealdata=NULL,
                        time_ind=NULL,
                        time_proc=NULL,
                        gp_sd_par=NULL,
                        num_diff=NULL,
                        m_sd_par=NULL,
                        min_length=NULL,
                        const_type=NULL,
                        legis_sd=NULL,
                        restrict_sd=NULL,
                        restrict_high=NULL,
                        restrict_low=NULL,
                        ar_sd=NULL,
                        diff_reg_sd=NULL,
                        diff_miss_sd=NULL,
                        discrim_reg_sd=NULL,
                        discrim_miss_sd=NULL,
                        fix_high=NULL,
                        fix_low=NULL) {
  

  # need to determine which missing values should not be considered
  # only remove missing values if non-inflated model is used

  if(length(Y_cont)>1 && !is.na(idealdata@miss_val[2])) {
    if(within_chain=="none") {
      Y_cont <- ifelse(modelpoints[discrete==0] %in% c(10,12),
                       Y_cont,
                       .na_if(Y_cont,as.numeric(idealdata@miss_val[2])))
    } else {
      # this is how to set a missing record if we want a square matrix for map_rect
      pad_id[discrete==0] <- ifelse(modelpoints[discrete==0] %in% c(10,12),
                       pad_id[discrete==0],
                       .na_if(Y_cont[discrete==0],idealdata@miss_val[2],pad_id=pad_id[discrete==0]))
    }
  }

  if(length(Y_int)>1 && !is.na(idealdata@miss_val[1])) {
    if(within_chain=="none") {
      Y_int <- if_else(modelpoints[discrete==1] %in% c(0,2,
                                                      4,
                                                      6,
                                                      8,
                                                      14),
                      Y_int,
                      .na_if(Y_int,idealdata@miss_val[1]))
    } else {
      pad_id[discrete==1] <- if_else(modelpoints[discrete==1] %in% c(0,2,
                                                      4,
                                                      6,
                                                      8,
                                                      14),
                      pad_id[discrete==1],
                      as.numeric(.na_if(Y_int[discrete==1],idealdata@miss_val[1],pad_id=pad_id[discrete==1])))
    }
    
    # need to downward adjust Y_int
    # convert from factor back to numeric as we have dealt with missing data
    # drop unused levels
    Y_int <- as.numeric(factor(Y_int))
    
    if(any(c(1,2) %in% modelpoints)) {
      Y_int <- ifelse(Y_int<3, Y_int - 1L,Y_int)
    }
    
    
    
  }

  # now need to calculate the true remove NAs
  # if within chain, we by definition won't have any NAs
  remove_nas_cont <- !is.na(Y_cont)
  remove_nas_int <- !is.na(Y_int)

  # this works because the data were sorted in id_make
  if(length(Y_cont)>1 && length(Y_int)>1) {
    if(within_chain=="none") {
      remove_nas <- c(remove_nas_int,
                      remove_nas_cont)
    } else {
      remove_nas <- remove_nas_int | remove_nas_cont
    }
    
  } else if(length(Y_cont)>1) {
    remove_nas <- remove_nas_cont
  } else {
    remove_nas <- remove_nas_int
  }
   
  
  if(within_chain=="none") {
    if(length(Y_cont)>1) {
      Y_cont <- Y_cont[remove_nas_cont]
      N_cont <- length(Y_cont)
    } else {
      N_cont <- 0
      Y_cont <- array(dim=c(0)) + 0
    }
    
    if(length(Y_int)>1) {
      Y_int <- Y_int[remove_nas_int]
      N_int <- length(Y_int)
    } else {
      N_int <- 0
      Y_int <- array(dim=c(0)) + 0L
    }
    
    N <- as.integer(N_int + N_cont)
    
    legispoints <- legispoints[remove_nas]
    billpoints <- billpoints[remove_nas]
    timepoints <- timepoints[remove_nas]
    modelpoints <- modelpoints[remove_nas]
    ordered_id <- ordered_id[remove_nas]
    discrete <- discrete[remove_nas]
    pad_id <- pad_id[remove_nas]
    
    # no padding necessary
    
    # Need to recode Y_int to adjust binary responses (should be 0/1)
    Y_int <- as.numeric(Y_int)
    
    # Y_int[modelpoints[discrete==1] %in% c(1,2)] <- if_else(Y_int[modelpoints[discrete==1] %in% c(1,2)] %in% c(1,2),
    #                                                        Y_int[modelpoints[discrete==1]  %in% c(1,2)] - 1L,
    #                                                        Y_int[modelpoints[discrete==1]  %in% c(1,2)])
    
    if(any(unique(modelpoints) %in% c(1,13)) && length(table(Y_int[modelpoints %in% c(1,13)]))>3) {
      stop('Too many values in score matrix for a binary model. Choose a different model_type.')
    } else if(any(unique(modelpoints) %in% c(2,14)) && length(table(Y_int[modelpoints %in% c(2,14)]))>4) {
      stop("Too many values in score matrix for a binary model. Choose a different model_type.")
    }
    
    # use zero values for map_rect stuff
    
    all_int_array <- array(dim=c(0,0)) + 0L
    all_cont_array <- array(dim=c(0,0)) + 0L
    
    Y_cont_map <- 0
    N_cont_map <- 0
    Y_int_map <- 0
    N_int_map <- 0
    N_cont_map <- 0
    N_map <- 0
    
    # create covariates
    
    legis_pred <- as.matrix(select(idealdata@score_matrix,
                                      idealdata@person_cov))[remove_nas,,drop=F]
    
    srx_pred <- as.matrix(select(idealdata@score_matrix,
                                 idealdata@item_cov))[remove_nas,,drop=F]
    
    sax_pred <- as.matrix(select(idealdata@score_matrix,
                                 idealdata@item_cov_miss))[remove_nas,,drop=F]
    
    LX <- length(idealdata@person_cov)
    SRX <- length(idealdata@item_cov)
    SAX <- length(idealdata@item_cov_miss)
    if(!is.infinite(max(Y_int))) {
      y_int_miss <- max(Y_int)
    } else {
      y_int_miss <- 0
    }
    
    if(!is.infinite(max(Y_cont))) {
      y_cont_miss <- max(Y_cont)
    } else {
      y_cont_miss <- 0
    }
    
  } else {
    
    # we just want to add missing values to the pad_id
    # leave everything the same length

    pad_id[!remove_nas] <- 1
    
    if(length(Y_cont)>1) {
      Y_cont <- Y_cont
      N_cont <- length(Y_cont)
      N <- N_cont
    } else {
      N_cont <- array(0)
      Y_cont <- 0
    }
    
    if(length(Y_int)>1) {
      Y_int <- Y_int
      N_int <- length(Y_int)
      N <- N_int
    } else {
      N_int <- array(0L)
      Y_int <- 0
    }
    
    
    
    # now replace all the values with 0 and switch to map_rect IDs

    Y_cont_map <- Y_cont
    N_cont_map <- N_cont
    Y_int_map <- Y_int
    N_int_map <- N_int
    N_cont_map <- N_cont
    N_map <- as.integer(N_int + N_cont)
    
    Y_cont <- array(dim=0)
    N_cont <- 0L
    Y_int <- array(dim=0)
    N_int <- 0L
    N <- 0L
    
    # Need to recode Y_int to adjust binary responses (should be 0/1)
    Y_int_map <- as.numeric(Y_int_map)
    
    # if((max(Y_int_map[modelpoints==1])>1 && !is.infinite(max(Y_int_map[modelpoints==1]))) || (max(Y_int_map[modelpoints==2])>2 && !is.infinite(max(Y_int_map[modelpoints==2])))) {
    #   Y_int_map[modelpoints[discrete==1] %in% c(1,2)] <- Y_int_map[modelpoints[discrete==1]  %in% c(1,2)] - 1L
    # }
  }

  max_t <- max(timepoints,na.rm=T)
  num_bills <- max(billpoints,na.rm=T)
  num_legis <- max(legispoints)
  
  if(any(c(5,6) %in% modelpoints)) {
    num_bills_grm <- num_bills
  } else {
    num_bills_grm <- 0L
  }
  
  if(any(11 %in% modelpoints)) {
    num_ls <- num_legis
  } else {
    num_ls <- 0L
  }
  
  
  
  
  # now need to determine number of categories

  # need to calculate number of categories for ordinal models
  if(within_chain=="none") {
    order_cats_rat <- ordered_id[discrete==1 & remove_nas]
    order_cats_grm <- ordered_id[discrete==1 & remove_nas]
    
    if(any(modelpoints %in% c(3,4))) {
      n_cats_rat <- unique(order_cats_rat)
    } else {
      n_cats_rat <- 0
    } 
    
    if(any(modelpoints %in% c(5,6))) {
      n_cats_grm <- unique(order_cats_grm)
    } else {
      n_cats_grm <- 0
    }
    
  } else {
    
    order_cats_rat <- ordered_id
    order_cats_grm <- ordered_id
    
    if(any(modelpoints %in% c(3,4))) {
      n_cats_rat <- unique(order_cats_rat[pad_id==0])
    } else {
      n_cats_rat <- 0
    }
    
    if(any(modelpoints %in% c(5,6))) {
      n_cats_grm <- unique(order_cats_grm[pad_id==0])
    } else {
      n_cats_grm <- 0
    }
  }
  
  n_cats_rat <- sapply(3:10,function(s) {
    if(s %in% n_cats_rat) {
      s
    } else {
      1
    }
  })
  
  n_cats_grm <- sapply(3:10,function(s) {
    if(s %in% n_cats_grm) {
      s
    } else {
      1
    }
  })
  

  # now stratify everything by number of shards = number of items

  if(within_chain!="none") {

    # Y_int_map will rep at 0 if length 1

    to_shards_int <- tibble(Y_int_map,
                          legispoints,
                          billpoints,
                          timepoints,
                          modelpoints,
                          order_cats_rat,
                          order_cats_grm,
                          pad_id,
                          discrete)
    
    if(Y_cont_map[1]==0) {
      Y_cont_map <- rep(Y_cont_map,nrow(idealdata@score_matrix))
    }
    
    if(length(time_ind)==1) {
      tibble_time <- tibble(time_ind=rep(time_ind,nrow(idealdata@score_matrix)))
    } else {
      
      # need a different kind of time_ind
      
      time_ind <- switch(class(idealdata@score_matrix$time_id)[1],
                         factor=as.numeric(idealdata@score_matrix$time_id),
                         Date=as.numeric(idealdata@score_matrix$time_id),
                         POSIXct=as.numeric(idealdata@score_matrix$time_id),
                         POSIXlt=as.numeric(idealdata@score_matrix$time_id),
                         numeric=idealdata@score_matrix$time_id,
                         integer=idealdata@score_matrix$time_id)
      
      tibble_time <- tibble(time_ind=time_ind)
    }

    to_shards_cont <- tibble(Y_cont_map) %>% 
      bind_cols(select(idealdata@score_matrix,idealdata@person_cov),
                select(idealdata@score_matrix,idealdata@item_cov),
                select(idealdata@score_matrix,idealdata@item_cov_miss),
                tibble_time)
    
    # create max values
    if(sum(discrete==1 & pad_id==0)>0) {
      y_int_miss <- max(to_shards_int$Y_int_map[discrete==1 & pad_id==0])
    } else {
      y_int_miss <- 0
    }
    if(sum(discrete==0 & pad_id==0)>0) {
      y_cont_miss <- max(to_shards_cont$Y_cont_map[discrete==0 & pad_id==0])
    } else {
      y_cont_miss <- 0
    }
    
    
    
    if(map_over_id=="persons") {
      to_shards_int <- to_shards_int %>% 
        gather(key = "variable",value="index",-legispoints)
      
      to_shards_int_split <- to_shards_int %>% 
        split(f=to_shards_int$legispoints) %>% 
        lapply(function(d) d$index)
      
      to_shards_int_array <- abind::abind(to_shards_int_split,along=2)
      
      to_shards_cont$legispoints <- legispoints
      
      to_shards_cont <- to_shards_cont %>% 
        gather(key = "variable",value="index",-legispoints)
      
      to_shards_cont_split <- to_shards_cont %>% 
        split(f=to_shards_cont$legispoints) %>% 
        lapply(function(d) d$index)
      
      to_shards_cont_array <- abind::abind(to_shards_cont_split,along=2)
      
    } else {
      to_shards_int <- to_shards_int %>% 
        gather(key = "variable",value="index",-billpoints)
      
      to_shards_int_split <- to_shards_int %>% 
        split(f=to_shards_int$billpoints) %>% 
        lapply(function(d) d$index)
      
      to_shards_int_array <- abind::abind(to_shards_int_split,along=2)
      
      to_shards_cont$billpoints <- billpoints
      
      to_shards_cont <- to_shards_cont %>% 
        gather(key = "variable",value="index",-billpoints)
      
      to_shards_cont_split <- to_shards_cont %>% 
        split(f=to_shards_cont$billpoints) %>% 
        lapply(function(d) d$index)
      
      to_shards_cont_array <- abind::abind(to_shards_cont_split,along=2)
    }
    
    # create matrix of IDs to pass along with data
    K <- ncol(to_shards_int_array)

    all_int_array <- rbind(matrix(c(rep(as.numeric(map_over_id=="persons"),K),
                                    rep(nrow(to_shards_cont_array)/length(unique(to_shards_cont$variable)),K),# pass along shard size for N
                                    rep(nrow(to_shards_int_array)/length(unique(to_shards_int$variable)),K), # pass along shard size for N
                                    rep(y_int_miss,K),
                                    rep(y_cont_miss,K),
                                     rep(max(legispoints),K),
                                     rep(max(billpoints),K),
                                    rep(num_ls,K),
                                    rep(max(timepoints),K),
                                    rep(length(unique(modelpoints)),K),
                                    rep(length(idealdata@person_cov),K),
                                    rep(length(idealdata@item_cov),K),
                                    rep(length(idealdata@item_cov_miss),K),
                                    rep(n_cats_rat,each=K),
                                    rep(n_cats_grm,each=K),
                                    c(1:K),
                                    rep(time_proc,K),
                                    rep(const_type,K),
                                    rep(restrict_high,K),
                                    rep(restrict_low,K),
                                    rep(fix_high,K),
                                    rep(fix_low,K)),ncol=K,byrow = T),
                            to_shards_int_array)
    
    all_int_array <- apply(all_int_array,2,as.integer)
    
    # add additional values for real array
    
    Q <- ncol(to_shards_cont_array)
    
    all_cont_array <- rbind(to_shards_cont_array,
                            matrix(c(rep(ar_sd,Q),
                                     rep(diff_reg_sd,Q),
                                     rep(diff_miss_sd,Q),
                                     rep(discrim_reg_sd,Q),
                                     rep(discrim_miss_sd,Q),
                                     rep(legis_sd,Q),
                                     rep(restrict_sd,Q),
                                     rep(gp_sd_par,Q),
                                     rep(m_sd_par,Q),
                                     rep(num_diff,Q),
                                     rep(min_length,Q)),ncol=Q,byrow=T))
    
    # empty values for covariates
    # add zero to avoid converting to logical matrix
    
    LX <- length(idealdata@person_cov)
    SRX <- length(idealdata@item_cov)
    SAX <- length(idealdata@item_cov_miss)
    
    legis_pred <- array(dim=c(0,0)) + 0L
    
    srx_pred <- array(dim=c(0,0)) + 0L
    
    sax_pred <- array(dim=c(0,0)) + 0L
    
    # need to zero out IDs
    
    legispoints <- array(dim=0) + 0L
    modelpoints <- array(dim=0) + 0L
    timepoints <- array(dim=0) + 0L
    billpoints <- array(dim=0) + 0L
    order_cats_grm <- array(dim=0) + 0L
    order_cats_rat <- array(dim=0) + 0L
  }
  
    return(list(Y_int=Y_int,
                Y_cont=Y_cont,
                pad_id=pad_id,
                legispoints=legispoints,
                billpoints=billpoints,
                timepoints=timepoints,
                modelpoints=modelpoints,
                remove_nas=remove_nas,
                all_int_array=t(all_int_array),
                to_shards_cont_array=t(all_cont_array),
                N_map=N_map,
                Y_int_map=Y_int_map,
                Y_cont_map=Y_cont_map,
                N_int_map=N_int_map,
                N_cont_map=N_cont_map,
                N=N,
                y_cont_miss=y_cont_miss,
                y_int_miss=y_int_miss,
                discrete=discrete,
                max_t=max_t,
                num_bills=num_bills,
                num_legis=num_legis,
                num_ls=num_ls,
                num_bills_grm=num_bills_grm,
                N_cont=N_cont,
                N_int=N_int,
                order_cats_rat=order_cats_rat,
                order_cats_grm=order_cats_grm,
                n_cats_rat=n_cats_rat,
                n_cats_grm=n_cats_grm,
                legis_pred=legis_pred,
                srx_pred=srx_pred,
                sax_pred=sax_pred,
                LX=LX,
                SRX=SRX,
                SAX=SAX))
  
  
}

# Need functions to calculate predicted outcomes

#' Bernoulli
#' @noRd
.cov_bern <- function(lin_val=NULL,...) {
  
  mean(plogis(lin_val)-0.5)
  
}

#' Ordinal outcomes
#' @noRd
.cov_ord <- function(lin_val=NULL,
                      covp=NULL,
                      K=null,
                      ...) {
  if(K==1) {
    1 - mean(plogis(lin_val - covp[,1]))
  } else if(K>1 && K<K) {
    mean(plogis(lin_val - covp[,1]) - plogis(lin_val - covp[,2]))
  } else {
    plogis(lin_val - covp[,1])
  }
  
  
}

#' Poisson
#' @noRd
.cov_pois <- function(lin_val,...) {
  mean(exp(lin_val))
}

#' Normal
#' @noRd
.cov_norm <- function(lin_val,...) {
  mean(lin_val)
}

#' Log-Normal
#' @noRd
.cov_lnorm <- function(lin_val,...) {
  mean(exp(lin_val))
}

#' Latent-Space
#' @noRd
.cov_latsp <- function(lin_val,...) {
  mean(plogis(lin_val)-0.5)
}

#' How to find cutpoints for id_plot_cov function
#' @noRd
.get_cuts_cov <- function(k=NULL,
                          m=NULL,
                          i=NULL,
                          sigma_all=NULL,
                          K=NULL,
                          obj=NULL,
                          these_items=NULL) {
  
  
  if(m %in% c(3,4)) {
    
    # easy peesy, just get the right intercept for k
    
    if(k==1) {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k,"]"))[[1]])
    } else if(k>1 && k<K) {
      cutp <- cbind(as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k-1,"]"))[[1]]),
                    as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k,"]"))[[1]]))
    } else {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k-1,"]"))[[1]])
    }
    
    return(cutp)
    
  } else {
    
    # need to determine the right graded response intercept based on sigma_all then return the cutpoint
    item_num <- these_items[i]
    if(k==1) {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("grm_steps_votes",K,"[",item_num,",",k,"]"))[[1]])
    } else if(k>1 && k<K) {
      cutp <- cbind(as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k-1,"]"))[[1]]),
                    as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k,"]"))[[1]]))
    } else {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k-1,"]"))[[1]])
    }
  }
  
  return(cutp)
}

#' Function to square data for map_rect
#' @noRd
.pad_data <- function(this_data,map_over_id=NULL,use_groups=FALSE) {
  
  # if we have duplicate observations by group/person, need to pad some more
  # need to add a counter to deal with unique rows issues (need one unique row per ID)
  
  
  
  if(use_groups) {
    
    this_data <- group_by(this_data,group_id,item_id,time_id) %>% 
      mutate(unique_row=1:n())
    
    this_data <- tidyr::complete(ungroup(this_data),group_id,item_id,time_id,unique_row) %>% 
      select(-unique_row)
    
  } else {
    
    if(nrow(distinct(this_data,person_id,item_id,time_id,.keep_all=T))!=nrow(this_data)) {
      warning("You may have duplicate observations by person_id, time_id and item_id in your data.")
    }
    
    this_data <- group_by(this_data,person_id,item_id,time_id) %>% 
      mutate(unique_row=1:n())
    
    this_data <- tidyr::complete(ungroup(this_data),person_id,item_id,time_id,unique_row) %>% 
      select(-unique_row)
    
  }
  
  # now need a marker and replace NAs with 0
  
  this_data$pad_id <- as.numeric(is.na(this_data$model_id))
  
  this_data <- lapply(this_data, function(c) {

    if(is.factor(c)) {
      c <- addNA(c)
      levels(c)[length(levels(c))] <- levels(c)[length(levels(c))-1]
    } else if(is.Date(c) || is.POSIXct(c) || is.POSIXlt(c)) {
      
      c[is.na(c)] <- min(c,na.rm=T)
      
      } else {
      c[is.na(c)] <- 0
    }
    return(c)
  }) %>% dplyr::as_tibble(.)
  
  return(this_data)
  
}