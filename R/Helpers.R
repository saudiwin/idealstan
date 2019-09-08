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
    items <- apply(this_params[,grepl(pattern = 'sigma_free',x=all_params)],2,mean)

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
      
      if(restrict_var) {
        if(time_proc==4) {
          return(list(L_full = L_full,
                      sigma_reg_free=sigma_reg_free,
                      m_sd=rep(m_sd_par,num_legis),
                      time_var_gp_free=log(rep(num_diff*time_range,num_legis-1)),
                      L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                      time_var_free = rexp(rate=1/time_sd,n=num_legis-1)))
        } else {
          return(list(L_full = L_full,
                      sigma_reg_free=sigma_reg_free,
                      L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                      time_var_free = rexp(rate=1/time_sd,n=num_legis-1)))
        }
        
      } else {
        return(list(L_full = L_full,
                    sigma_reg_free=sigma_reg_free,
                    L_AR1 = array(runif(n = num_legis,min = -.5,max=.5)),
                    time_var_free = rexp(rate=1/time_sd,n=num_legis-1)))
      }
      
    } else {
      return(list(L_full = L_full,
                  time_var_free = rexp(rate=1/time_sd,n=num_legis-1),
                  sigma_reg_free=sigma_reg_free,
                  L_AR1 = array(runif(n = num_legis,min = -.5,max=.5))))
    }

  } else {
    #identification run
    return(list(L_full = rnorm(n=num_legis,mean=0,sd=person_sd),
                sigma_reg_free=sigma_reg_free,
         L_AR1 = runif(n = num_legis,min = -.5,max=.5)))
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
.na_if <- function(x,to_na=NULL) {
  if(is.factor(x)) {
    levels(x)[levels(x)==to_na] <- NA
  } else {
    x <- na_if(x,to_na)
  }
  return(x)
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

