#' Simulate IRT ideal point data
#' 
#' A function designed to simulate absence-inflated data with either binary or ordinal outcomes.
#' 
#' This function produces simulated data that matches (as closely as possible) the models
#' used in the underlying Stan code. Currently the simulation can produce absence-inflated and
#' non-absence-inflated binary and ordinal ideal point models.
#' 
#' @param num_person The number of persons/personlators
#' @param num_bills The number of items/bills
#' @param model_type One of \code{'binary'}, \code{'ordinal-rating'}, \code{'ordinal-grm'} or \code{'poisson'}
#' @param absence_discrim_sd The SD of the discrimination parameters for the absence model
#' @param absence_diff_mean The mean intercept for the absence model; increasing it will lower the total number of
#' absences
#' @param reg_discrim_sd The SD of the discrimination parameters for the non-inflated model
#' @param diff_sd The SD of the difficulty parameters (bill/item intercepts)
#' @param time_points The number of time points for time-varying legislator/person parameters
#' @param time_process The process used to generate the ideal points: currently either \code{'random'} 
#' for a random walk or \code{'AR'} for an AR1 process
#' @param ideal_pts_sd The SD for the person/personlator ideal points
#' @param prior_type The statistical distribution that generates the data. Currently only 
#' 'gaussian' is supported.
#' @param ordinal_outcomes If \code{model} is \code{'ordinal'}, an integer giving the total number of categories
#' @param absence If \code{TRUE}, an absence-inflated dataset is produced.
#' @return The results is a \code{idealdata} object that can be used in the 
#' \code{\link{id_estimate}} function to run a model. It can also be used in the simulation
#' plotting functions.
#' @seealso \code{\link{id_plot_sims}} for plotting fitted models versus true values.
#' @export
id_sim_gen <- function(num_person=20,num_bills=50,
                       model_type='binary',absence_discrim_sd=2,absence_diff_mean=0.5,
                             reg_discrim_sd=2,diff_sd=.25,
                            time_points=1,
                            time_process='random',
                             ideal_pts_sd=1,prior_type='gaussian',ordinal=TRUE,ordinal_outcomes=3,
                             graded_response=FALSE,absence=TRUE) {
  
  # Allow for different type of distributions for ideal points

  if(prior_type=='gaussian') {
    
    prior_func <- function(params) {
      
      output <- rnorm(n=params$N,mean=params$mean,sd=params$sd)
      
    }
    
  } else if(prior_type=='log-normal') {
    prior_func <- function(params) {
      
      output <- rlnorm(n=params$N,meanlog=params$mean,sdlog=params$sd)
      
    }
  } else if(prior_type=='exponential') {
    prior_func <- function(params) {
      
      output <- rexp(n=params$N,rate=params$mean)
      
    }
  }
  
  # First simulate ideal points for person/legislators/bills
  # Bill difficulty parameters are fixed because they are not entirely interesting (they represent intercepts)
  
  absence_diff <- prior_func(params=list(N=num_bills,mean=absence_diff_mean,sd=diff_sd)) 
  #Discrimination parameters more important because they reflect how much information a bill contributes
  # need to make some of them negative to reflect the switching nature of policies
  #absence_discrim <- prior_func(params=list(N=num_bills,mean=1,sd=absence_discrim_sd)) * if_else(runif(num_bills-1)>0.5,1,-1)
  absence_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=absence_discrim_sd))
  # person ideal points common to both types of models (absence and regular)

  if(time_points==1) {
    ideal_pts <- as.matrix(prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd)))
  } else if(time_points>1) {
    # if more than 1 time point, generate via an AR or random walk process
    if(time_process=='AR') {
      ideal_t1 <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      # random AR parameters
      ar_adj <- runif(n = num_person,min = -0.5,max=0.5)
      # drift parameters
      drift <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      
      ideal_pts <- lapply(1:num_person, function(i) {
        this_person <- .gen_ts_data(t=time_points,
                                    adj_in=ar_adj[i],
                                    alpha_int=drift[i],
                                    sigma=ideal_pts_sd,
                                    init_sides=ideal_t1[i])
        return(this_person)
      }) %>% bind_cols %>% as.matrix
    } else if(time_process=='random') {
      ideal_t1 <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      
      ideal_pts <- lapply(1:num_person, function(i) {
        this_person <- .gen_ts_data(t=time_points,
                                    adj_in=1,
                                    alpha_int=0,
                                    sigma=ideal_pts_sd,
                                    init_sides=ideal_t1[i])
        return(this_person)
      }) %>% bind_cols %>% as.matrix
      
    } else {
      stop('Incorrect time process specified.')
    }
  }
  
  # First generate prob of absences

  person_points <- rep(1:num_person,times=num_bills)
  bill_points <- rep(1:num_bills,each=num_person)

  # generate time points
  
  if((num_bills %% time_points)!=0) stop('Total number of time points must be a multiple of the number of bills/items.')
  time_points <- rep(1:time_points,each=num_bills/time_points)
  time_points <- time_points[bill_points]
    
  pr_absence <- sapply(1:length(person_points),function(n) {
    ideal_pts[time_points[n],person_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
    }) %>% plogis()
  
  reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=diff_sd))
  reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd))
  
  # this is the same for all DGPs
  
  pr_vote <- sapply(1:length(person_points),function(n) {
    ideal_pts[time_points[n],person_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
  }) %>% plogis()
    
  # now pick a DGP function and run it
  
  run_func <- switch(model_type,binary=.binary,
                     `ordinal-ratingscale`=.ord_rating,
                     `ordinal-grm`=.ord_grm,
                     `poisson`=.poisson)
  
  outobj <- run_func(pr_absence=pr_absence,
           pr_vote=pr_vote,
           N=length(person_points),
           ordinal_outcomes=ordinal_outcomes,
           absence=absence,
           num_person=num_person,
           num_bills=num_bills,
           time=rep(1:max(time_points),each=num_bills/max(time_points)))
  
  outobj@simul_data <- list(num_person=num_person,
                                       num_bills=num_bills,
                                       absence_discrim_sd=absence_discrim_sd,
                                       absence_diff_mean=absence_diff_mean,
                                       reg_discrim_sd=reg_discrim_sd,
                                       ideal_pts_sd=ideal_pts_sd,
                                       prior_func=prior_func,
                                       ordinal=ordinal,
                                       ordinal_outcomes=ordinal_outcomes,
                                       graded_response=graded_response,
                                       true_person=ideal_pts,
                                       true_reg_discrim=reg_discrim,
                                       true_abs_discrim=absence_discrim)

  outobj@person_data <- data_frame(person.names=paste0('person_',1:nrow(outobj@score_matrix)),
                                               group='L')
  
  outobj@simulation <- TRUE
  
  return(outobj)
  
  # Estimate prob of people voting on a bill (yes/no/abstain), then deflate that by the probability
  # of absence
  
  if(ordinal==TRUE & graded_response==FALSE & absence==TRUE) {
    
    # Need to simulate a separate prob for each outcome category
    # I only simulate 3 outcome categories by default as that is what you find in personlatures
    # Standard model for ordinal outcomes is the rating-scale model, also called a "divide by total"
    # ordinal logit/categorical logit
    
    # Now create cutpoints that are equally spaced in the ideal point space
    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
    cutpoints <- cutpoints[2:(length(cutpoints)-1)]
  
    #Generate outcomes by personlator
    
    cuts <- sapply(cutpoints,function(y) {
      pr_vote - y
    },simplify='array')


    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    votes <- sapply(1:nrow(cuts), function(i) {
      this_cut <- cuts[i,]
      
      pr_bottom <- 1 - plogis(this_cut[1])
      
      mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
        plogis(this_cut[c]) - plogis(this_cut[c+1])
      })
      
      pr_top <- plogis(this_cut[length(this_cut)])
      
      return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
    })
    
  
    # now determine if the outcome. personlators only vote if they show up
    # Absences are coded as category 4
    
    combined <- if_else(pr_absence<runif(length(person_points)),votes,as.integer(ordinal_outcomes)+1L)
    
    # Create a vote matrix
    
    combined <- matrix(combined,ncol=num_bills,nrow=num_person,byrow = F)
    
    #Got the vote matrix, run ideal_data
    
    colnames(combined) <- paste0('Vote_',1:ncol(combined))
    row.names(combined) <- paste0('person_',1:nrow(combined))
    
    out_data <- id_make(score_data=combined,person_data=data_frame(person.names=paste0('person_',1:nrow(combined)),
                                                                        group='L',
                                                                        true_person=as.numeric(ideal_pts)),
                               miss_val = ordinal_outcomes+1,
                               high_val = ordinal_outcomes,
                               low_val = 1,
                               middle_val = 2:(ordinal_outcomes-1),
                              inflate=absence,
                               simul_data=list(num_person=num_person,
                                              num_bills=num_bills,
                                              absence_discrim_sd=absence_discrim_sd,
                                              absence_diff_mean=absence_diff_mean,
                                              reg_discrim_sd=reg_discrim_sd,
                                              ideal_pts_sd=ideal_pts_sd,
                                              prior_func=prior_func,
                                              ordinal=ordinal,
                                              ordinal_outcomes=ordinal_outcomes,
                                              graded_response=graded_response,
                                              true_person=ideal_pts,
                                              true_reg_discrim=reg_discrim,
                                              true_abs_discrim=absence_discrim),
                               simulation=TRUE)
    
  } else if(ordinal==TRUE & graded_response==TRUE & absence==TRUE) {
    stop('Graded response model not currently implemented in idealstan.')
    reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd)) %>% as.matrix
    
    all_outcomes <- lapply((1-ordinal_outcomes),function(x) {
      
      
      reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=1))
      
      pr_vote <-t(t(ideal_pts %*% t(reg_discrim))-reg_diff)
      
    })
    
  } else if(ordinal==FALSE & graded_response==FALSE & absence==FALSE) {
    
    
  } else if(ordinal==TRUE & graded_response==FALSE & absence==FALSE) {
    
    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
    cutpoints <- cutpoints[2:(length(cutpoints)-1)]
    
    #Generate outcomes by personlator
    
    cuts <- sapply(cutpoints,function(y) {
      pr_vote - y
    },simplify='array')
    
    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    votes <- sapply(1:nrow(cuts), function(i) {
      this_cut <- cuts[i,]
      
      pr_bottom <- 1 - plogis(this_cut[1])
      
      mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
        plogis(this_cut[c]) - plogis(this_cut[c+1])
      })
      
      pr_top <- plogis(this_cut[length(this_cut)])
      
      return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
    })
    
    
    # now determine if the outcome. personlators only vote if they show up
    # Absences are coded as category 4
    
    combined <- votes
    
    # Create a vote matrix
    
    combined <- matrix(combined,ncol=num_bills,nrow=num_person,byrow = F)
    
    #Got the vote matrix, run ideal_data
    
    colnames(combined) <- paste0('Vote_',1:ncol(combined))
    row.names(combined) <- paste0('person_',1:nrow(combined))
    
    out_data <- id_make(score_data=combined,person_data=data_frame(person.names=paste0('person_',1:nrow(combined)),
                                                                        group='L',
                                                                        true_person=as.numeric(ideal_pts)),
                               miss_val = ordinal_outcomes+1,
                               high_val = ordinal_outcomes,
                               low_val = 1,
                               inflate=absence,
                               middle_val = 2:(ordinal_outcomes-1),
                               simul_data=list(num_person=num_person,
                                               num_bills=num_bills,
                                               absence_discrim_sd=absence_discrim_sd,
                                               absence_diff_mean=absence_diff_mean,
                                               reg_discrim_sd=reg_discrim_sd,
                                               ideal_pts_sd=ideal_pts_sd,
                                               prior_func=prior_func,
                                               ordinal=ordinal,
                                               ordinal_outcomes=ordinal_outcomes,
                                               graded_response=graded_response,
                                               true_person=ideal_pts,
                                               true_reg_discrim=reg_discrim,
                                               true_abs_discrim=absence_discrim),
                               simulation=TRUE)
  } else if(ordinal==F && graded_response==F && absence==T) {
    
  }
  
  return(out_data)
  
  
}

#' A function that loops over numbers of personlators/bills to provide a coherent over-view of 
#' idealstan performance for a given model type.
.id_sim_test <- function(param_range=c(50,150),by=10,simul_type='absence',is.ordinal=TRUE,
                           restrict_type='constrain_twoway',restrict_params='person',
                           num_constrain=10,fixtype='pinned',...) {

  #check for inconsistent parameters
  
  if(num_constrain>(min(param_range)-1)) {
    stop('Please do not select num_constrain greater than the minimum of param_range.')
  }
  
  if(simul_type=='absence') {
    simul_func <- id_sim_gen
    if(is.ordinal==TRUE) {
      model_type <- 4
    } else {
      model_type <- 2
    }
    absence <- T
  }
  full_range <- seq(param_range[1],param_range[2],by=by)
  all_sims <- lapply(full_range, function(N){
    sim_data <- simul_func(num_person=N,num_bills=N,ordinal=is.ordinal,absence=absence)

    true_param <- switch(restrict_params,
                             person=sim_data@simul_data$true_person,
                             discrim_reg=sim_data@simul_data$true_reg_discrim,
                             discrim_miss=sim_data@simul_data$true_abs_discrim)
  
    high_par <- sort(true_param,decreasing=TRUE,index.return=TRUE)$ix[1:num_constrain]
    low_par <- sort(true_param,index.return=TRUE)$ix[1:num_constrain]
    high_par_est <- sort(true_param,decreasing=TRUE,index.return=TRUE)$x[1:num_constrain]
    low_par_est <- sort(true_param,index.return=TRUE)$x[1:num_constrain]
    return(list(sim_data=sim_data,
                high_par=high_par,
                low_par=low_par,
                high_par_est=high_par_est,
                low_par_est=low_par_est))
    browser()
  })
  
  
  
  #See if this works
  
  est_models <- lapply(all_sims,function(m,...) {

    id_estimate(m$sim_data,
                   use_vb=FALSE,
                   restrict_ind_high=c(m$high_par,m$low_par),
                   pin_vals = c(m$high_par_est,m$low_par_est),
                   fixtype=fixtype,
                   restrict_params=restrict_params,
                   restrict_type=restrict_type,
                   ...)
    
    },...)

  est_models_vb <-  lapply(all_sims,function(m,...) {
    
    id_estimate(m$sim_data,
                   use_vb=TRUE,
                   restrict_ind_high=c(m$high_par,m$low_par),
                   pin_vals = c(m$high_par_est,m$low_par_est),
                   fixtype=fixtype,
                   restrict_params=restrict_params,
                   restrict_type=restrict_type,
                   ...)
    
  },...)


  est_models_cov <- lapply(1:length(est_models),function(i) {
    id_sim_coverage(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
    }) %>% bind_rows
  est_models_vb_cov <- lapply(1:length(est_models),function(i) {
    id_sim_coverage(est_models_vb[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_rmse <- lapply(1:length(est_models),function(i) {
    id_sim_rmse(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_vb_rmse <- lapply(1:length(est_models),function(i) {
    id_sim_rmse(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_resid <- lapply(1:length(est_models),function(i) {
    id_sim_resid(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_vb_resid <- lapply(1:length(est_models),function(i) {
    id_sim_resid(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  
  
  
  
  return(list(est_models=bind_rows(est_models_cov,est_models_resid,est_models_rmse),
         est_models_vb=bind_rows(est_models_vb_cov,est_models_vb_resid,est_models_rmse)))
  
}

#' RMSE function for calculating individual RMSE values compared to true simulation scores
#' Returns a data frame with RMSE plus quantiles.
#' @param obj A fitted \code{idealstan} object with true data from \code{\link{id_sim_gen}}
#' @param rep Over how many replicates to calculate RMSE? Currently can only be 1
#' @export
id_sim_rmse <- function(obj,rep=1) {
  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@score_data@simul_data
  
  true_person <- all_true$true_person[as.numeric(row.names(obj@score_data@score_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  
  over_params <- function(est_param,true_param) {
  
  if(class(est_param)=='array') {
    param_length <- dim(est_param)[3]
    all_rmse <- sapply(1:param_length, function(i) {
      this_param <- sqrt((est_param[,,i] - true_param[i])^2)
    })
  } else if(class(est_param)=='matrix') {
    param_length <- ncol(est_param)
    all_rmse <- sapply(1:param_length, function(i) {
      this_param <- sqrt((est_param[,i] - true_param[i])^2)
    })
  }
    
    out_data1 <- data_frame(avg=apply(all_rmse,2,mean),
                            high=apply(all_rmse,2,quantile,probs=0.9),
                            low=apply(all_rmse,2,quantile,probs=0.1),
                            total_avg=mean(all_rmse),
                            total_high=quantile(all_rmse,0.9),
                            total_low=quantile(all_rmse,0.1),
                            Params=1:param_length,
                            est_type='RMSE',
                            iter=rep)
    return(out_data1)
  }
  
  
  out_data <- list(`Ideal Points`=over_params(all_params$L_full,true_person),
                   `Absence Discrimination`=over_params(all_params$sigma_abs_full,true_sigma_abs),
                   `Item Discrimination`=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)

}

#' Function that computes how often the true value of the parameter is included within the 
#' 95/5 high posterior density interval
#' @param obj A fitted \code{idealstan} object with true data generated by \code{\link{id_sim_gen}}
#' @param rep How many times the models were fitted on new data, currently can only be 1
#' @param quantiles What the quantile coverage of the high posterior density interval should be
#' @export
id_sim_coverage <- function(obj,rep=1,quantiles=c(.95,.05)) {

  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@score_data@simul_data
  
  true_person <- all_true$true_person[as.numeric(row.names(obj@score_data@score_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  
  over_params <- function(est_param,true_param) {

    if(class(est_param)=='array') {
      param_length <- dim(est_param)[3]
      all_covs <- sapply(1:param_length, function(i) {
        high <- quantile(est_param[,,i],quantiles[1])
        low <- quantile(est_param[,,i],quantiles[2])
        this_sd <- sd(est_param[,,i])
        #this_param <- (true_param[i] < (true_param[i]+1.96*this_sd)) && (true_param[i] > (true_param[i]-1.96*this_sd))
        this_param <- (true_param[i] < high) && (true_param[i] >low)
        
      })
    } else if(class(est_param)=='matrix') {
      param_length <- ncol(est_param)
      all_covs <- sapply(1:param_length, function(i) {
         high <- quantile(est_param[,i],.95)
         low <- quantile(est_param[,i],.05)
        this_sd <- sd(est_param[,i])
        #this_param <- (true_param[i] < (true_param[i]+1.96*this_sd)) && (true_param[i] > (true_param[i]-1.96*this_sd))
        this_param <- (true_param[i] < high) && (true_param[i] > low)
        
      })
    }
    all_covs <- data_frame(avg=as.numeric(all_covs),est_type='Coverage',iter=rep)
    return(all_covs)
  }

  total_iters <- obj@stan_samples@stan_args[[1]]$iter-obj@stan_samples@stan_args[[1]]$warmup
  if(rep<total_iters) {
    to_iters <- sample(1:total_iters,rep)
  } else {
    to_iters <- 1:total_iters
  }
  # Do this when we want to test asymptotic properties of the estimator
  # person_points <- lapply(to_iters,over_params,
  #                         est_param=all_params$L_full,
  #                         true_param=true_person)
  
  out_data <- list(`Person Ideal Points`=over_params(all_params$L_full,true_person),
                         `Absence Discriminations`=over_params(all_params$sigma_abs_full,true_sigma_abs),
                         `Item Discrimations`=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)
}

#' Residual function for checking estimated samples compared to true simulation scores
#' Returns a data frame with residuals plus quantiles.
#' @param obj A fitted \code{idealstan} object with true data from \code{\link{id_sim_gen}}
#' @param rep Over how many replicates to calculate residuals? Currently can only be 1
#' @export
id_sim_resid <- function(obj,rep=1) {
  
  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@score_data@simul_data
  
  true_person <- all_true$true_person[as.numeric(row.names(obj@score_data@score_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@score_data@score_matrix))]
  
  over_params <- function(est_param,true_param) {

  if(class(est_param)=='array') {
    param_length <- dim(est_param)[3]
    all_resid <- sapply(1:param_length, function(i) {
      this_param <- (est_param[,,i] - true_param[i])
    })
  } else if(class(est_param)=='matrix') {
    param_length <- ncol(est_param)
    all_resid <- sapply(1:param_length, function(i) {
      this_param <- (est_param[,i] - true_param[i])
    })
  }
  
  
  out_data1 <- data_frame(avg=apply(all_resid,2,mean),
                          high=apply(all_resid,2,quantile,probs=0.9),
                          low=apply(all_resid,2,quantile,probs=0.1),
                          total_avg=mean(all_resid),
                          total_high=quantile(all_resid,0.9),
                          total_low=quantile(all_resid,0.1),
                          Params=1:param_length,
                          est_type='Residuals',
                          iter=rep)
  

    return(out_data1)
  }
  
  out_data <- list(`Ideal Points`=over_params(all_params$L_full,true_person),
                   `Absence Discrimination`=over_params(all_params$sigma_abs_full,true_sigma_abs),
                   `Item Discrimination`=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)

}