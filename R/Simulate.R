#' Simulate IRT ideal point data
#' 
#' A function designed to simulate IRT ideal point data.
#' 
#' This function produces simulated data that matches (as closely as possible) the models
#' used in the underlying Stan code. Currently the simulation can produce inflated and non-inflated
#' models with binary, ordinal (GRM and rating-scale), Poisson, Normal and Log-Normal responses.
#' 
#' @param num_person The number of persons/persons
#' @param num_bills The number of items/bills
#' @param model_type One of \code{'binary'}, \code{'ordinal_rating'}, \code{'ordinal_grm'}, \code{'poisson'}
#' \code{'normal'}, or \code{'lognormal'}
#' @param latent_space Whether to use the latent space formulation of the ideal point model 
#' \code{FALSE} by default. NOTE: currently, the package only has estimation for a 
#' binary response with the latent space formulation.
#' @param absence_discrim_sd The SD of the discrimination parameters for the inflated model
#' @param absence_diff_mean The mean intercept for the inflated model; increasing it will lower the total number of
#' missing data
#' @param reg_discrim_sd The SD of the discrimination parameters for the non-inflated model
#' @param diff_sd The SD of the difficulty parameters (bill/item intercepts)
#' @param time_points The number of time points for time-varying legislator/person parameters
#' @param time_process The process used to generate the ideal points: either \code{'random'} 
#' for a random walk, \code{'AR'} for an AR1 process,
#' or \code{'GP'} for a Gaussian process.
#' @param time_sd The standard deviation of the change in ideal points over time (should be low relative to 
#' \code{ideal_pts_sd})
#' @param ideal_pts_sd The SD for the person/person ideal points
#' @param prior_type The statistical distribution that generates the data. Currently only 
#' 'gaussian' is supported.
#' @param ordinal_outcomes If \code{model} is \code{'ordinal'}, an integer giving the total number of categories
#' @param inflate If \code{TRUE}, an missing-data-inflated dataset is produced.
#' @param sigma_sd If a normal or log-normal distribution is being fitted, this parameter gives the standard 
#' deviation of the outcome (i.e. the square root of the variance).
#' @return The results is a \code{idealdata} object that can be used in the 
#' \code{\link{id_estimate}} function to run a model. It can also be used in the simulation
#' plotting functions.
#' @seealso \code{\link{id_plot_sims}} for plotting fitted models versus true values.
#' @import posterior
#' @export
id_sim_gen <- function(num_person=20,num_bills=50,
                       model_type='binary',
                       latent_space=FALSE,
                       absence_discrim_sd=2,absence_diff_mean=0.5,
                             reg_discrim_sd=2,diff_sd=.25,
                            time_points=1,
                            time_process='random',
                          time_sd=.1,
                             ideal_pts_sd=1,prior_type='gaussian',ordinal_outcomes=3,
                        inflate=FALSE,
                       sigma_sd=1) {
  
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
  
  ideal_pts_mean <- NULL

  if(time_points==1) {
    ideal_pts <- as.matrix(prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd)))
    drift <- 0
    ar_adj <- 0
  } else if(time_points>1) {
    # if more than 1 time point, generate via an AR, random-walk or GP process
    if(time_process=='GP') {
      
      ideal_pts_mean <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      ar_adj <- runif(n=num_person,2.5,5.5) # rho parameter in GPs
      drift <- 0.5 # marginal standard deviation
      simu_data <- list(N=num_person,
                        T=time_points,
                        x=1:time_points,
                        ideal_pts=ideal_pts_mean,
                        rho=ar_adj,
                        alpha=drift,
                        sigma=time_sd)
      # loop over persons and construct GP with stan code
      
      stan_code <- system.file("stan_files","sim_gp.stan",
                            package="idealstan")
      
      simu_mod <- stan_code %>% 
        cmdstan_model(include_paths=dirname(stan_code))
      
      simu_fit <- simu_mod$sample(data=simu_data, iter_sampling=1,
                                  chains=1, fixed_param=T)
      
      ideal_pts <- simu_fit$draws("Y") %>% as_draws_df %>% 
        select(-`.iteration`,-`.draw`,-`.chain`) %>% 
        gather(key="variable",value="value") %>% 
        mutate(person=as.numeric(stringr::str_extract(variable,"(?<=\\[)[0-9]+")),
               time=as.numeric(stringr::str_extract(variable,"(?<=,)[0-9]+"))) %>% 
        select(-variable) %>% 
        spread("time","value") %>% 
        select(-person) %>% 
        as.matrix
      
    } else {
      ideal_t1 <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      if(time_process=='AR') {
        # random AR parameters
        ar_adj <- runif(n = num_person,min = -0.5,max=0.5)
      } else if(time_process=='random') {
        ar_adj <- rep(1,num_person)
      }
      # drift parameters
      drift <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      
      ideal_pts <- lapply(1:num_person, function(i) {
        this_person <- .gen_ts_data(t=time_points,
                                    adj_in=ar_adj[i],
                                    alpha_int=drift[i],
                                    sigma=time_sd,
                                    init_sides=ideal_t1[i])
        return(this_person)
      }) %>% bind_cols %>% as.matrix
      ideal_pts <- t(ideal_pts)
    }
  }
  
  # First generate prob of absences

  person_points <- rep(1:num_person,times=num_bills)
  bill_points <- rep(1:num_bills,each=num_person)

  # generate time points
  
  if((num_bills %% time_points)!=0) stop('Total number of time points must be a multiple of the number of bills/items.')
  time_points <- rep(1:time_points,each=num_bills/time_points)
  time_points <- time_points[bill_points]
  
  if(latent_space) {
    # use latent-space formulation for likelihood

    pr_absence <- sapply(1:length(person_points),function(n) {
      -sqrt((ideal_pts[person_points[n],time_points[n]] - absence_diff[bill_points[n]])^2)
    }) %>% plogis()
  } else {
    # use IRT formulation for likelihood
    pr_absence <- sapply(1:length(person_points),function(n) {
      ideal_pts[person_points[n],time_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]
    }) %>% plogis()
    
  }

  reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=diff_sd))
  reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd))
  
  # this is the same for all DGPs
  if(latent_space) {
    if(inflate) {
      pr_vote <- sapply(1:length(person_points),function(n) {
        -sqrt((ideal_pts[person_points[n],time_points[n]] - reg_diff[bill_points[n]])^2)
      }) %>% plogis()
    } else {
      # latent space non-inflated formulation is different
      reg_discrim <- prior_func(params=list(N=num_person,mean=0,sd=ideal_pts_sd))
      pr_vote <- sapply(1:length(person_points),function(n) {
        reg_discrim[person_points[n]] + absence_discrim[bill_points[n]] -
          sqrt((ideal_pts[person_points[n],time_points[n]] - reg_diff[bill_points[n]])^2)
      }) %>% plogis()
    }
    
  } else {
    pr_vote <- sapply(1:length(person_points),function(n) {
      ideal_pts[person_points[n],time_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
    }) %>% plogis()
  }

    
  # now pick a DGP function and run it
  
  run_func <- switch(model_type,binary=.binary,
                     `ordinal_ratingscale`=.ordinal_ratingscale,
                     `ordinal_grm`=.ordinal_grm,
                     `poisson`=.poisson,
                     normal=.normal,
                     lognormal=.lognormal)
  
  if(is.null(run_func)) {
    stop("Please select one of the available options for model_type from the help file.")
  }
  
  outobj <- run_func(pr_absence=pr_absence,
           pr_vote=pr_vote,
           N=length(person_points),
           ordinal_outcomes=ordinal_outcomes,
           inflate=inflate,
           latent_space=latent_space,
           time_points=time_points,
           item_points=bill_points,
           person_points=person_points,
           sigma_sd=sigma_sd)
  
  outobj@simul_data <- list(num_person=num_person,
                                       num_bills=num_bills,
                                       absence_discrim_sd=absence_discrim_sd,
                                       absence_diff_mean=absence_diff_mean,
                                       reg_discrim_sd=reg_discrim_sd,
                                       ideal_pts_sd=ideal_pts_sd,
                                       prior_func=prior_func,
                                       ordinal_outcomes=ordinal_outcomes,
                                       true_person=ideal_pts,
                                       true_reg_discrim=reg_discrim,
                                       true_abs_discrim=absence_discrim,
                                  true_person_mean=ideal_pts_mean,
                            time_sd=time_sd,
                            drift=drift,
                            ar_adj=ar_adj)

  outobj@person_data <- data_frame(person.names=paste0('person_',1:nrow(outobj@score_matrix)),
                                               group='L')
  
  outobj@simulation <- TRUE
  
  return(outobj)
  
}

#' A function that loops over numbers of persons/bills to provide a coherent over-view of 
#' idealstan performance for a given model type.
#' @noRd
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
  
  
  all_true <- obj@score_data@simul_data
  
  if(length(unique(as.numeric(obj@score_data@score_matrix$time_id)))>1) {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id)),]
    person_est <- .get_varying(obj)
  } else {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id))]
    person_est <- obj@stan_samples$draws("L_full") %>% as_draws_matrix()
  }
  true_sigma_reg <- all_true$true_reg_discrim
  true_sigma_abs <- all_true$true_abs_discrim
  
  over_params <- function(est_param,true_param) {
    
    param_length <- ncol(est_param)
  
  if(class(true_param)=='matrix') {
    
    num_person <- length(unique(as.numeric(obj@score_data@score_matrix$person_id)))
    time <- length(unique(as.numeric(obj@score_data@score_matrix$time_id)))
    
    # make grid to loop over for person and time
    
    person_time <- expand.grid(1:time,1:num_person)
    
    all_rmse <- sapply(1:nrow(person_time), function(i) {
      
      this_param <- sqrt((est_param[,i] - true_param[person_time$Var2[i],person_time$Var1[i]])^2)
      
      return(this_param)
    })
    
  } else if(class(true_param)=='numeric') {
    
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
  
  
  out_data <- list(`Ideal Points`=over_params(person_est,true_person),
                   `Absence Discriminations`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_abs_free")),
                                                         true_sigma_abs),
                   `Item Discrimations`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_reg_free")),
                                                    true_sigma_reg))
  
  return(out_data)

}

#' Function that computes how often the true value of the parameter is included within the 
#' 95/5 high posterior density interval
#' @param obj A fitted \code{idealstan} object with true data generated by \code{\link{id_sim_gen}}
#' @param rep How many times the models were fitted on new data, currently can only be 1
#' @param quantiles What the quantile coverage of the high posterior density interval should be
#' @export
id_sim_coverage <- function(obj,rep=1,quantiles=c(.95,.05)) {
  
  all_true <- obj@score_data@simul_data
  
  if(length(unique(as.numeric(obj@score_data@score_matrix$time_id)))>1) {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id)),]
    person_est <- .get_varying(obj)
  } else {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id))]
    person_est <- obj@stan_samples$draws("L_full") %>% as_draws_matrix()
  }
  
  true_sigma_reg <- all_true$true_reg_discrim
  true_sigma_abs <- all_true$true_abs_discrim
  
  over_params <- function(est_param,true_param) {
    
    if(class(true_param)=="matrix") {
      
      num_person <- length(unique(as.numeric(obj@score_data@score_matrix$person_id)))
      time <- length(unique(as.numeric(obj@score_data@score_matrix$time_id)))
      high <- apply(est_param,2, quantile,quantiles[1])
      low <- apply(est_param,2, quantile,quantiles[2])
      
      # make grid to loop over for person and time
      
      person_time <- expand.grid(1:time,1:num_person)
      
      all_covs <- sapply(1:nrow(person_time), function(i) {
        
        this_param <- (true_param[person_time$Var2[i],person_time$Var1[i]] < high[i]) && (true_param[person_time$Var2[i],person_time$Var1[i]] >low[i])
        
        return(this_param)
      })
    } else if(class(true_param)=="numeric") {
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

  total_iters <- nrow(as_draws_matrix(obj@stan_samples$draws("L_full")))
  
  if(rep<total_iters) {
    to_iters <- sample(1:total_iters,rep)
  } else {
    to_iters <- 1:total_iters
  }
  # Do this when we want to test asymptotic properties of the estimator
  # person_points <- lapply(to_iters,over_params,
  #                         est_param=all_params$L_full,
  #                         true_param=true_person)

    out_data <- list(`Person Ideal Points`=over_params(person_est,true_person),
                     `Absence Discriminations`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_abs_free")),
                                                           true_sigma_abs),
                     `Item Discrimations`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_reg_free")),
                                                      true_sigma_reg))


  
  return(out_data)
}

#' Residual function for checking estimated samples compared to true simulation scores
#' Returns a data frame with residuals plus quantiles.
#' @param obj A fitted \code{idealstan} object with true data from \code{\link{id_sim_gen}}
#' @param rep Over how many replicates to calculate residuals? Currently can only be 1
#' @export
id_sim_resid <- function(obj,rep=1) {
  
  all_true <- obj@score_data@simul_data
  
  if(length(unique(as.numeric(obj@score_data@score_matrix$time_id)))>1) {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id)),]
    person_est <- .get_varying(obj)
  } else {
    true_person <- all_true$true_person[as.numeric(levels(obj@score_data@score_matrix$person_id))]
    person_est <- obj@stan_samples$draws("L_full") %>% as_draws_matrix()
  }
  
  true_sigma_reg <- all_true$true_reg_discrim
  true_sigma_abs <- all_true$true_abs_discrim
  
  
  
  over_params <- function(est_param,true_param) {
    
    param_length <- ncol(est_param)

  if(class(true_param)=='matrix') {
    
    num_person <- length(unique(as.numeric(obj@score_data@score_matrix$person_id)))
    time <- length(unique(as.numeric(obj@score_data@score_matrix$time_id)))
    
    # make grid to loop over for person and time
    
    person_time <- expand.grid(1:time,1:num_person)
    
    all_resid <- sapply(1:nrow(person_time), function(i) {
      
      this_param <- est_param[,i] - true_param[person_time$Var2[i],person_time$Var1[i]]
      
      return(this_param)
    })
    
  } else if(class(true_param)=='numeric') {
    
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
  
  out_data <- list(`Ideal Points`=over_params(person_est,true_person),
                   `Absence Discrimination`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_abs_free")),true_sigma_abs),
                   `Item Discrimination`=over_params(as_draws_matrix(obj@stan_samples$draws("sigma_reg_free")),true_sigma_reg))
  
  return(out_data)

}