#' A function designed to simulate absence-inflated data with either binary or ordinal outcomes
#' @export
simulate_models <- function(num_legis=50,num_bills=50,absence_discrim_sd=.1,absence_diff_mean=0.5,
                             reg_discrim_sd=.1,diff_sd=.1,
                             ideal_pts_sd=1,prior_type='gaussian',ordinal=TRUE,ordinal_outcomes=3,
                             graded_response=FALSE,noise=0.05,absence=TRUE) {
  
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
  
  #Fixed average participation value for each legislator
  
  avg_particip <- rnorm(n=1,mean=0,sd=1)
  
  # First simulate ideal points for legislators/bills
  # Bill difficulty parameters are fixed because they are not entirely interesting (they represent intercepts)
  
  absence_diff <- prior_func(params=list(N=num_bills,mean=absence_diff_mean,sd=diff_sd)) 
  
  
  #Discrimination parameters more important because they reflect how much information a bill contributes
  # need to make some of them negative to reflect the switching nature of policies
  absence_discrim <- prior_func(params=list(N=num_bills,mean=1,sd=absence_discrim_sd)) * if_else(runif(num_bills)>0.5,1,-1)
  
  # Legislator ideal points common to both types of models (absence and regular)
  
  ideal_pts <- prior_func(params=list(N=num_legis,mean=0,sd=ideal_pts_sd))
  
  # First generate prob of absences
  # Use matrix multiplication because it's faster (unlike the stan method)
  
  #pr_absence <- plogis(t(t(ideal_pts %*% t(absence_discrim))-absence_diff + prior_func(params=list(N=num_bills,mean=0,sd=noise))) - 0.1*avg_particip)
  legis_points <- rep(1:num_legis,times=num_bills)
  bill_points <- rep(1:num_bills,each=num_legis)
  
  pr_absence <- sapply(1:length(legis_points), function(n) {
    ideal_pts[legis_points[n]]*absence_discrim[bill_points[n]] - absence_diff[bill_points[n]]}) %>% plogis
  
  reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=diff_sd))
  reg_discrim <- prior_func(params=list(N=num_bills,mean=1,sd=reg_discrim_sd)) * if_else(runif(num_bills)>0.5,1,-1)
  
  pr_vote <- sapply(1:length(legis_points), function(n) {
    ideal_pts[legis_points[n]]*reg_discrim[bill_points[n]] - reg_diff[bill_points[n]]
  })
  
    #plogis(t(t(ideal_pts %*% t(absence_discrim))) -absence_diff - 0.1*avg_particip)
  # Estimate prob of people voting on a bill (yes/no/abstain), then deflate that by the probability
  # of absence
  
  if(ordinal==TRUE & graded_response==FALSE & absence==TRUE) {
    
    # Need to simulate a separate prob for each outcome category
    # I only simulate 3 outcome categories by default as that is what you find in legislatures
    # Standard model for ordinal outcomes is the rating-scale model, also called a "divide by total"
    # ordinal logit/categorical logit
    
    # First calculate probabilities of vote outcomes for all legislators
    
    # pr_vote <- t(t(ideal_pts %*% t(reg_discrim))-reg_diff)
    # Now create cutpoints that are equally spaced in the ideal point space
    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
    cutpoints <- cutpoints[2:(length(cutpoints)-1)]
  
    #Generate outcomes by legislator
    
    cuts <- sapply(cutpoints,function(y) {
      pr_vote - y
    },simplify='array')
    
    # probs_out <- apply(cuts,c(1,2),function(x) {
    #   adj_out <- c(1,plogis(x)) - c(plogis(x),0)
    # })

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
    
  
    # now determine if the outcome. legislators only vote if they show up
    # Absences are coded as category 4
    
    combined <- if_else(pr_absence<0.5,votes,as.integer(ordinal_outcomes)+1L)
    
    # Create a vote matrix
    
    combined <- matrix(combined,ncol=num_bills,nrow=num_legis,byrow = F)
    
    #Got the vote matrix, run ideal_data
    
    colnames(combined) <- paste0('Vote_',1:ncol(combined))
    row.names(combined) <- paste0('Legis_',1:nrow(combined))
    
    out_data <- make_idealdata(vote_data=combined,legis_data=data_frame(legis.names=paste0('Legis_',1:nrow(combined)),
                                                                        party='L',
                                                                        true_legis=as.numeric(ideal_pts)),
                               abs_vote = ordinal_outcomes+1,
                               yes_vote = ordinal_outcomes,
                               no_vote = 1,
                               abst_vote = 2:(ordinal_outcomes-1),
                               simul_data=list(num_legis=num_legis,
                                              num_bills=num_bills,
                                              absence_discrim_sd=absence_discrim_sd,
                                              absence_diff_mean=absence_diff_mean,
                                              reg_discrim_sd=reg_discrim_sd,
                                              ideal_pts_sd=ideal_pts_sd,
                                              prior_func=prior_func,
                                              ordinal=ordinal,
                                              ordinal_outcomes=ordinal_outcomes,
                                              graded_response=graded_response,
                                              true_legis=ideal_pts,
                                              true_reg_discrim=reg_discrim,
                                              true_abs_discrim=absence_discrim),
                               simulation=TRUE)
    
  } else if(ordinal==TRUE & graded_response==TRUE & absence==TRUE) {
    
    reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd)) %>% as.matrix
    
    all_outcomes <- lapply((1-ordinal_outcomes),function(x) {
      
      
      reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=1))
      
      pr_vote <-t(t(ideal_pts %*% t(reg_discrim))-reg_diff)
      
    })
    
  } else if(ordinal==FALSE & graded_response==FALSE & absence==FALSE) {
    
    #standard IRT 2-PL model

    votes <- as.numeric(plogis(pr_vote)>0.5)
    
    
    # now determine if the outcome. legislators only vote if they show up
    # Absences are coded as category 4
    
    combined <- votes
    
    # Create a vote matrix
    
    combined <- matrix(combined,ncol=num_bills,nrow=num_legis,byrow = F)
    
    #Got the vote matrix, run ideal_data
    
    colnames(combined) <- paste0('Vote_',1:ncol(combined))
    row.names(combined) <- paste0('Legis_',1:nrow(combined))
    
    out_data <- make_idealdata(vote_data=combined,legis_data=data_frame(legis.names=paste0('Legis_',1:nrow(combined)),
                                                                        party='L',
                                                                        true_legis=as.numeric(ideal_pts)),
                               abs_vote = NULL,
                               yes_vote = 1,
                               no_vote = 0,
                               abst_vote = NULL,
                               simul_data=list(num_legis=num_legis,
                                               num_bills=num_bills,
                                               absence_discrim_sd=absence_discrim_sd,
                                               absence_diff_mean=absence_diff_mean,
                                               reg_discrim_sd=reg_discrim_sd,
                                               ideal_pts_sd=ideal_pts_sd,
                                               prior_func=prior_func,
                                               ordinal=ordinal,
                                               ordinal_outcomes=ordinal_outcomes,
                                               graded_response=graded_response,
                                               true_legis=ideal_pts,
                                               true_reg_discrim=reg_discrim,
                                               true_abs_discrim=absence_discrim),
                               simulation=TRUE)
  } else if(ordinal==TRUE & graded_response==FALSE & absence==FALSE) {
    
    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
    cutpoints <- cutpoints[2:(length(cutpoints)-1)]
    
    #Generate outcomes by legislator
    
    cuts <- sapply(cutpoints,function(y) {
      pr_vote - y
    },simplify='array')
    

    
    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    votes <- sapply(1:nrow(cuts), function(i) {
      if(all(cuts[i,]>0)) {
        return(as.integer(ordinal_outcomes))
      } else if(all(cuts[i,]<0)) {
        return(1L)
      } else {
        all_pos <- which(cuts[i,]>0)+1
        return(as.integer(all_pos[1]))
      }
    })

    
    
    # now determine if the outcome. legislators only vote if they show up
    # Absences are coded as category 4
    
    combined <- votes
    
    # Create a vote matrix
    
    combined <- matrix(combined,ncol=num_bills,nrow=num_legis,byrow = F)
    
    #Got the vote matrix, run ideal_data
    
    colnames(combined) <- paste0('Vote_',1:ncol(combined))
    row.names(combined) <- paste0('Legis_',1:nrow(combined))
    
    out_data <- make_idealdata(vote_data=combined,legis_data=data_frame(legis.names=paste0('Legis_',1:nrow(combined)),
                                                                        party='L',
                                                                        true_legis=as.numeric(ideal_pts)),
                               abs_vote = ordinal_outcomes+1,
                               yes_vote = ordinal_outcomes,
                               no_vote = 1,
                               abst_vote = 2:(ordinal_outcomes-1),
                               simul_data=list(num_legis=num_legis,
                                               num_bills=num_bills,
                                               absence_discrim_sd=absence_discrim_sd,
                                               absence_diff_mean=absence_diff_mean,
                                               reg_discrim_sd=reg_discrim_sd,
                                               ideal_pts_sd=ideal_pts_sd,
                                               prior_func=prior_func,
                                               ordinal=ordinal,
                                               ordinal_outcomes=ordinal_outcomes,
                                               graded_response=graded_response,
                                               true_legis=ideal_pts,
                                               true_reg_discrim=reg_discrim,
                                               true_abs_discrim=absence_discrim),
                               simulation=TRUE)
  }
  
  return(out_data)
  
  
}

#' A function that loops over numbers of legislators/bills to provide a coherent over-view of 
#' idealstan performance for a given model type.
#' @export
test_idealstan <- function(param_range=c(50,150),by=10,simul_type='absence',is.ordinal=TRUE,
                           restrict_type='constrain_twoway',restrict_params='legis',
                           num_constrain=10,fixtype='pinned',...) {

  #check for inconsistent parameters
  
  if(num_constrain>(min(param_range)-1)) {
    stop('Please do not select num_constrain greater than the minimum of param_range.')
  }
  
  if(simul_type=='absence') {
    simul_func <- simulate_models
    if(is.ordinal==TRUE) {
      model_type <- 4
    } else {
      model_type <- 2
    }
    absence <- T
  }
  full_range <- seq(param_range[1],param_range[2],by=by)
  all_sims <- lapply(full_range, function(N){
    sim_data <- simul_func(num_legis=N,num_bills=N,ordinal=is.ordinal,absence=absence)

    true_param <- switch(restrict_params,
                             legis=sim_data@simul_data$true_legis,
                             discrim_reg=sim_data@simul_data$true_reg_discrim,
                             discrim_abs=sim_data@simul_data$true_abs_discrim)
  
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

    estimate_ideal(m$sim_data,
                   use_vb=FALSE,
                   restrict_ind_high=c(m$high_par,m$low_par),
                   pin_vals = c(m$high_par_est,m$low_par_est),
                   fixtype=fixtype,
                   restrict_params=restrict_params,
                   restrict_type=restrict_type,
                   ...)
    
    },...)

  est_models_vb <-  lapply(all_sims,function(m,...) {
    
    estimate_ideal(m$sim_data,
                   use_vb=TRUE,
                   restrict_ind_high=c(m$high_par,m$low_par),
                   pin_vals = c(m$high_par_est,m$low_par_est),
                   fixtype=fixtype,
                   restrict_params=restrict_params,
                   restrict_type=restrict_type,
                   ...)
    
  },...)


  est_models_cov <- lapply(1:length(est_models),function(i) {
    calc_coverage(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
    }) %>% bind_rows
  est_models_vb_cov <- lapply(1:length(est_models),function(i) {
    calc_coverage(est_models_vb[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_rmse <- lapply(1:length(est_models),function(i) {
    calc_rmse(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_vb_rmse <- lapply(1:length(est_models),function(i) {
    calc_rmse(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_resid <- lapply(1:length(est_models),function(i) {
    calc_resid(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  est_models_vb_resid <- lapply(1:length(est_models),function(i) {
    calc_resid(est_models[[i]],rep=i) %>% bind_rows(.id="ID")
  }) %>% bind_rows
  
  
  
  
  return(list(est_models=bind_rows(est_models_cov,est_models_resid,est_models_rmse),
         est_models_vb=bind_rows(est_models_vb_cov,est_models_vb_resid,est_models_rmse)))
  
}

#' RMSE function for calculating individual RMSE values compared to true simulation scores
#' Returns a data frame with RMSE plus quantiles.
#' @param est_param A matrix of posterior draws of a parameter
#' @param true_param A matrix (one column) of true parameter values
#' @export
calc_rmse <- function(obj,rep=1) {
  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@vote_data@simul_data
  
  true_legis <- all_true$true_legis[as.numeric(row.names(obj@vote_data@vote_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  
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
  
  
  out_data <- list(legis_rmse=over_params(all_params$L_full,true_legis),
                   sigma_abs_rmse=over_params(all_params$sigma_abs_full,true_sigma_abs),
                   sigma_reg_rmse=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)

}

#' Function that computes how often the true value of the parameter is included within the 
#' 90/10 high posterior density interval
#' @param est_param A matrix of posterior draws of a parameter
#' @param true_param A matrix (one column) of true parameter values
#' @export
calc_coverage <- function(obj,rep=1) {

  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@vote_data@simul_data
  
  true_legis <- all_true$true_legis[as.numeric(row.names(obj@vote_data@vote_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  
  over_params <- function(est_param,true_param) {

    if(class(est_param)=='array') {
      param_length <- dim(est_param)[3]
      all_covs <- sapply(1:param_length, function(i) {
        high <- quantile(est_param[,,i],.95)
        low <- quantile(est_param[,,i],.05)
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


  
  out_data <- list(legis_cov=over_params(all_params$L_full,true_legis),
                         sigma_abs_cov=over_params(all_params$sigma_abs_full,true_sigma_abs),
                         sigma_reg_cov=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)
}

#' Residual function for checking estimated samples compared to true simulation scores
#' Returns a data frame with residuals plus quantiles.
#' @param est_param A matrix of posterior draws of a parameter
#' @param true_param A matrix (one column) of true parameter values
#' @export
calc_resid <- function(obj,rep=1) {
  
  all_params <- rstan::extract(obj@stan_samples)
  
  all_true <- obj@vote_data@simul_data
  
  true_legis <- all_true$true_legis[as.numeric(row.names(obj@vote_data@vote_matrix))]
  true_sigma_reg <- all_true$true_reg_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  true_sigma_abs <- all_true$true_abs_discrim[as.numeric(colnames(obj@vote_data@vote_matrix))]
  
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
  
  out_data <- list(legis_resid=over_params(all_params$L_full,true_legis),
                   sigma_abs_resid=over_params(all_params$sigma_abs_full,true_sigma_abs),
                   sigma_reg_resid=over_params(all_params$sigma_reg_full,true_sigma_reg))
  
  return(out_data)

}