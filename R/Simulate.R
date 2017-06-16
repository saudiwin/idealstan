#' A function designed to simulate absence-inflated data with either binary or ordinal outcomes
#' @export
simulate_absence <- function(num_legis=10,num_bills=100,absence_discrim_sd=1,absence_diff_mean=0.5,
                             reg_discrim_sd=4,
                             ideal_pts_sd=1,prior_type='gaussian',ordinal=TRUE,ordinal_outcomes=3,
                             graded_response=FALSE,noise=0.05) {
  
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
  
  avg_particip <- rnorm(n=num_legis,mean=0,sd=.1)
  
  # First simulate ideal points for legislators/bills
  # Bill difficulty parameters are fixed because they are not entirely interesting (they represent intercepts)
  
  absence_diff <- prior_func(params=list(N=num_bills,mean=absence_diff_mean,sd=1)) 
  
  
  #Discrimination parameters more important because they reflect how much information a bill contributes
  
  absence_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=absence_discrim_sd)) %>% as.matrix 
  
  # Legislator ideal points common to both types of models (absence and regular)
  
  ideal_pts <- prior_func(params=list(N=num_legis,mean=0,sd=ideal_pts_sd)) %>% as.matrix
  
  # First generate prob of absences
  # Use matrix multiplication because it's faster (unlike the stan method)
  
  pr_absence <- plogis(t(t(ideal_pts %*% t(absence_discrim))-absence_diff + prior_func(params=list(N=num_bills,mean=0,sd=noise))) - 0.1*avg_particip)
  
  # Estimate prob of people voting on a bill (yes/no/abstain), then deflate that by the probability
  # of absence
  
  if(ordinal==TRUE & graded_response==FALSE) {
    
    # Need to simulate a separate prob for each outcome category
    # I only simulate 3 outcome categories by default as that is what you find in legislatures
    # Standard model for ordinal outcomes is the rating-scale model, also called a "divide by total"
    # ordinal logit/categorical logit
    
    reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=1))
    reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd)) %>% as.matrix
    
    pr_vote <-t(t(ideal_pts %*% t(reg_discrim))-reg_diff + prior_func(params=list(N=num_bills,mean=0,sd=noise)))
    
  cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
  cutpoints <- cutpoints[2:(length(cutpoints)-1)]
  
    #Generate outcomes by legislator
    
    cuts <- sapply(cutpoints,function(y) {
      pr_vote - y
    },simplify='array')
    
    probs_out <- apply(cuts,c(1,2),function(x) {
      adj_out <- c(1,plogis(x)) - c(plogis(x),0)
    })
  
    # now determine if the outcome. legislators only vote if they show up
    # transform the absence matrix
    
    absences <- apply(pr_absence,c(1,2),function(x) 
        ifelse(x<0.5,1,NA))
    votes <- apply(probs_out,3,function(x) {
     each_bill <- apply(x,2,function(y) sample(length(y),size=1,prob = y)) 
    })
    
    combined <- t(sapply(1:nrow(absences), function(x) {
      output <- absences[x,] * votes[x,]
    }))
    
    # Finally, replace absences with a new category, 6
    
    combined <- replace(combined,is.na(combined),ordinal_outcomes+1)
    
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
    
  } else if(ordinal==TRUE & graded_response==TRUE) {
    
    reg_discrim <- prior_func(params=list(N=num_bills,mean=0,sd=reg_discrim_sd)) %>% as.matrix
    
    all_outcomes <- lapply((1-ordinal_outcomes),function(x) {
      
      
      reg_diff <- prior_func(params=list(N=num_bills,mean=0,sd=1))
      
      pr_vote <-t(t(ideal_pts %*% t(reg_discrim))-reg_diff)
      
    })
    
  } else if(ordinal==FALSE) {
    
  }
  
  return(out_data)
  
  
}

#' A function that loops over numbers of legislators/bills to provide a coherent over-view of 
#' idealstan performance for a given model type.
#' @export
test_idealstan <- function(legis_range=c(10,100),simul_type='absence',is.ordinal=TRUE,...) {
  
  if(simul_type=='absence') {
    simul_func <- simulate_absence
    if(is.ordinal==TRUE) {
      model_type <- 4
    } else {
      model_type <- 2
    }
  }
  
  all_sims <- lapply(seq(legis_range[1],legis_range[2],by=2), function(N){
    sim_data <- simul_func(num_legis=N,ordinal=is.ordinal)
  })
  
  #See if this works
  
  est_models <- lapply(all_sims,estimate_ideal,model_type=model_type,...)
  est_models_vb <- lapply(all_sims,estimate_ideal,use_vb=TRUE,model_type=model_type)
  
  return(list('regular'=est_models,'vb'=est_models_vb))
  
}

#' RMSE function for calculating individual RMSE values compared to true simulation scores
#' Returns a data frame with RMSE plus quantiles.
#' @param est_param A matrix of posterior draws of a parameter
#' @param true_param A matrix (one column) of true parameter values
#' @export
calc_rmse <- function(est_param,true_param) {
  
  if(class(est_param)=='array') {
    param_length <- dim(est_param)[3]
    all_rmse <- sapply(1:param_length, function(i) {
      this_param <- sqrt((est_param[,,i] - true_param[i,])^2)
    })
  } else if(class(est_param)=='matrix') {
    param_length <- ncol(est_param)
    all_rmse <- sapply(1:param_length, function(i) {
      this_param <- sqrt((est_param[,i] - true_param[i,])^2)
    })
  }

  
  out_data1 <- data_frame(avg=apply(all_rmse,2,mean),
                         high=apply(all_rmse,2,quantile,probs=0.9),
                         low=apply(all_rmse,2,quantile,probs=0.1),
                         total_avg=mean(all_rmse),
                         total_high=quantile(all_rmse,0.9),
                         total_low=quantile(all_rmse,0.1),
                         Params=1:param_length)
  
  # Do a scale flip and check
  
  true_param <- true_param * -1
  
  out_data2 <- data_frame(avg=apply(all_rmse,2,mean),
                          high=apply(all_rmse,2,quantile,probs=0.9),
                          low=apply(all_rmse,2,quantile,probs=0.1),
                          total_avg=mean(all_rmse),
                          total_high=quantile(all_rmse,0.9),
                          total_low=quantile(all_rmse,0.1),
                          Params=1:param_length)
  
  if(mean(out_data1$total_avg)<mean(out_data2$total_avg)) {
    return(out_data1)
  } else {
    return(out_data2)
  }
}

#' Function that computes how often the true value of the parameter is included within the 
#' 90/10 high posterior density interval
#' @param est_param A matrix of posterior draws of a parameter
#' @param true_param A matrix (one column) of true parameter values
#' @export
calc_coverage <- function(est_param,true_param) {
  
  if(class(est_param)=='array') {
    param_length <- dim(est_param)[3]
    all_covs <- sapply(1:param_length, function(i) {
      high <- quantile(est_param[,,i],.9)
      low <- quantile(est_param[,,i],.1)
      this_param <- (true_param[i,] < high) && (true_param[i,] > low)
    })
  } else if(class(est_param)=='matrix') {
    param_length <- ncol(est_param)
    all_covs <- sapply(1:param_length, function(i) {
      high <- quantile(est_param[,i],.9)
      low <- quantile(est_param[,i],.1)
      this_param <- (true_param[i,] < high) && (true_param[i,] > low)
    })
  }

  
  out_data <- data_frame(avg=all_covs,
                         Params=1:param_length)
  
  return(out_data)
}