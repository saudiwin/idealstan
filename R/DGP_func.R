
# DGP Functions -----------------------------------------------------------

# Functions used to generate all the models
#' @noRd
.binary <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    y=NULL,
                    N=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    type='simulate',
                    outcome=NULL,
                    latent_space=NULL,
                    ...)
                    {
  
  # need a factor to multiply the probability for latent-space models
  if(latent_space && inflate) {
    mul_fac <- 2
  } else {
    mul_fac <- 1
  }

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- as.numeric((mul_fac*pr_vote)>runif(N))
  } else if(type=='predict') {
    votes <- apply(pr_vote,2,function(c) as.numeric((c*mul_fac)>runif(N)))
  } else if(type=='log_lik') {
    if(inflate) {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcome), 
                          dbinom(1,size = 1,prob=pr_absence[,c]*mul_fac,log=T),
                          dbinom(outcome-1,size=1,prob=pr_vote[,c]*mul_fac,log=T))
      })
      
    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- dbinom(outcome-1,size=1,prob=pr_vote[,c]*mul_fac,log=T)})
    }
    return(t(over_iters))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    combined <- ifelse((pr_absence*mul_fac)<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data)
    
    return(out_data) 
  } else if(type=='predict') {
    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),votes[,c],2))
    # add one to have minimum = 1
    combined <- combined + 1
    attr(combined,'output') <- 'all'
    # transpose to make S x N matrix
    return(t(combined))
  }
                   
}

.ordinal_ratingscale <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    cutpoints=NULL,
                    ordinal_outcomes=NULL,
                    type='simulate',
                    y=NULL,
                    outcome=NULL,
                    ...)
{
  
  if(inflate && type!='simulate') {
    ordinal_outcomes <- ordinal_outcomes -1
  }
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = ordinal_outcomes+1))
    cutpoints <- cutpoints[2:(length(cutpoints)-1)]
    
    #Generate outcomes by personlator
    
    cuts <- sapply(cutpoints,function(y) {
      qlogis(pr_vote) - y
    },simplify='array')
  } else if(type=='predict') {
    # over posterior draws
    cuts_iters <- sapply(1:nrow(cutpoints), function(i) {
      cuts <- sapply(1:ncol(cutpoints),function(y) {
        qlogis(pr_vote[,i]) - cutpoints[i,y]
      })
    },simplify='array')

  } else if(type=='log_lik') {
    if(inflate) {
      n_outcomes <- length(unique(outcome)) - 1
    } else {
      n_outcomes <- length(unique(outcome))
    }
    # over posterior draws
    cuts_iters <- sapply(1:nrow(cutpoints), function(i) {
      cuts <- sapply(1:ncol(cutpoints),function(y) {
        qlogis(pr_vote[,i]) - cutpoints[i,y]
      })
    },simplify='array')
    
  }
  
  # Now we pick votes as a function of the number of categories
  # This code should work for any number of categories
  
  if(type=='simulate') {
    votes <- sapply(1:nrow(cuts), function(i) {
      this_cut <- cuts[i,]
      
      pr_bottom <- 1 - plogis(this_cut[1])
      
      mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
        plogis(this_cut[c]) - plogis(this_cut[c+1])
      })
      
      pr_top <- plogis(this_cut[length(this_cut)])
      
      return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
    })
    
    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           ordered_id=ordinal_outcomes,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data)
    
    return(out_data) 
  } else if(type=='predict') {

    over_iters <- sapply(1:ncol(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
      })
    })

    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),over_iters[,c],as.integer(ordinal_outcomes)+1L))
    attr(combined,'output') <- 'all'
    return(t(combined))
  } else if(type=='log_lik') {
    over_iters <- sapply(1:ncol(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(c(pr_bottom,mid_prs,pr_top))
      })
    },simplify='array')
    out_num <- as.numeric(outcome)

    if(inflate) {
      # remove top category for vote prediction
      out_num[out_num==max(out_num)] <- max(out_num) - 1
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcome), 
                          dbinom(1,size = 1,prob=pr_absence[,c],log=T),
                          log(over_iters[out_num,,c]))
      })
    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- log(over_iters[out_num,,c])
      })
    }
    
    attr(outdens,'output') <- 'all'
    return(t(outdens))
  }
                   
}

.ordinal_grm <- function(pr_absence=NULL,
                                 pr_vote=NULL,
                         y=NULL,
                                 N=NULL,
                                 inflate=NULL,
                                 person_points=NULL,
                                 item_points=NULL,
                                 time_points=NULL,
                                 ordinal_outcomes=NULL,
                         cutpoints=NULL,
                         type='simulate',
                         outcome=NULL,
                                 ...)
{
  
  if(inflate && type!='simulate') {
    ordinal_outcomes <- ordinal_outcomes -1
  }
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }

  # need one set of cutpoints for each item
  if(type=='simulate') {
    all_cuts <- sapply(1:max(item_points), function(i) {
      cutpoints <- sort(runif(2))
    })
    all_cuts <- all_cuts[,item_points]
    
    #Generate outcomes by person and item
    
    cuts <- sapply(1:(ordinal_outcomes-1),function(y) {
      qlogis(pr_vote) - all_cuts[y,]
    },simplify='array')
  } else {
    # over posterior draws
    
    cuts_iters <- sapply(1:dim(cutpoints)[1], function(i) {
        qlogis(pr_vote[,i]) - cutpoints[i,item_points,]
    },simplify='array')
  }

  
  if(type=='simulate') {
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
    
    # remove pr of absence if model is not inflated
    
    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_disc=combined,
                           person_id=person_points,
                           ordered_id=ordinal_outcomes,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data)
    
    return(out_data)                      
  } else if(type=='predict') {
    over_iters <- sapply(1:ncol(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
      })
    })
    
    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),over_iters[,c],as.integer(ordinal_outcomes)+1L))
    attr(combined,'output') <- 'all'
    return(t(combined))
  } else if(type=='log_lik') {
    
    over_iters <- sapply(1:ncol(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(c(pr_bottom,mid_prs,pr_top))
      })
    },simplify='array')
    
    out_num <- as.numeric(outcome)

    if(inflate) {
      # remove top category for vote prediction
      out_num[out_num==max(out_num)] <- max(out_num) - 1
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcome), 
                          dbinom(1,size = 1,prob=pr_absence[,c],log=T),
                          log(over_iters[out_num,,c]))
      })
    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- log(over_iters[out_num,,c])
      })
    }
    
    attr(outdens,'output') <- 'all'
    return(t(outdens))
  }
  
}

.poisson <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    y=NULL,
                    N=NULL,
                    max_val=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    type='simulate',
                    output=NULL,
                    outcome=NULL,
                    ...)
{

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rpois(n = length(pr_vote),lambda = exp(pr_vote))
  } else if(type=='predict') {
    votes <- apply(pr_vote,2,function(c) rpois(n=length(c),lambda = exp(c)))
  } else if(type=='log_lik') {
    if(inflate) {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcome), 
                          dbinom(1,size = 1,prob=pr_absence[,c],log=T),
                          dpois(outcome,lambda=exp(pr_vote[,c]),log=T))
      })
      
    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- dpois(outcome,lambda=exp(pr_vote[,c]),log=T)})
    }
    return(t(over_iters))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {

    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        unbounded=T)
    
    return(out_data) 
  } else if(type=='predict') {
    if(output=='observed') {
      combined <- votes
      attr(combined,'output') <- 'observed'
      attr(combined,'output_type') <- 'discrete'
    } else if(output=='missing') {
      combined <- apply(pr_absence, 2,function(c) as.numeric(c>runif(N)))
      attr(combined,'output') <- 'missing'
      attr(combined,'output_type') <- 'discrete'
    }
    # transpose to make S x N matrix
    return(t(combined))
  } 
  
}

.normal <- function(pr_absence=NULL,
                     pr_vote=NULL,
                    y=NULL,
                     N=NULL,
                    max_val=NULL,
                     inflate=NULL,
                     person_points=NULL,
                     item_points=NULL,
                     time_points=NULL,
                    sigma_sd=NULL,
                    type='simulate',
                    output='observed',
                    outcome=NULL,
                     ...)
{

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rnorm(n = length(pr_vote),mean = pr_vote,sd = sigma_sd)
  } else if(type=='predict') {

    votes <- sapply(1:ncol(pr_vote),function(c) rnorm(n=nrow(pr_vote),mean=pr_vote[,c],sd=sigma_sd[c]))
  } else if(type=='log_lik') {
    if(inflate) {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcome), 
                          dbinom(1,size = 1,prob=pr_absence[,c],log=T),
                          dnorm(outcome,mean=pr_vote[,c],sd=sigma_sd[c],log=T))
      })
      
    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- dnorm(outcome,mean=pr_vote[,c],sd=sigma_sd[c],log=T)})
    }
    return(t(over_iters))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    
    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_cont=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        unbounded=T)
    
    return(out_data) 
  } else if(type=='predict') {
    if(output=='observed') {
      combined <- votes
      attr(combined,'output') <- 'observed'
      attr(combined,'output_type') <- 'continuous'
    } else if(output=='missing') {
      combined <- apply(pr_absence, 2,function(c) as.numeric(c>runif(N)))
      attr(combined,'output') <- 'missing'
      attr(combined,'output_type') <- 'discrete'
    }
    # transpose to make S x N matrix
    return(t(combined))
  } 
  
}

.lognormal <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    y=NULL,
                    max_val=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    sigma_sd=NULL,
                    type='simulate',
                    output='observed',
                    outcome=NULL,
                    ...)
{
  
  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rlnorm(n = length(pr_vote),meanlog = exp(pr_vote),sdlog = sigma_sd)
  } else if(type=='predict') {
    votes <- sapply(1:ncol(pr_vote),function(c) rlnorm(n=nrow(pr_vote),meanlog=exp(pr_vote[,c]),sdlog=sigma_sd[c]))
  } else if(type=='log_lik') {
    if(inflate) {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- ifelse(is.na(outcomel), 
                          dbinom(1,size = 1,prob=pr_absence[,c],log=T),
                          dnorm(outcome,mean=pr_vote[,c],sd=sigma_sd[c],log=T))
      })

    } else {
      over_iters <- sapply(1:ncol(pr_vote), function(c) {
        outdens <- dnorm(outcome,mean=pr_vote[,c],sd=sigma_sd[c],log=T)})
    }
    return(t(over_iters))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    
    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome_cont=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        unbounded=T)
    
    return(out_data) 
  } else if(type=='predict') {
    if(output=='observed') {
      combined <- votes
      attr(combined,'output') <- 'observed'
      attr(combined,'output_type') <- 'continuous'
    } else if(output=='missing') {
      combined <- apply(pr_absence, 2,function(c) as.numeric(c>runif(N)))
      attr(combined,'output') <- 'missing'
      attr(combined,'output_type') <- 'discrete'
    }
    # transpose to make S x N matrix
    return(t(combined))
  }               
}

#' Function to generate random-walk or AR(1) person parameters
#' Recursively generate data
#' @noRd
.gen_ts_data <- function(t,adj_in,alpha_int,sigma,init_sides) {
  current_val <- new.env()
  current_val$t1 <- 0
  
  out_vec <- lapply(1:t,function(t_1) {
    
    if(t_1==1) {
      t_11 <- alpha_int
      current_val$t1 <- t_11
      return(data_frame(t_11))
    } else {
      if(adj_in==1) {
        t_11 <- adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
      } else {
        t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
      }
      
    }
    current_val$t1 <- t_11
    return(data_frame(t_11))
  })  %>% bind_rows
  return(out_vec)
}
