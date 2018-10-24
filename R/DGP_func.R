
# DGP Functions -----------------------------------------------------------

# Functions used to generate all the models

.binary <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    type='simulate',
                    ...)
                    {

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- as.numeric(plogis(pr_vote)>runif(N))
  } else if(type=='predict') {
    votes <- apply(pr_vote,2,function(c) as.numeric(plogis(c)>runif(N)))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    combined <- if_else(pr_absence<(runif(N)+pr_boost),votes,2)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        miss_val = 2,
                        high_val = 1,
                        low_val = 0,
                        middle_val = NULL,
                        inflate=inflate)
    
    return(out_data) 
  } else if(type=='predict') {
    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),votes[,c],2))
    # add one to have minimum = 1
    combined <- combined + 1
    
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
      pr_vote - y
    },simplify='array')
  } else if(type=='predict') {
    # over posterior draws
    cuts_iters <- sapply(1:nrow(cutpoints), function(i) {
      cuts <- sapply(1:ncol(cutpoints),function(y) {
        pr_vote[,i] - cutpoints[i,y]
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
    
    combined <- if_else(pr_absence<(runif(N)+pr_boost),votes,as.integer(ordinal_outcomes)+1L)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        miss_val = as.integer(ordinal_outcomes)+1,
                        high_val = ordinal_outcomes,
                        low_val = 1,
                        middle_val = 2:(ordinal_outcomes-1),
                        inflate=inflate,
                        ordinal=T)
    
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
    return(t(combined))
  }
                   
}

.ordinal_grm <- function(pr_absence=NULL,
                                 pr_vote=NULL,
                                 N=NULL,
                                 inflate=NULL,
                                 person_points=NULL,
                                 item_points=NULL,
                                 time_points=NULL,
                                 ordinal_outcomes=NULL,
                         cutpoints=NULL,
                         type='simulate',
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
      pr_vote - all_cuts[y,]
    },simplify='array')
  } else {
    # over posterior draws
    
    cuts_iters <- sapply(1:dim(cutpoints)[1], function(i) {
        pr_vote[,i] - cutpoints[i,item_points,]
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
    
    combined <- if_else(pr_absence<(runif(N)+pr_boost),votes,as.integer(ordinal_outcomes)+1L)
    
    # Create a score dataset
    
    out_data <- data_frame(outcome=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        miss_val = as.integer(ordinal_outcomes)+1,
                        high_val = ordinal_outcomes,
                        low_val = 1,
                        middle_val = 2:(ordinal_outcomes-1),
                        inflate=inflate,
                        ordinal=T)
    
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
    return(t(combined))
  }
  
}

.poisson <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    max_val=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    type='simulate',
                    ...)
{

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rpois(n = length(pr_vote),lambda = exp(pr_vote))
  } else if(type=='predict') {
    votes <- apply(pr_vote,2,function(c) rpois(n=length(c),lambda = exp(c)))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {

    combined <- if_else(pr_absence<(runif(N)+pr_boost),votes,as.integer(max(votes)+1))
    
    # Create a score dataset
    
    out_data <- data_frame(outcome=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')
    
    out_data <- id_make(score_data=out_data,
                        middle_val = NULL,
                        miss_val=max(votes)+1,
                        inflate=inflate,
                        unbounded=T)
    
    return(out_data) 
  } else if(type=='predict') {
    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),votes[,c],max_val))
    # transpose to make S x N matrix
    return(t(combined))
  }
  
}

.normal <- function(pr_absence=NULL,
                     pr_vote=NULL,
                     N=NULL,
                    max_val=NULL,
                     inflate=NULL,
                     person_points=NULL,
                     item_points=NULL,
                     time_points=NULL,
                    sigma_sd=NULL,
                    type='simulate',
                     ...)
{
  
  #standard IRT 2-PL model
  
  votes <- rnorm(n = length(pr_vote),mean = pr_vote,sd = sigma_sd)
  
  # remove pr of absence if model is not inflated
  
  if(inflate==F) {
    pr_absence <- 0
  }
  
  combined <- ifelse(pr_absence<runif(N),votes,max(votes)+1)
  
  # Create a score dataset
  
  out_data <- data_frame(outcome=combined,
                         person_id=person_points,
                         time_id=time_points,
                         item_id=item_points,
                         group_id='G')
  
  out_data <- id_make(score_data=out_data,
                      middle_val = NULL,
                      inflate=inflate,
                      unbounded=T)
  
  return(out_data)                    
}

.lognormal <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    max_val=NULL,
                    inflate=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    sigma_sd=NULL,
                    type='simulate',
                    ...)
{
  
  #standard IRT 2-PL model
  
  votes <- rlnorm(n = length(pr_vote),meanlog = exp(pr_vote),sdlog = sigma_sd)
  
  # remove pr of absence if model is not inflated
  
  if(inflate==F) {
    pr_absence <- 0
  }
  
  combined <- ifelse(pr_absence<runif(N),votes,max(votes)+1)
  
  # Create a score dataset
  
  out_data <- data_frame(outcome=combined,
                         person_id=person_points,
                         time_id=time_points,
                         item_id=item_points,
                         group_id='G')
  
  out_data <- id_make(score_data=out_data,
                      middle_val = NULL,
                      inflate=inflate,
                      unbounded=T)
  
  return(out_data)                    
}

#' Function to generate random-walk or AR(1) person parameters
#' Recursively generate data
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
