
# DGP Functions -----------------------------------------------------------

# Functions used to generate all the models

.binary <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    absence=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    ...)
                    {
  
  #standard IRT 2-PL model
  
  votes <- as.numeric(plogis(pr_vote)>runif(N))
  
  # remove pr of absence if model is not inflated
  
  if(absence==F) {
    pr_absence <- 0
  }
  
  combined <- if_else(pr_absence<runif(N),votes,2)
  
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
                      inflate=absence)
  
  return(out_data)                    
}

.ordinal_ratingscale <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    absence=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    ordinal_outcomes=NULL,
                    ...)
{
  
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
  
  # remove pr of absence if model is not inflated
  
  if(absence==F) {
    pr_absence <- 0
  }
  
  combined <- if_else(pr_absence<runif(N),votes,as.integer(ordinal_outcomes)+1L)
  
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
                      inflate=absence)
  
  return(out_data)                    
}

.ordinal_grm <- function(pr_absence=NULL,
                                 pr_vote=NULL,
                                 N=NULL,
                                 absence=NULL,
                                 person_points=NULL,
                                 item_points=NULL,
                                 time_points=NULL,
                                 ordinal_outcomes=NULL,
                                 ...)
{
  
  # need one set of cutpoints for each item
  
  all_cuts <- sapply(1:max(item_points), function(i) {
    cutpoints <- sort(runif(2))
  })
  all_cuts <- all_cuts[,item_points]
  
  #Generate outcomes by person and item
  
  cuts <- sapply(1:(ordinal_outcomes-1),function(y) {
    pr_vote - all_cuts[y,]
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
  
  # remove pr of absence if model is not inflated
  
  if(absence==F) {
    pr_absence <- 0
  }
  
  combined <- if_else(pr_absence<runif(N),votes,as.integer(ordinal_outcomes)+1L)
  
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
                      inflate=absence)
  
  return(out_data)                    
}

.poisson <- function(pr_absence=NULL,
                    pr_vote=NULL,
                    N=NULL,
                    absence=NULL,
                    person_points=NULL,
                    item_points=NULL,
                    time_points=NULL,
                    ...)
{
  
  #standard IRT 2-PL model
  
  votes <- rpois(n = length(pr_vote),lambda = exp(pr_vote))
  
  # remove pr of absence if model is not inflated
  
  if(absence==F) {
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
                      inflate=absence,
                      continuous=T)
  
  return(out_data)                    
}

#' Function to generate random-walk or AR(1) person parameters
#' Recursively generate data
.gen_ts_data <- function(t,adj_in,alpha_int,sigma,init_sides) {
  current_val <- new.env()
  current_val$t1 <- 0
  
  out_vec <- lapply(1:t,function(t_1) {
    
    if(t_1==1) {
      t_11 <- init_sides
      current_val$t1 <- t_11
      return(data_frame(t_11))
    } else {
      t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma/10)
    }
    current_val$t1 <- t_11
    return(data_frame(t_11))
  })  %>% bind_rows
  return(out_vec)
}
