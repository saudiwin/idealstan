
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
