# All posterior prediction functions for models.

#' Function to predict absence-inflated ordinal models
.predict_abs_ord <- function(all_params=NULL,n_iters=NULL,draws=NULL,n_votes=NULL,
                             legis_points=NULL,bill_points=NULL,time_points=NULL,
                             obj=NULL) {

  abs_cat <- as.integer(obj@vote_data@abs_vote)
  all_votes <- as.integer(obj@vote_data@vote_int[-length(obj@vote_data@vote_int)])
  out_matrix <- sapply(sample(1:n_iters,draws),function(s) {
    # Loop over samples
    out_votes <- sapply(1:n_votes, function(v) {
      #Loop over individual votes
      pr_absence <- plogis(all_params$sigma_abs_full[s,bill_points[v]]*all_params$L_full[s,time_points[v],legis_points[v]] - 
                             all_params$B_abs[s,bill_points[v]])
      pr_vote <- all_params$sigma_reg_full[s,bill_points[v]]*all_params$L_full[s,time_points[v],legis_points[v]] - 
                          all_params$B_yes[s,bill_points[v]]
      votes <- if_else(pr_absence>0.5,abs_cat,.sample_cut(pr_vote=pr_vote,
                                                          cutpoints=all_params$steps_votes[s,],
                                                          n_outcomes=length(all_votes)))
      return(votes)
      })
    })
  # transpose to fit bayesplot function
  return(t(out_matrix))
}

#' Helper function for sampling from ordinal cutpoints
.sample_cut <- function(pr_vote=NULL,cutpoints=NULL,n_outcomes=NULL) {

  # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
  
  cuts <- sapply(cutpoints,function(y) {
    pr_vote - y
  })
  
  
  # Now we pick votes as a function of the number of categories
  # This code should work for any number of categories

    if(all(cuts>0)) {
      all_pos <- n_outcomes
    } else if(all(cuts<0)) {
      all_pos <- 1L
    } else {
      all_pos <- which(cuts>0)+1
    }
  
  return(as.integer(all_pos))

}