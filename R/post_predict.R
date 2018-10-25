# All posterior prediction functions for models.

#' Function to predict absence-inflated ordinal models
.predict_abs_ord <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           obj = NULL) {
    abs_cat <- as.integer(obj@score_data@miss_val)
    all_votes <-
      as.integer(obj@score_data@vote_int[-length(obj@score_data@vote_int)])
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_absence <-
          plogis(all_params$sigma_abs_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
                   all_params$A_int_full[s, bill_points[v]])
        pr_vote <-
          all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]]
        votes <-
          if_else(
            pr_absence > runif(1),
            abs_cat,
            .sample_cut(
              pr_vote = pr_vote,
              cutpoints = all_params$steps_votes[s, ],
              n_outcomes = length(all_votes)
            )
          )
        return(votes)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Function to calculate log-likelihood of absence-inflated ordinal models
.predict_abs_ord_ll <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           y=NULL,
           obj = NULL) {
    abs_cat <- as.integer(obj@score_data@miss_val)
    all_votes <-
      as.integer(obj@score_data@vote_int[-length(obj@score_data@vote_int)])
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_absence <-
          plogis(all_params$sigma_abs_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
                   all_params$A_int_full[s, bill_points[v]])
        pr_vote <-
          all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]]
        votes <-
          if_else(
            pr_absence > runif(1),
            abs_cat,
            .sample_cut_ll(
              pr_vote = pr_vote,
              cutpoints = all_params$steps_votes[s, ],
              n_outcomes = length(all_votes)
            )
          )
        return(log(votes[y[v]]))
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Function to predict ordinal models
.predict_ord <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           obj = NULL) {
    all_votes <-
      as.integer(obj@score_data@vote_int[-length(obj@score_data@vote_int)])
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        pr_vote <-
          all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]]
        votes <- .sample_cut(
          pr_vote = pr_vote,
          cutpoints =
            all_params$steps_votes[s, ],
          n_outcomes = length(all_votes)
        )
        
        return(votes)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Function to produce log-likelihood of ordinal models
.predict_ord_ll <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           y=NULL,
           obj = NULL) {
    all_votes <-
      as.integer(obj@score_data@vote_int[-length(obj@score_data@vote_int)])
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        pr_vote <-
          all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]]
        votes <- .sample_cut_ll(
          pr_vote = pr_vote,
          cutpoints =
            all_params$steps_votes[s, ],
          n_outcomes = length(all_votes)
        )
        
        return(log(votes[y[v]]))
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Helper function for sampling from ordinal cutpoints
.sample_cut <-
  function(pr_vote = NULL,
           cutpoints = NULL,
           n_outcomes = NULL) {
    # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
    
    cuts <- sapply(cutpoints, function(y) {
      pr_vote - y
    })
    
    
    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    
    pr_bottom <- 1 - plogis(cuts[1])
    
    mid_prs <- sapply(1:(length(cuts) - 1), function(c) {
      plogis(cuts[c]) - plogis(cuts[c + 1])
    })
    
    pr_top <- plogis(cuts[length(cuts)])
    
    return(as.integer(sample(
      1:(length(cuts) + 1),
      size = 1,
      prob = c(pr_bottom, mid_prs, pr_top)
    )))
    
  }

#' Helper function for obtaining density given ordinal cutpoints
.sample_cut_ll <-
  function(pr_vote = NULL,
           cutpoints = NULL,
           n_outcomes = NULL) {
    # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
    
    cuts <- sapply(cutpoints, function(y) {
      pr_vote - y
    })
    
    
    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    
    pr_bottom <- 1 - plogis(cuts[1])
    
    mid_prs <- sapply(1:(length(cuts) - 1), function(c) {
      plogis(cuts[c]) - plogis(cuts[c + 1])
    })
    
    pr_top <- plogis(cuts[length(cuts)])
    
    return(c(pr_bottom, mid_prs, pr_top))
    
  }

#' Helper function for obtaining density given ordinal GRM cutpoints
.sample_cut_grm_ll <-
  function(pr_vote = NULL,
           cutpoints = NULL,
           n_outcomes = NULL,
           item_points=NULL) {
    # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
    
    cuts <- sapply(cutpoints, function(y) {
      pr_vote - y
    })
    
    
    # Now we pick votes as a function of the number of categories
    # This code should work for any number of categories
    
    pr_bottom <- 1 - plogis(cuts[1])
    
    mid_prs <- sapply(1:(length(cuts) - 1), function(c) {
      plogis(cuts[c]) - plogis(cuts[c + 1])
    })
    
    pr_top <- plogis(cuts[length(cuts)])
    
    return(c(pr_bottom, mid_prs, pr_top))
    
  }

#' Function to predict ordinary 2 PL models
.predict_2pl <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           obj = NULL) {
    all_votes <- as.integer(obj@score_data@vote_int)
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_vote <-
          plogis(all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]])
        votes <- if_else(pr_vote > runif(1), 2L, 1L)
        return(votes)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

.predict_2pl_ll <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           y=NULL,
           obj = NULL) {
    all_votes <- as.integer(obj@score_data@vote_int)
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_vote <-
          all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]]
        dens <- dbinom(y[v]-1,1,plogis(pr_vote),log = T)
        return(dens)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Function to predict absence-inflated binary models
.predict_abs_bin <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           y=NULL,
           obj = NULL) {
    abs_cat <- as.integer(obj@score_data@miss_val)
    
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_absence <-
          plogis(all_params$sigma_abs_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
                   all_params$A_int_full[s, bill_points[v]])
        pr_vote <-
          plogis(all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
          all_params$B_int_full[s, bill_points[v]])
        this_vote <- if_else(pr_vote > runif(1), 2L, 1L)
        votes <- if_else(pr_absence > runif(1), abs_cat, this_vote)
        return(votes)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }

#' Function to find log-likelihood of absence-inflated binary models
.predict_abs_bin_ll <-
  function(all_params = NULL,
           n_iters = NULL,
           sample_draws = NULL,
           sample_scores = NULL,
           legis_points = NULL,
           bill_points = NULL,
           time_points = NULL,
           y=NULL,
           obj = NULL) {
    abs_cat <- as.integer(obj@score_data@miss_val)
    
    out_matrix <- sapply(sample_draws, function(s) {
      # Loop over samples
      out_votes <- sapply(sample_scores, function(v) {
        #Loop over individual votes
        pr_absence <-
          plogis(all_params$sigma_abs_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
                   all_params$A_int_full[s, bill_points[v]])
        pr_vote <-
          plogis(all_params$sigma_reg_full[s, bill_points[v]] * all_params$L_full[s, time_points[v], legis_points[v]] -
                   all_params$B_int_full[s, bill_points[v]])
        this_vote <- if_else(pr_vote > runif(1), 2L, 1L)
        votes <- if_else(pr_absence > runif(1), abs_cat, this_vote)
        if(y[v]==abs_cat) {
          dens <- dbinom(as.numeric(y[v]==abs_cat),1,plogis(pr_absence),log = T)
        } else {
          dens <- dbinom(as.numeric(y[v]==abs_cat),1,plogis(pr_absence),log = T) + 
            dbinom(y[v]-1,1,pr_vote,log = T)
        }
        
        return(dens)
      })
    })
    # transpose to fit bayesplot function
    return(t(out_matrix))
  }