
# Helpers for posterior quantities ----------------------------------------

# In id_post_pred(), pr_vote and pr_absence are draws x observations matrices
# for one item, and outcome is the outcome for each observation as passed to
# Stan, where missing values of inflated models are coded at or above miss_val.

#' Which observations are coded as missing
#' @noRd
.miss_rows <- function(outcome, miss_val, inflate) {
  if(inflate && length(miss_val) == 1 && !is.na(miss_val)) {
    outcome >= miss_val
  } else {
    rep(FALSE, length(outcome))
  }
}

#' Repeat a per-observation vector as a draws x observations matrix
#' @noRd
.by_obs <- function(x, n_draws) {
  matrix(x, nrow=n_draws, ncol=length(x), byrow=TRUE)
}

#' Add the missing-data model to the log density of the observed outcomes
#'
#' Matches the Stan likelihood: for inflated models, observed rows add
#' log Pr(observed) and missing rows are log Pr(missing).
#' @noRd
.inflate_log_lik <- function(obs_ll, pr_absence, is_miss, inflate) {
  if(!inflate) return(obs_ll)
  miss_mat <- .by_obs(is_miss, nrow(obs_ll))
  out <- log1p(-pr_absence) + obs_ll
  out[miss_mat] <- log(pr_absence[miss_mat])
  out
}

#' Expected outcome, times Pr(observed) for inflated models
#' @noRd
.inflate_epred <- function(expected, pr_absence, inflate) {
  out <- if(inflate) (1 - pr_absence) * expected else expected
  attr(out,'output') <- 'all'
  out
}

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
                    miss_val=NULL,
                    latent_space=NULL,
                    cov_effect=NULL,
                    person_x=NULL,
                    person_cov_effect=NULL,
                    item_x=NULL,
                    item_discrim_cov_effect=NULL,
                    item_miss_x=NULL,
                    item_miss_discrim_cov_effect=NULL,
                    ...) {


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
    votes <- t(apply(pr_vote,1,function(c) as.numeric((c*mul_fac)>runif(N))))
  } else if(type=="epred") {

    votes <- pr_vote

  } else if(type=='log_lik') {
    # outcome is 0/1
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 0, outcome), nrow(pr_vote))
    obs_ll <- dbinom(y, size=1, prob=pmin(pr_vote*mul_fac, 1), log=TRUE)
    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
  }
  
  
  # remove pr of inflate if model is not inflated
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {
    combined <- ifelse((pr_absence*mul_fac)<(runif(N)+pr_boost),votes,NA)
    
    combined <- combined - min(combined,na.rm=TRUE)
    
    # Create a score dataset
    
    out_data <- tibble(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      # Create person covariate data - compute linear combination X %*% beta
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      # Create item covariate data - compute linear combination X %*% beta
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

    return(out_data) 
  } else if(type=='predict') {
    combined <- sapply(1:nrow(pr_absence), function(c) ifelse(pr_absence[c,]<(runif(N)+pr_boost),votes[c,],2))
    # add one to have minimum = 1
    combined <- combined + 1
    attr(combined,'output') <- 'all'
    # transpose to make S x N matrix
    return(t(combined))
    
  } else if(type=="epred") {
    
    # need to calculate pr(Yes)pr(Present) + Pr(Yes)pr(Absent)
    
    if(inflate) {
      
      combined <- (1 - pr_absence) * pr_vote

    } else {
      
      combined <- pr_vote
      
    }

    attr(combined,'output') <- 'all'
    # transpose to make S x N matrix
    return(combined)
    
  }
                   
}

#' @importFrom ordbetareg rordbeta dordbeta
#' @importFrom tibble tibble as_tibble
.ordbeta <- function(pr_absence=NULL,
                                 pr_vote=NULL,
                                 N=NULL,
                                 inflate=NULL,
                                 person_points=NULL,
                                 item_points=NULL,
                                 time_points=NULL,
                                 cutpoints=NULL,
                                 ordinal_outcomes=NULL,
                                 type='simulate',
                                output=NULL,
                                 y=NULL,
                                 phi=NULL,
                                 outcome=NULL,
                                 cov_effect=NULL,
                                 person_x=NULL,
                                 person_cov_effect=NULL,
                                 item_x=NULL,
                                 item_discrim_cov_effect=NULL,
                                 item_miss_x=NULL,
                                 item_miss_discrim_cov_effect=NULL,
                                 miss_val=NULL,
                                 ...)
{
  
  if(!inflate) {
    pr_boost <- 1
  } else {
    pr_boost <- 0
  }
  
  if(type=='simulate') {

    cutpoints <- quantile(pr_vote,probs=seq(0,1,length.out = 3))
    cutpoints <- cutpoints[c(1,3)]

  } else {

    # posterior: cutpoints is a draws x 2 matrix of this item's cutpoints
    # (logit scale) and phi a vector of draws of this item's phi

    n_draws <- nrow(pr_vote)
    eta <- qlogis(pr_vote)
    pr_above_low <- plogis(eta - matrix(cutpoints[,1], nrow=n_draws, ncol=ncol(pr_vote)))
    pr_one <- plogis(eta - matrix(cutpoints[,2], nrow=n_draws, ncol=ncol(pr_vote)))

  }
  
  # Now we pick votes as a function of the number of categories
  # This code should work for any number of categories
  
  if(type=='simulate') {
    
    votes <- rordbeta(n=length(pr_vote),
                      mu=pr_vote,
                      phi=phi,
                      cutpoints=qlogis(cutpoints))
    
    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)
    
    # Create a score dataset
    
    out_data <- tibble(outcome_cont=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

    return(out_data)

  } else if(type=='predict') {
    
    if(output=='observed') {
      
      combined <- sapply(1:nrow(pr_vote), function(i) {
        
        rordbeta(n=ncol(pr_vote),mu=pr_vote[i,],phi=phi[i],
                 cutpoints=cutpoints[i,])
        
              })
      
      combined <- t(combined)
      
      attr(combined,'output') <- 'observed'
      attr(combined,'output_type') <- 'continuous'
    } else if(output=='missing') {
      
      combined <- apply(pr_absence, 2,function(c) as.numeric(c>runif(N)))
      
      attr(combined,'output') <- 'missing'
      attr(combined,'output_type') <- 'discrete'
    }
    
    return(combined)
    
  } else if(type=='epred') {

    # E[y] = Pr(y = 1) + Pr(0 < y < 1) * mean of the beta part
    return(.inflate_epred(pr_one + (pr_above_low - pr_one) * pr_vote,
                          pr_absence, inflate))

  } else if(type=='log_lik') {

    # same density as ordbeta_lpdf in the Stan model
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 0.5, outcome), n_draws)
    phi_mat <- matrix(phi, nrow=n_draws, ncol=ncol(pr_vote))

    obs_ll <- ifelse(y==0, log1p(-pr_above_low),
                     ifelse(y==1, log(pr_one),
                            log(pr_above_low - pr_one) +
                              dbeta(pmin(pmax(y, 1e-12), 1 - 1e-12),
                                    pr_vote*phi_mat, (1-pr_vote)*phi_mat, log=TRUE)))

    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
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
                    cov_effect=NULL,
                    person_x=NULL,
                    person_cov_effect=NULL,
                    item_x=NULL,
                    item_discrim_cov_effect=NULL,
                    item_miss_x=NULL,
                    item_miss_discrim_cov_effect=NULL,
                    miss_val=NULL,
                    ...)
{
  
  # if(inflate && type!='simulate') {
  #   ordinal_outcomes <- ordinal_outcomes -1
  # }
  
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
      qlogis(pr_vote) - qlogis(y)
    },simplify='array')
  } else if(type=='predict') {
    # over posterior draws
    cuts_iters <- sapply(1:nrow(cutpoints), function(i) {
      cuts <- sapply(1:ncol(cutpoints),function(y) {
        qlogis(pr_vote[i,]) - c(cutpoints[i,y])
      })
    },simplify='array')

  } else if(type %in% c('log_lik','epred')) {

    # Pr(y > k) for each cutpoint k: a draws x observations x cutpoints array,
    # using the same parameterization as ordered_logistic() in Stan
    n_draws <- nrow(pr_vote)
    n_cuts <- ncol(cutpoints)
    cut_mat <- matrix(as.numeric(cutpoints), nrow=n_draws)
    eta <- qlogis(pr_vote)
    pr_above <- vapply(1:n_cuts, function(k) plogis(eta - cut_mat[,k]),
                       matrix(0, n_draws, ncol(pr_vote)))
    dim(pr_above) <- c(n_draws, ncol(pr_vote), n_cuts)

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
    
    out_data <- tibble(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           ordered_id=ordinal_outcomes,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

    return(out_data)
  } else if(type=='predict') {

    over_iters <- sapply(1:nrow(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(sample(as.integer(ordinal_outcomes),
                      size=1,prob=c(pr_bottom,mid_prs,pr_top)))
      })
    })

    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),over_iters[c,],miss_val))
    
    attr(combined,'output') <- 'all'
    return(combined)
  } else if(type=='epred') {

    # expected category (1 = lowest): 1 + sum over k of Pr(y > k)
    return(.inflate_epred(1 + apply(pr_above, c(1,2), sum), pr_absence, inflate))

  } else if(type=='log_lik') {

    # outcome is the category, 1 to number of cutpoints + 1
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- ifelse(is_miss, 1, outcome)

    obs_ll <- sapply(seq_along(y), function(j) {
      upper <- if(y[j] > 1) pr_above[,j,y[j]-1] else rep(1, n_draws)
      lower <- if(y[j] <= n_cuts) pr_above[,j,y[j]] else rep(0, n_draws)
      log(upper - lower)
    })
    obs_ll <- matrix(obs_ll, nrow=n_draws)

    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
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
                         cov_effect=NULL,
                         person_x=NULL,
                         person_cov_effect=NULL,
                         item_x=NULL,
                         item_discrim_cov_effect=NULL,
                         item_miss_x=NULL,
                         item_miss_discrim_cov_effect=NULL,
                         miss_val=NULL,
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
  # number of cutpoints = ordinal_outcomes - 1

  if(type=='simulate') {
    n_cuts <- ordinal_outcomes - 1
    all_cuts <- sapply(1:max(item_points), function(i) {
      cutpoints <- sort(runif(n_cuts))
    })
    # Ensure all_cuts is a matrix even if n_cuts = 1
    if(!is.matrix(all_cuts)) {
      all_cuts <- matrix(all_cuts, nrow = 1)
    }
    all_cuts <- all_cuts[, item_points, drop = FALSE]

    #Generate outcomes by person and item

    cuts <- sapply(1:(ordinal_outcomes-1),function(y) {
      qlogis(pr_vote) - qlogis(all_cuts[y,])
    },simplify='array')
  } else {
    # over posterior draws
    
    cuts_iters <- sapply(1:dim(cutpoints)[1], function(i) {
        qlogis(pr_vote[i,]) - cutpoints[i,item_points]
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
    
    out_data <- tibble(outcome_disc=combined,
                           person_id=person_points,
                           ordered_id=ordinal_outcomes,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

    return(out_data)
  } else if(type=='predict') {
    over_iters <- sapply(1:nrow(pr_vote), function(d) {
      votes <- sapply(1:dim(cuts_iters)[1], function(i) {
        
        this_cut <- cuts_iters[i,d]
        
        pr_bottom <- 1 - plogis(this_cut[1])
        
        mid_prs <- sapply(1:(length(this_cut)-1), function(c) {
          plogis(this_cut[c]) - plogis(this_cut[c+1])
        })
        
        pr_top <- plogis(this_cut[length(this_cut)])
        
        return(sample(1:(length(this_cut)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top)))
      })
    })
    
    combined <- sapply(1:ncol(pr_absence), function(c) ifelse(pr_absence[,c]<(runif(N)+pr_boost),over_iters[c,],miss_val))
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
                          dbinom(1,size = 1,prob=pr_absence[,c],log=TRUE),
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
                    miss_val=NULL,
                    cov_effect=NULL,
                    person_x=NULL,
                    person_cov_effect=NULL,
                    item_x=NULL,
                    item_discrim_cov_effect=NULL,
                    item_miss_x=NULL,
                    item_miss_discrim_cov_effect=NULL,
                    ...)
{

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rpois(n = length(pr_vote),lambda = exp(qlogis(pr_vote)))
  } else if(type=='predict') {
    votes <- apply(pr_vote,2,function(c) rpois(n=length(c),lambda = exp(qlogis(c))))
  } else if(type=='epred') {
    # expected count
    return(.inflate_epred(exp(qlogis(pr_vote)), pr_absence, inflate))
  } else if(type=='log_lik') {
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 0, outcome), nrow(pr_vote))
    obs_ll <- dpois(y, lambda=exp(qlogis(pr_vote)), log=TRUE)
    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
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
    
    out_data <- tibble(outcome_disc=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula,
                        unbounded = TRUE)

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
    return(combined)
  }

}

#' Binomial outcomes (model types 17 and 18): successes out of `trials`
#'
#' `pr_vote` is the probability of success. For `type='simulate'` it is a
#' vector with one value per observation; otherwise it is a draws x
#' observations matrix, and `trials` has one value per observation.
#' @noRd
.binomial <- function(pr_absence=NULL,
                      pr_vote=NULL,
                      N=NULL,
                      inflate=NULL,
                      trials=NULL,
                      person_points=NULL,
                      item_points=NULL,
                      time_points=NULL,
                      type='simulate',
                      output=NULL,
                      outcome=NULL,
                      miss_val=NULL,
                      person_x=NULL,
                      person_cov_effect=NULL,
                      item_x=NULL,
                      item_discrim_cov_effect=NULL,
                      ...)
{

  if(type=='simulate') {

    trials <- rep_len(trials, length(pr_vote))

    votes <- rbinom(n=length(pr_vote), size=trials, prob=pr_vote)

    # remove pr of inflate if model is not inflated
    pr_boost <- if(inflate) 0 else 1

    combined <- ifelse(pr_absence<(runif(N)+pr_boost),votes,NA)

    # Create a score dataset

    out_data <- tibble(outcome_disc=combined,
                       outcome_trials=trials,
                       person_id=person_points,
                       time_id=time_points,
                       item_id=item_points,
                       group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

    return(out_data)

  }

  # posterior quantities: draws x observations matrices

  n_draws <- nrow(pr_vote)
  trials_mat <- matrix(trials, nrow=n_draws, ncol=ncol(pr_vote), byrow=TRUE)

  if(type=='predict') {

    if(output=='observed') {
      combined <- matrix(rbinom(n=length(pr_vote), size=trials_mat, prob=pr_vote),
                         nrow=n_draws)
      attr(combined,'output') <- 'observed'
      attr(combined,'output_type') <- 'discrete'
    } else if(output=='missing') {
      combined <- apply(pr_absence, 2,function(c) as.numeric(c>runif(length(c))))
      attr(combined,'output') <- 'missing'
      attr(combined,'output_type') <- 'discrete'
    }

    return(combined)

  } else if(type=='epred') {

    # expected number of successes
    return(.inflate_epred(trials_mat * pr_vote, pr_absence, inflate))

  } else if(type=='log_lik') {

    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 0, outcome), n_draws)
    obs_ll <- dbinom(y, size=trials_mat, prob=pr_vote, log=TRUE)
    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))

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
                    miss_val=NULL,
                    cov_effect=NULL,
                    person_x=NULL,
                    person_cov_effect=NULL,
                    item_x=NULL,
                    item_discrim_cov_effect=NULL,
                    item_miss_x=NULL,
                    item_miss_discrim_cov_effect=NULL,
                     ...)
{

  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rnorm(n = length(pr_vote),mean = qlogis(pr_vote),sd = sigma_sd)
  } else if(type=='predict') {
    # sigma_sd: draws of this item's residual SD
    votes <- sapply(1:ncol(pr_vote),function(c) rnorm(n=nrow(pr_vote),mean=qlogis(pr_vote[,c]),sd=sigma_sd))
  } else if(type=='epred') {
    return(.inflate_epred(qlogis(pr_vote), pr_absence, inflate))
  } else if(type=='log_lik') {
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 0, outcome), nrow(pr_vote))
    obs_ll <- dnorm(y, mean=qlogis(pr_vote), sd=sigma_sd, log=TRUE)
    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
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
    
    out_data <- tibble(outcome_cont=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula)

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
    return(combined)
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
                    miss_val=NULL,
                    cov_effect=NULL,
                    person_x=NULL,
                    person_cov_effect=NULL,
                    item_x=NULL,
                    item_discrim_cov_effect=NULL,
                    item_miss_x=NULL,
                    item_miss_discrim_cov_effect=NULL,
                    ...)
{
  
  #standard IRT 2-PL model
  if(type=='simulate') {
    votes <- rlnorm(n = length(pr_vote),meanlog = qlogis(pr_vote),sdlog = sigma_sd)
  } else if(type=='predict') {
    # sigma_sd: draws of this item's residual SD
    votes <- sapply(1:ncol(pr_vote),function(c) rlnorm(n=nrow(pr_vote),meanlog=qlogis(pr_vote[,c]),sdlog=sigma_sd))
  } else if(type=='epred') {
    # mean of the log-normal distribution
    return(.inflate_epred(exp(qlogis(pr_vote) + sigma_sd^2/2), pr_absence, inflate))
  } else if(type=='log_lik') {
    is_miss <- .miss_rows(outcome, miss_val, inflate)
    y <- .by_obs(ifelse(is_miss, 1, outcome), nrow(pr_vote))
    obs_ll <- dlnorm(y, meanlog=qlogis(pr_vote), sdlog=sigma_sd, log=TRUE)
    return(.inflate_log_lik(obs_ll, pr_absence, is_miss, inflate))
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
    
    out_data <- tibble(outcome_cont=combined,
                           person_id=person_points,
                           time_id=time_points,
                           item_id=item_points,
                           group_id='G')

    # Handle person covariates (matrix multiplication for covariate contribution)
    has_person_cov <- !is.null(person_x) && is.matrix(person_x) && ncol(person_x) >= 1 &&
                      !is.null(person_cov_effect) && any(person_cov_effect != 0)

    # Handle item covariates
    has_item_cov <- !is.null(item_x) && is.matrix(item_x) && ncol(item_x) >= 1 &&
                    !is.null(item_discrim_cov_effect) && any(item_discrim_cov_effect != 0)

    # Build covariate formulas and data
    person_cov_formula <- NULL
    item_cov_formula <- NULL

    if (has_person_cov) {
      person_cov_value <- as.vector(person_x %*% person_cov_effect)
      cov_data <- distinct(out_data, person_id, time_id) %>%
        arrange(person_id, time_id) %>%
        mutate(person_cov = person_cov_value[person_id])
      out_data <- left_join(out_data, cov_data, by = c("person_id", "time_id"))
      person_cov_formula <- ~ person_cov
    }

    if (has_item_cov) {
      item_cov_value <- as.vector(item_x %*% item_discrim_cov_effect)
      item_cov_data <- tibble(item_id = 1:nrow(item_x), item_cov = item_cov_value)
      out_data <- left_join(out_data, item_cov_data, by = "item_id")
      item_cov_formula <- ~ item_cov
    }

    out_data <- id_make(score_data = out_data,
                        person_cov = person_cov_formula,
                        item_cov = item_cov_formula,
                        unbounded = TRUE)

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
    return(combined)
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
      return(tibble(t_11))
    } else {
      if(adj_in==1) {
        t_11 <- adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
      } else {
        t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
      }
      
    }
    current_val$t1 <- t_11
    return(tibble(t_11))
  })  %>% bind_rows
  return(out_vec)
}
