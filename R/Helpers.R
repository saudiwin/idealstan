

# Data Preparation for id_estimate ----------------------------------------

#' Custom match.call that also captures default arguments
#' From https://stackoverflow.com/questions/14397364/match-call-with-default-arguments
#' @noRd
.match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )
  
  
  match.call(sys.function(sys.parent()), call)
}

#' converts an id_make object to a Stan list
#' @noRd
.make_stan_data <- function(idealdata=NULL,...) {
  
  # make all the original arguments accessible
  
  # Capture the arguments as a list
  args <- list(...)
  # Assign each argument as a variable in the current environment
  list2env(args, envir = environment())
  
  # check on restriction length
  
  if((length(restrict_ind_high) > 1 || num_restrict_high > 1) && (length(fix_high)==1)) {
    
    fix_high <- rep(fix_high, pmax(length(restrict_ind_high), num_restrict_high))
    
  }
  
  if((length(restrict_ind_high) > 1 || num_restrict_high > 1) && (length(restrict_sd_high)==1 || length(restrict_N_high)==1)) {
    
    restrict_N_high <- rep(restrict_N_high, pmax(length(restrict_ind_high), num_restrict_high))
    restrict_sd_high <- rep(restrict_sd_high, pmax(length(restrict_ind_high), num_restrict_high))
    
  }

  
  if(!is.null(restrict_ind_high)) names(restrict_ind_high) <- fix_high
  
  if((length(restrict_ind_low) > 1 || num_restrict_low > 1) && (length(fix_low)==1)) {
    
    fix_low <- rep(fix_low, pmax(length(restrict_ind_low), num_restrict_low))

  }
  
  if((length(restrict_ind_low) > 1 || num_restrict_low > 1) && (length(restrict_sd_low)==1 || length(restrict_N_low)==1)) {
    
    restrict_N_low <- rep(restrict_N_low, pmax(length(restrict_ind_low), num_restrict_low))
    restrict_sd_low <- rep(restrict_sd_low, pmax(length(restrict_ind_low), num_restrict_low))
    
  }
  
  if(fixtype != "vb_full" && (length(fix_high)>length(restrict_ind_high) || length(restrict_N_high)>length(restrict_ind_high) || length(restrict_sd_high)>length(restrict_ind_high))) stop("Please pass a number of discrimination constrained parameters restrict_N_high/restrict_sd_high that is equal to the number of constrained parameters passed to fix_high.")
  
  if(fixtype != "vb_full" && (length(fix_low)>length(restrict_ind_low) || length(restrict_N_low)>length(restrict_ind_low) || length(restrict_sd_low)>length(restrict_ind_low))) stop("Please pass a number of discrimination constrained parameters restrict_N_low/restrict_sd_low that is equal to the number of constrained parameters passed to fix_low.")

  
  if(!is.null(restrict_ind_low)) names(restrict_ind_low) <- fix_low
  
  
  if(use_subset || sample_it) {
    idealdata <- subset_ideal(idealdata,use_subset=use_subset,sample_it=sample_it,subset_group=subset_group,
                              subset_person=subset_person,sample_size=sample_size)
  }
  
  #Using an un-identified model with variational inference, find those parameters that would be most useful for
  #constraining/pinning to have an identified model for full Bayesian inference
  
  # change time IDs if non time-varying model is being fit
  if(vary_ideal_pts=='none') {
    idealdata@score_matrix$time_id <- 1
  } 
  
  vary_ideal_pts <- switch(vary_ideal_pts,
                           none=1,
                           random_walk=2,
                           AR1=3,
                           GP=4,
                           splines=5)
  
  if(is.null(vary_ideal_pts))
    stop("Please pass an option to vary_ideal_pts that is either 'none', 'random_walk', 'AR1', 'GP' or 'splines'.")
  
  # number of combined posterior models
  
  mod_count <- length(unique(idealdata@score_matrix$model_id))
  
  if(length(unique(idealdata@score_matrix$ordered_id))>1) {
    mod_count <- mod_count + length(unique(idealdata@score_matrix$ordered_id)) - 1
  }
  
  
  
  # set GP parameters
  
  # check if varying model IDs exist and replace with model type
  # if not
  
  if(is.na(idealdata@score_matrix$model_id[1]) || idealdata@score_matrix$model_id[1]=="missing") {
    
    idealdata@score_matrix$model_id <- model_type
    idealdata@score_matrix$discrete <- as.numeric(model_type %in% c(1,2,3,4,5,6,7,8,13,14))
    
  } else {
    if(!is.numeric(idealdata@score_matrix$model_id)) {
      stop("Column model_id in the data is not a numeric series of 
           integers. Please pass a numeric value in the id_make function
           for model_id based on the available model types.")
    }
  }
  
  if((!(all(c(restrict_ind_high,restrict_ind_low) %in% levels(idealdata@score_matrix$person_id))) && !use_groups) && const_type=="persons") {
    
    stop("Your restricted persons/items are not in the data.")
    
  } else if(!(all(c(restrict_ind_high,restrict_ind_low) %in% levels(idealdata@score_matrix$item_id))) && const_type=="items") {
    
    stop("Your restricted persons/items are not in the data.")
    
  } else if((!(all(c(restrict_ind_high,restrict_ind_low) %in% levels(idealdata@score_matrix$group_id)))  && use_groups) && const_type=="persons") {
    
    stop("Your restricted persons/items are not in the data.")
    
  }
  
  # check if ordinal categories exist in the data if model_id>1
  
  if(any(c(3,4,5,6) %in% idealdata@score_matrix$model_id)) {
    if(is.null(idealdata@score_matrix$ordered_id) || !is.numeric(idealdata@score_matrix$ordered_id)) {
      stop("If you have an ordered categorical variable in a multi-distribution model, you must include a column in the data with the number of ordered categories for each row in the data.\n
             See id_make documentation for more info.")
    }
  } else {
    idealdata@score_matrix$ordered_id <- 0
  }
  
  # sort data according to shard
  
  if(map_over_id=="persons") {
    
    if(use_groups) {
      
      idealdata@score_matrix <- arrange(idealdata@score_matrix, group_id, time_id)
      
    } else {
      
      idealdata@score_matrix <- arrange(idealdata@score_matrix, person_id, time_id)
      
    }
    
  } else {
    
    idealdata@score_matrix <- arrange(idealdata@score_matrix, item_id, time_id)
    
  }
  
  # use either row numbers for person/legislator IDs or use group IDs (static or time-varying)
  
  if(use_groups) {
    legispoints <- as.numeric(idealdata@score_matrix$group_id)
  } else {
    legispoints <- as.numeric(idealdata@score_matrix$person_id)
  }
  
  billpoints <- as.numeric(idealdata@score_matrix$item_id)
  timepoints <- as.numeric(factor(as.numeric(idealdata@score_matrix$time_id)))
  modelpoints <- as.integer(idealdata@score_matrix$model_id)
  ordered_id <- as.integer(idealdata@score_matrix$ordered_id)
  # for gaussian processes, need actual time values
  time_ind <- switch(class(idealdata@score_matrix$time_id)[1],
                     factor=unique(as.numeric(idealdata@score_matrix$time_id)),
                     Date=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXct=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXlt=unique(as.numeric(idealdata@score_matrix$time_id)),
                     numeric=unique(idealdata@score_matrix$time_id),
                     integer=unique(idealdata@score_matrix$time_id))
  
  # Calculate number of time points for spline validation
  time_points <- length(time_ind)

  if(vary_ideal_pts==5) {

    if(!is.null(spline_knots) && length(spline_knots)==1 && spline_knots < 1)
      stop("Please pass a value for the number of spline_knots that is at least equal to 1 but less than the number of time points.")
    
    if(spline_degree < 1)
      stop("Please pass a value for spline_degree that is at least 1.")
    
    # code borrowed from very helpful Stan documentation by Milad Kharratzadeh
    # see https://github.com/milkha/Splines_in_Stan/blob/master/splines_in_stan.pdf
    
    # need to rescale time_ind to an interval that is easy for sampling
    # first get location of knots if they are present
    
    if(!is.null(spline_knots) && length(spline_knots)>1) {
      
      spline_knots <- switch(class(spline_knots),
                             factor=unique(as.numeric(spline_knots,
                                                      levels=levels(idealdata@score_matrix$time_id))),
                             Date=unique(as.numeric(spline_knots)),
                             POSIXct=unique(as.numeric(spline_knots)),
                             POSIXlt=unique(as.numeric(spline_knots)),
                             numeric=unique(spline_knots),
                             integer=unique(spline_knots))
      
      spline_knots <- which(time_ind %in% spline_knots)
      
    }
    
    old_bounds <- c(min(time_ind,na.rm=T),max(time_ind, na.rm=T))
    time_ind <- 2 * ((time_ind - min(time_ind))/(max(time_ind) - min(time_ind))) - 1
    
    if(!is.null(spline_knots) && length(spline_knots)>1) {
      
      if(length(spline_knots)>(time_points-2)) stop("Please pass along knots that are 2 less than the number of time points.")
      
      spline_knots <- (1:time_points)[spline_knots]
      
    } else if(!is.null(spline_knots)) {
      
      if(spline_knots>(time_points-2)) stop("Please pass along knots that are 2 less than the number of time points.")
      
      spline_knots <- quantile((1:time_points), 
                               probs=seq(0,1,length.out=spline_knots+2))
      
      # remove first and last knot, these should be internal
      
      spline_knots <- spline_knots[2:(length(spline_knots)-1)]
      
    }
    
    B <- t(splines::bs(time_ind, 
                       knots = spline_knots,
                       degree = spline_degree,intercept=TRUE))
    
    num_basis <- nrow(B)
    
    T_spline <- length(unique(time_ind))
    
  } else {
    
    B <- matrix(0L,
                nrow=1,ncol=1)
    num_basis <- 1
    T_spline <- 1
    
  }
  
  if((any(c(9,10,11,12,15,16) %in% idealdata@score_matrix$model_id)) && (any(c(1,2,3,4,5,6,7,8,13,14) %in% idealdata@score_matrix$model_id))) {
    Y_int <- idealdata@score_matrix$outcome_disc
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else if (any(c(9,10,11,12,15,16) %in% idealdata@score_matrix$model_id)) {
    Y_int <- array(0)
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else {
    Y_cont <- array(0)
    Y_int <- idealdata@score_matrix$outcome_disc
  }
  
  
  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
  # need to have a different way to remove missing values if multiple
  # posteriors used
  # set values for length of discrete/continuous outcomes  
  remove_list <- .remove_nas(Y_int,
                             Y_cont,
                             discrete=idealdata@score_matrix$discrete,
                             legispoints,
                             billpoints,
                             timepoints,
                             modelpoints,
                             ordered_id,
                             idealdata,
                             time_ind=as.array(time_ind),
                             time_proc=vary_ideal_pts,
                             ar_prior=ar_prior,
                             const_type=switch(const_type,
                                               persons=1L,
                                               items=2L),
                             discrim_reg_scale=discrim_reg_scale,
                             discrim_reg_shape=discrim_reg_shape,
                             discrim_miss_scale=discrim_miss_scale,
                             discrim_miss_shape=discrim_miss_shape,
                             diff_reg_sd=diff_reg_sd,
                             diff_miss_sd=diff_miss_sd,
                             legis_sd=person_sd,
                             restrict_sd_high=restrict_sd_high,
                             restrict_sd_low=restrict_sd_low,
                             restrict_N_high=restrict_N_high,
                             restrict_N_low=restrict_N_low,
                             restrict_high=restrict_ind_high,
                             restrict_low=restrict_ind_low,
                             fix_high=as.numeric(names(restrict_ind_high)),
                             fix_low=as.numeric(names(restrict_ind_low)))
  
  # need to create new data if map_rect is in operation 
  # and we have missing values / ragged arrays
  
  out_list <- .make_sum_vals(idealdata@score_matrix,map_over_id,use_groups=use_groups,
                             remove_nas=remove_list$remove_nas)
  
  sum_vals <- out_list$sum_vals
  
  # need number of shards
  
  S <- nrow(sum_vals)
  
  # create IDs for auxiliary parameters (normal/lognormal/ordbeta)
  
  if(het_var) {
    
    # for normal/lognormal (the sigma parameter is essentially identical)
    
    num_var <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(9,10,11,12)]))
    
    # for ordbeta
    
    num_ordbeta <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(15,16)]))
    
    mod_items <- tibble(model_id=remove_list$modelpoints,
                        item_id=remove_list$billpoints) %>% 
      distinct
    
    mod_items_sigma <- mutate(mod_items,normal_mods=model_id %in% c(9,10,11,12)) %>% 
      group_by(normal_mods) %>% 
      mutate(num_var=1:n())
    
    mod_items_phi <- mutate(mod_items,ordbeta_mods=model_id %in% c(15,16)) %>% 
      group_by(ordbeta_mods) %>% 
      mutate(ordbeta_id=1:n())
    
    type_het_var <- arrange(mod_items_sigma, item_id) %>% pull(num_var)
    
    ordbeta_id <- arrange(mod_items_phi, item_id) %>% pull(ordbeta_id)
    
    
  }
  
  # check for boundary priors
  
  if(!is.null(boundary_prior)) {
    
    if(is.null(boundary_prior$beta)) {
      
      stop("If you want to use a boundary-avoiding prior for time-series variance, please pass a list with an element named beta, i.e. list(beta=1).")
      
    }
    
    if(boundary_prior$beta <= 0) {
      
      stop("Boundary prior beta value must be strictly positive (i.e. greater than 0).")
      
    }
    
    inv_gamma_beta <- boundary_prior$beta
    
  } else {
    
    inv_gamma_beta <- 0
    
  }
  
  if(remove_list$N_cont>0) {
    
    Y_cont <- remove_list$Y_cont[out_list$this_data$orig_order]
    
  } else {
    
    Y_cont <- remove_list$Y_cont
    
  }
  
  if(remove_list$N_int>0) {
    
    Y_int <- remove_list$Y_int[out_list$this_data$orig_order]
    
    order_cats_rat <- remove_list$order_cats_rat[out_list$this_data$orig_order]
    order_cats_grm <- remove_list$order_cats_grm[out_list$this_data$orig_order]
    
  } else {
    
    Y_int <- remove_list$Y_int
    order_cats_rat <- remove_list$order_cats_rat
    order_cats_grm <- remove_list$order_cats_grm
    
  }
  
  if(!is.null(ignore_db)) {
    
    # check for correct columns
    
    if(!all(c("person_id",
              "time_id",
              "ignore") %in% names(ignore_db))) {
      stop("Ignore DB does not have the three necessary columns: time_id, person_id, and ignore (0 or 1).")
      
    }
    
    ignore_db <- mutate(ungroup(ignore_db),
                        person_id=as.numeric(person_id),
                        time_id=as.numeric(factor(as.numeric(ignore_db$time_id))))
    
    # filter out time_ids not in main data
    
    ignore_db <- filter(ignore_db, time_id %in% remove_list$timepoints,
                        person_id %in% remove_list$legispoints) %>% 
      select(person_id,time_id,ignore) %>% 
      arrange(person_id,time_id) %>% 
      as.matrix
    
  } else {
    
    ignore_db <- matrix(nrow=0,ncol=3)
    
  }
  
  if(idealdata@person_cov[1]=="personcov0") {
    
    legis_pred <- remove_list$legis_pred
    
  } else {
    
    
    legis_pred <- remove_list$legis_pred[out_list$this_data$orig_order,,drop=FALSE]
  }
  
  if(idealdata@item_cov[1]=="itemcov0") {
    
    srx_pred <- remove_list$srx_pred
    
  } else {
    
    srx_pred <- remove_list$srx_pred[out_list$this_data$orig_order,,drop=FALSE]
    
  }
  
  if(idealdata@item_cov_miss[1]=="itemcovmiss0") {
    
    sax_pred <- remove_list$sax_pred
    
  } else {
    
    sax_pred <- remove_list$sax_pred[out_list$this_data$orig_order,,drop=FALSE]
    
  }


  
  this_data <- list(N=remove_list$N,
  N_cont=remove_list$N_cont,
  N_int=remove_list$N_int,
  Y_int=Y_int,
  Y_cont=Y_cont,
  y_int_miss=remove_list$y_int_miss,
  y_cont_miss=remove_list$y_cont_miss,
  num_var=num_var,
  type_het_var=array(type_het_var),
  B=B,
  debug_mode=debug_mode,
  num_basis=num_basis,
  T_spline=T_spline,
  S=nrow(sum_vals),
  S_type=as.numeric(map_over_id=="persons"),
  T=remove_list$max_t,
  num_legis=remove_list$num_legis,
  num_bills=remove_list$num_bills,
  num_ls=remove_list$num_ls,
  ll=remove_list$legispoints[out_list$this_data$orig_order],
  bb=remove_list$billpoints[out_list$this_data$orig_order],
  mm=remove_list$modelpoints[out_list$this_data$orig_order],
  ignore=as.numeric(nrow(ignore_db)>0),
  ignore_db=ignore_db,
  mod_count=length(unique(remove_list$modelpoints)),
  num_restrict_high=as.integer(1),
  num_restrict_low=as.integer(1),
  tot_cats=length(remove_list$n_cats_rat),
  n_cats_rat=remove_list$n_cats_rat,
  n_cats_grm=remove_list$n_cats_grm,
  order_cats_rat=order_cats_rat,
  order_cats_grm=order_cats_grm,
  num_bills_grm=ifelse(any(remove_list$modelpoints %in% c(5,6)),
                       remove_list$num_bills,0L),
  LX=remove_list$LX,
  SRX=remove_list$SRX,
  SAX=remove_list$SAX,
  legis_pred=legis_pred,
  srx_pred=srx_pred,
  sax_pred=sax_pred,
  time=remove_list$timepoints[out_list$this_data$orig_order],
  time_proc=vary_ideal_pts,
  discrim_reg_upb=discrim_reg_upb - discrim_reg_lb,
  discrim_reg_lb=discrim_reg_lb,
  discrim_miss_upb=discrim_miss_upb - discrim_miss_lb,
  discrim_miss_lb=discrim_miss_lb,
  discrim_reg_scale=discrim_reg_scale,
  discrim_reg_shape=discrim_reg_shape,
  discrim_abs_scale=discrim_miss_scale,
  discrim_abs_shape=discrim_miss_shape,
  diff_reg_sd=diff_reg_sd,
  diff_abs_sd=diff_miss_sd,
  legis_sd=person_sd,
  restrict_sd_high=discrim_reg_scale,
  restrict_sd_low=discrim_reg_shape,
  restrict_N_high=discrim_reg_scale,
  restrict_N_low=discrim_reg_shape,
  time_sd=time_fix_sd,
  time_var_sd=time_var,
  ar1_up=ar1_up,
  ar1_down=ar1_down,
  inv_gamma_beta=inv_gamma_beta,
  center_cutoff=as.integer(time_center_cutoff),
  restrict_var=restrict_var,
  ar_prior=ar_prior,
  zeroes=as.numeric(inflate_zero),
  time_ind=as.array(time_ind),
  gp_rho=gp_rho,
  gp_alpha=gp_alpha,
  gp_nugget=gp_nugget,
  id_refresh=id_refresh,
  sum_vals=as.matrix(sum_vals),
  const_type=switch(const_type,
                    persons=1L,
                    items=2L),
  restrict_high=1,
  restrict_low=2,
  fix_high=0,
  fix_low=0,
  pos_discrim=as.integer(sign(discrim_reg_upb)==sign(discrim_reg_lb)),
  grainsize=grainsize,
  prior_only=as.integer(prior_only),
  num_ordbeta=num_ordbeta,
  ordbeta_id=ordbeta_id,
  phi_mean=rep(ordbeta_phi_mean, times=num_ordbeta),
  ordbeta_cut_phi=rep(ordbeta_cut_phi,times=num_ordbeta),
  ordbeta_cut_alpha=matrix(rep(ordbeta_cut_alpha,times=num_ordbeta),nrow=num_ordbeta,
                           ncol=3,byrow=T))
  
  idealdata <- id_model(object=idealdata,fixtype=fixtype,this_data=this_data,
                        restrict_ind_high=restrict_ind_high,
                        restrict_ind_low=restrict_ind_low,
                        ncores=ncores,
                        use_groups=use_groups,
                        fix_high=fix_high,
                        fix_low=fix_low,
                        num_restrict_high=num_restrict_high,
                        num_restrict_low=num_restrict_low,
                        const_type=const_type)
  
  restrict_ind_high <- idealdata@restrict_ind_high
  restrict_ind_low <- idealdata@restrict_ind_low
  
  if(("outcome_cont" %in% names(idealdata@score_matrix)) && ("outcome_disc" %in% names(idealdata@score_matrix))) {
    Y_int <- idealdata@score_matrix$outcome_disc
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else if ("outcome_cont" %in% names(idealdata@score_matrix)) {
    Y_int <- array(0)
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else {
    Y_cont <- array(0)
    Y_int <- idealdata@score_matrix$outcome_disc
  }
  
  
  # need to redo everything post-identification
  
  remove_list <- .remove_nas(Y_int,
                             Y_cont,
                             discrete=idealdata@score_matrix$discrete,
                             legispoints,
                             billpoints,
                             timepoints,
                             modelpoints,
                             ordered_id,
                             idealdata,
                             time_ind=as.array(time_ind),
                             time_proc=vary_ideal_pts,
                             ar_prior=ar_prior,
                             const_type=switch(const_type,
                                               persons=1L,
                                               items=2L),
                             discrim_reg_scale=discrim_reg_scale,
                             discrim_reg_shape=discrim_reg_shape,
                             discrim_miss_scale=discrim_miss_scale,
                             discrim_miss_shape=discrim_miss_shape,
                             diff_reg_sd=diff_reg_sd,
                             diff_miss_sd=diff_miss_sd,
                             legis_sd=person_sd,
                             restrict_sd_high=restrict_sd_high,
                             restrict_sd_low=restrict_sd_low,
                             restrict_N_high=restrict_N_high,
                             restrict_N_low=restrict_N_low,
                             restrict_high=restrict_ind_high,
                             restrict_low=restrict_ind_low,
                             fix_high=as.numeric(names(restrict_ind_high)),
                             fix_low=as.numeric(names(restrict_ind_low)))
  
  idealdata <- remove_list$idealdata
  
  # need to create new data if map_rect is in operation 
  # and we have missing values / ragged arrays
  
  out_list <- .make_sum_vals(idealdata@score_matrix,map_over_id,use_groups=use_groups,
                             remove_nas=remove_list$remove_nas)
  
  sum_vals <- out_list$sum_vals
  
  # make sure data is re-sorted by ID
  
  idealdata@score_matrix <- out_list$this_data
  
  # need number of shards
  
  S <- nrow(sum_vals)
  
  # check for heterogenous variances
  
  if(het_var) {
    
    # for normal/lognormal (the sigma parameter is essentially identical)
    
    num_var <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(9,10,11,12)]))
    
    # for ordbeta
    
    num_ordbeta <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(15,16)]))
    
    mod_items <- tibble(model_id=remove_list$modelpoints,
                        item_id=remove_list$billpoints) %>% 
      distinct
    
    mod_items_sigma <- mutate(mod_items,normal_mods=model_id %in% c(9,10,11,12)) %>% 
      group_by(normal_mods) %>% 
      mutate(num_var=1:n())
    
    mod_items_phi <- mutate(mod_items,ordbeta_mods=model_id %in% c(15,16)) %>% 
      group_by(ordbeta_mods) %>% 
      mutate(ordbeta_id=1:n())
    
    type_het_var <- arrange(mod_items_sigma, item_id) %>% pull(num_var)
    
    ordbeta_id <- arrange(mod_items_phi, item_id) %>% pull(ordbeta_id)
    
    
  }
  
  if(remove_list$N_cont>0) {
    
    Y_cont <- remove_list$Y_cont[out_list$this_data$orig_order]
    
  } else {
    
    Y_cont <- remove_list$Y_cont
    
  }
  
  if(remove_list$N_int>0) {
    
    Y_int <- remove_list$Y_int[out_list$this_data$orig_order]
    
    order_cats_rat <- remove_list$order_cats_rat[out_list$this_data$orig_order]
    order_cats_grm <- remove_list$order_cats_grm[out_list$this_data$orig_order]
    
  } else {
    
    Y_int <- remove_list$Y_int
    order_cats_rat <- remove_list$order_cats_rat
    order_cats_grm <- remove_list$order_cats_grm
    
  }
  
  if(idealdata@person_cov[1]=="personcov0") {
    
    legis_pred <- remove_list$legis_pred
    
  } else {
    
    
    legis_pred <- remove_list$legis_pred[out_list$this_data$orig_order,,drop=FALSE]
  }
  
  if(idealdata@item_cov[1]=="itemcov0") {
    
    srx_pred <- remove_list$srx_pred
    
  } else {
    
    srx_pred <- remove_list$srx_pred[out_list$this_data$orig_order,,drop=FALSE]
    
  }
  
  if(idealdata@item_cov_miss[1]=="itemcovmiss0") {
    
    sax_pred <- remove_list$sax_pred
    
  } else {
    
    sax_pred <- remove_list$sax_pred[out_list$this_data$orig_order,,drop=FALSE]
    
  }
  
  this_data <- list(N=remove_list$N,
                    N_cont=remove_list$N_cont,
                    N_int=remove_list$N_int,
                    Y_int=Y_int,
                    Y_cont=Y_cont,
                    y_int_miss=remove_list$y_int_miss,
                    y_cont_miss=remove_list$y_cont_miss,
                    num_var=num_var,
                    type_het_var=array(type_het_var),
                    B=B,
                    debug_mode=debug_mode,
                    num_basis=num_basis,
                    T_spline=T_spline,
                    S=nrow(sum_vals),
                    S_type=as.numeric(map_over_id=="persons"),
                    T=remove_list$max_t,
                    num_legis=remove_list$num_legis,
                    num_bills=remove_list$num_bills,
                    num_ls=remove_list$num_ls,
                    #num_bills_grm=remove_list$num_bills_grm,
                    ll=remove_list$legispoints[out_list$this_data$orig_order],
                    bb=remove_list$billpoints[out_list$this_data$orig_order],
                    mm=remove_list$modelpoints[out_list$this_data$orig_order],
                    ignore=as.numeric(nrow(ignore_db)>0),
                    ignore_db=ignore_db,
                    mod_count=length(unique(remove_list$modelpoints)),
                    tot_cats=length(remove_list$n_cats_rat),
                    n_cats_rat=remove_list$n_cats_rat,
                    n_cats_grm=remove_list$n_cats_grm,
                    order_cats_rat=order_cats_rat,
                    order_cats_grm=order_cats_grm,
                    num_bills_grm=ifelse(any(remove_list$modelpoints %in% c(5,6)),
                                         remove_list$num_bills,0L),
                    LX=remove_list$LX,
                    SRX=remove_list$SRX,
                    SAX=remove_list$SAX,
                    legis_pred=legis_pred,
                    srx_pred=srx_pred,
                    sax_pred=sax_pred,
                    time=remove_list$timepoints[out_list$this_data$orig_order],
                    time_proc=vary_ideal_pts,
                    discrim_reg_upb=discrim_reg_upb - discrim_reg_lb,
                    discrim_reg_lb=discrim_reg_lb,
                    discrim_miss_upb=discrim_miss_upb - discrim_miss_lb,
                    discrim_miss_lb=discrim_miss_lb,
                    discrim_reg_scale=discrim_reg_scale,
                    discrim_reg_shape=discrim_reg_shape,
                    discrim_abs_scale=discrim_miss_scale,
                    discrim_abs_shape=discrim_miss_shape,
                    diff_reg_sd=diff_reg_sd,
                    diff_abs_sd=diff_miss_sd,
                    legis_sd=person_sd,
                    restrict_sd_high=restrict_sd_high,
                    restrict_sd_low=restrict_sd_low,
                    restrict_N_high=restrict_N_high,
                    restrict_N_low=restrict_N_low,
                    time_sd=time_fix_sd,
                    time_var_sd=time_var,
                    ar1_up=ar1_up,
                    ar1_down=ar1_down,
                    inv_gamma_beta=inv_gamma_beta,
                    center_cutoff=as.integer(time_center_cutoff),
                    restrict_var=restrict_var,
                    ar_prior=ar_prior,
                    zeroes=as.numeric(inflate_zero),
                    time_ind=as.array(time_ind),
                    gp_rho=gp_rho,
                    gp_alpha=gp_alpha,
                    gp_nugget=gp_nugget,
                    id_refresh=id_refresh,
                    sum_vals=as.matrix(sum_vals),
                    const_type=switch(const_type,
                                      persons=1L,
                                      items=2L),
                    num_restrict_high=length(restrict_ind_high),
                    num_restrict_low=length(restrict_ind_low),
                    restrict_high=as.integer(restrict_ind_high),
                    restrict_low=as.integer(restrict_ind_low),
                    fix_high=as.integer(names(restrict_ind_high)),
                    fix_low=as.integer(names(restrict_ind_low)),
                    pos_discrim=as.integer(sign(discrim_reg_upb)==sign(discrim_reg_lb)),
                    grainsize=grainsize,
                    prior_only=as.integer(prior_only),
                    num_ordbeta=num_ordbeta,
                    ordbeta_id=ordbeta_id,
                    phi_mean=rep(ordbeta_phi_mean, times=num_ordbeta),
                    ordbeta_cut_phi=rep(ordbeta_cut_phi,times=num_ordbeta),
                    ordbeta_cut_alpha=matrix(rep(ordbeta_cut_alpha,times=num_ordbeta),nrow=num_ordbeta,
                                             ncol=3,byrow=T))
  
  
  return(list(stan_data=this_data,
              remove_list=remove_list,
              idealdata=idealdata,
              out_list=out_list))
  
  
}


# Generalized Beta Distribution Functions ---------------------------------

#' Define the PDF of the generalized beta distribution
#' @noRd
.genbeta_pdf <- function(y, alpha, beta, lb, lb_offset) {
  x <- (y - lb) / lb_offset
  pdf_value <- (x^(alpha - 1)) * ((1 - x)^(beta - 1)) / (lb_offset * beta(alpha, beta))
  return(pdf_value)
}

#' Function to generate random samples from the generalized beta distribution
#' @noRd
.genbeta_sample <- function(n, alpha, beta, lb, lb_offset) {
  # Initialize a vector to store samples
  samples <- numeric(n)
  count <- 0
  
  # Set up the rejection sampling loop
  while (count < n) {
    # Propose values between lb and lb + lb_offset
    y_proposal <- runif(1, lb, lb + lb_offset)
    u <- runif(1)  # Uniformly distributed random number for acceptance
    
    # Calculate acceptance probability
    pdf_val <- .genbeta_pdf(y_proposal, alpha, beta, lb, lb_offset)
    max_pdf <- .genbeta_pdf(lb + lb_offset / 2, alpha, beta, lb, lb_offset) # peak near center
    
    if (u < pdf_val / max_pdf) {
      # Accept the proposal
      count <- count + 1
      samples[count] <- y_proposal
    }
  }
  
  return(samples)
}


# Functions imported from cmdstanr for pulling inits out of pathfinder
# needed because sometimes pathfinder produces inits that evaluate to log(0)

#' @noRd
validate_fit_init = function(init, model_variables) {
  # Convert from data.table to data.frame
  if (all(init$return_codes() == 1)) {
    stop("We are unable to create initial values from a model with no samples. Please check the results of the model used for inits before continuing.")
  } else if (!is.null(model_variables) &&!any(names(model_variables$parameters) %in% init$metadata()$stan_variables)) {
    stop("None of the names of the parameters for the model used for initial values match the names of parameters from the model currently running.")
  }
}

#' Remove the leftmost dimension if equal to 1
#' @noRd
#' @param x An array like object
.remove_leftmost_dim <- function(x) {
  dims <- dim(x)
  if (length(dims) == 1) {
    return(drop(x))
  } else if (dims[1] == 1) {
    new_dims <- dims[-1]
    # Create a call to subset the array, maintaining all remaining dimensions
    subset_expr <- as.call(c(as.name("["), list(x), 1, rep(TRUE, length(new_dims)), drop = FALSE))
    new_x <- eval(subset_expr)
    return(array(new_x, dim = new_dims))
  } else {
    return(x)
  }
}

# function that actually writes the to files 
# only takes lists as input

#' Write initial values to files if provided as list of lists
#' @param init List of init lists.
#' @param num_procs Number of inits needed.
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @noRd
process_init_list <- function(init, num_procs, model_variables = NULL,
                              warn_partial = FALSE,
                              ...) {
  if (!all(sapply(init, function(x) is.list(x) && !is.data.frame(x)))) {
    stop("If 'init' is a list it must be a list of lists.", call. = FALSE)
  }
  if (length(init) != num_procs) {
    stop("'init' has the wrong length. See documentation of 'init' argument.", call. = FALSE)
  }
  if (any(sapply(init, function(x) length(x) == 0))) {
    stop("'init' contains empty lists.", call. = FALSE)
  }
  if (!is.null(model_variables)) {
    missing_parameter_values <- list()
    parameter_names <- names(model_variables$parameters)
    for (i in seq_along(init)) {
      is_parameter_value_supplied <- parameter_names %in% names(init[[i]])
      if (!all(is_parameter_value_supplied)) {
        missing_parameter_values[[i]] <- parameter_names[!is_parameter_value_supplied]
      }
      for (par_name in parameter_names[is_parameter_value_supplied]) {
        # Make sure that initial values for single-element containers don't get
        # unboxed when writing to JSON
        if (model_variables$parameters[[par_name]]$dimensions == 1 && length(init[[i]][[par_name]]) == 1) {
          init[[i]][[par_name]] <- array(init[[i]][[par_name]], dim = 1)
        }
      }
    }
    if (length(missing_parameter_values) > 0 && isTRUE(warn_partial)) {
      warning_message <- c(
        "Init values were only set for a subset of parameters. \nMissing init values for the following parameters:\n"
      )
      for (i in seq_along(missing_parameter_values)) {
        if (length(init) > 1) {
          line_text <- paste0(" - chain ", i, ": ")
        } else {
          line_text <- ""
        }
        if (length(missing_parameter_values[[i]]) > 0) {
          warning_message <- c(warning_message, paste0(line_text, paste0(missing_parameter_values[[i]], collapse = ", "), "\n"))
        }
      }
      warning_message <- c(warning_message, "\nTo disable this message use options(cmdstanr_warn_inits = FALSE).\n")
      message(warning_message)
    }
  }
  if (any(grepl("\\[", names(unlist(init))))) {
    stop(
      "'init' contains entries with parameter names that include square-brackets, which is not permitted. ",
      "To supply inits for a vector, matrix or array of parameters, ",
      "create a single entry with the parameter's name in the 'init' list ",
      "and specify initial values for the entire parameter container.",
      call. = FALSE)
  }
  init_paths <-
    tempfile(
      pattern = "init-",
      tmpdir = tempdir(),
      fileext = ""
    )
  init_paths <- paste0(init_paths, "_", seq_along(init), ".json")
  for (i in seq_along(init)) {
    cmdstanr::write_stan_json(init[[i]], init_paths[i])
  }
  init_paths
}


#' Write initial values to files if provided as posterior `draws` object
#' (taken from cmdstanr package)
#' @noRd
#' @param init A type that inherits the `posterior::draws` class.
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @importFrom posterior as_draws_df subset_draws draws_of variables
process_init_draws <- function(init, num_procs, model_variables = NULL,
                               warn_partial = FALSE,
                               ...) {
  
  init$weight <- NULL
  
  variable_names <- variables(init, with_indices = F)
  
  draws <- as_draws_df(init)
  
  # Since all other process_init functions return `num_proc` inits
  # This will only happen if a raw draws object is passed
  if (nrow(draws) < num_procs) {
    idx <- rep(1:nrow(draws), ceiling(num_procs / nrow(draws)))[1:num_procs]
    draws <- draws[idx,]
  } else if (nrow(draws) > num_procs) {
    draws <- resample_draws(draws, ndraws = num_procs,
                                       method ="simple_no_replace")
  }
  draws_rvar = as_draws_rvars(draws)
  variable_names <- variable_names[variable_names %in% names(draws_rvar)]
  variable_names <- variable_names[!(variable_names %in% c("lp__",
                                                              "time_var_full"))]
  
  model_variables <- model_variables[names(model_variables) %in% names(draws_rvar)]
  
  # remove lp__ and time_var_full
  
  
  
  draws_rvar <- subset_draws(draws_rvar, variable = variable_names)

  inits = lapply(1:num_procs, function(draw_iter) {
    init_i = lapply(variable_names, function(var_name) {
      x = .remove_leftmost_dim(posterior::draws_of(
        posterior::subset_draws(draws_rvar[[var_name]], draw=draw_iter)))
      if (any(model_variables[[var_name]] == 0) || any(is.nan(model_variables[[var_name]]))) {
        return(as.double(x))
      } else {
        return(x)
      }
    })
    bad_names = unlist(lapply(variable_names, function(var_name) {
      x = drop(posterior::draws_of(drop(
        posterior::subset_draws(draws_rvar[[var_name]], draw=draw_iter))))
      if (any(is.infinite(x)) || any(is.na(x)) || any(is.nan(x))) {
        return(var_name)
      }
      return("")
    }))
    any_na_or_inf = bad_names != ""
    if (any(any_na_or_inf)) {
      err_msg = paste0(paste(bad_names[any_na_or_inf], collapse = ", "), " contains NA or Inf values!")
      if (length(any_na_or_inf) > 1) {
        err_msg = paste0("Variables: ", err_msg)
      } else {
        err_msg = paste0("Variable: ", err_msg)
      }
      stop(err_msg)
    }
    names(init_i) = variable_names
    return(init_i)
  })

  return(inits)
  
}


#' @noRd
#' @importFrom posterior resample_draws
process_init_pathfinder <- function(init, num_procs, model_variables = NULL,
                                           warn_partial = FALSE,
                                           ...) {
  
    validate_fit_init(init, model_variables)
    # Convert from data.table to data.frame
    draws_df = init$draws(format = "df")
    if (is.null(model_variables)) {
      model_variables = init$metadata()$stan_variable_sizes
    }
    draws_df$weight = rep(1.0, nrow(draws_df))
    init_draws_df = resample_draws(draws_df, ndraws = num_procs,
                                              weights = draws_df$weight, method = "simple_no_replace")
    init_draws_lst = process_init_draws(init_draws_df,
                                  num_procs = num_procs, model_variables = model_variables, warn_partial=warn_partial)
    
    return(init_draws_lst)
    
}


#' @noRd
.extract_samples <- function(obj=NULL,extract_type=NULL,...) {
  if(!is.null(extract_type)) {
    if(extract_type == 'cutpoints') {
      # Determine number of categories dynamically
      if(inherits(obj@score_data@score_matrix$outcome_disc,'factor')) {
        cut_names <- levels(obj@score_data@score_matrix$outcome_disc)
      } else {
        cut_names <- as.character(unique(obj@score_data@score_matrix$outcome_disc))
      }
      cut_names <- cut_names[!is.na(cut_names) & cut_names != "NA"]
      n_cats <- length(cut_names)
      param <- paste0('steps_votes', n_cats)
    } else {
      param <- switch(extract_type,persons='L_full',
                      obs_discrim='sigma_reg_free',
                      miss_discrim='sigma_abs_free',
                      obs_diff='B_int_free',
                      miss_diff='A_int_free')
    }
    as_tibble(obj@stan_samples$draws(c(param,...)), .name_repair = "minimal")
  } else {
    as_tibble(obj@stan_samples$draws(...), .name_repair = "minimal")
  }


}

#' Helper function for normalization
#' @noRd
.normalize <- function(outcome,true_bounds=NULL) {
  
  if(is.character(outcome)) {
    
    stop("Please do not pass a character vector as a response/outcome.\nThat really doesn't make any sense.")
    
  }
  
  if(is.factor(outcome)) {
    
    message("Converting factor response variable to numeric.")
    
    outcome <- as.numeric(outcome)
    
  }
  
  if(is.na(min(outcome, na.rm=T)) || is.infinite(min(outcome, na.rm=T))) {
    
    stop("The vector does not have enough non-missing data.")
    
  }
  
  if(!is.null(true_bounds)) {
    
    min_out <- true_bounds[1]
    max_out <- true_bounds[2]
  } else {
    
    min_out <- min(outcome, na.rm=T)
    max_out <- max(outcome, na.rm=T)
    
    message(paste0("Normalizing using the observed bounds of ",min_out, " - ",
                   max_out,". If these are incorrect, please pass the bounds to use to the true_bounds parameter."))
    
  }
  
  trans_out <- (outcome - min_out) / (max_out - min_out)
  
  # handle values very close to 0
  
  attr(trans_out, "upper_bound") <- max_out
  attr(trans_out, "lower_bound") <- min_out
  
  return(trans_out)
  
  
}



#' Helper function for preparing person ideal point plot data
#' @noRd
.prepare_legis_data <- function(object,
                                high_limit=NULL,
                                low_limit=NULL,
                                type='ideal_pts',
                                sample_draws=0,
                                include=NULL,
                                add_cov=TRUE,
                                use_chain=NULL,
                                aggregated=NULL) {
  
  if(object@use_method %in% c("laplace","pathfinder")) use_chain <- 1
  
  if(is.null(use_chain))
      use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  
  if(length(unique(object@score_data@score_matrix$time_id))>1 && type!='variance') {
    
    if(length(use_chain)<dim(as_draws_array(object@stan_samples$draws("L_full")))[2]) {
      
      print(paste0("Using only one chain: chain ",use_chain))
      
      person_params <- .get_varying(object,time_id=object@this_data$time, person_id=object@this_data$ll,
                   use_chain=use_chain)
      
    } else {
      
      person_params <- object@time_varying 
    }
    
    
    
    if(add_cov) {
      
      person_params <- .add_person_cov(person_params,object,object@this_data$legis_pred,
                                       object@this_data$ll,
                                       object@this_data$time,
                                       use_chain)
      
    }
    
    if(!is.null(include)) {

      person_params <- person_params[,as.numeric(stringr::str_extract(colnames(person_params),'[0-9]+(?=\\])')) %in% include]
      
    }
    
    if("draws_matrix" %in% class(person_params)) {
      
      person_params <- person_params %>% 
        as_draws_df %>% 
        dplyr::select(-`.chain`,-`.iteration`,-`.draw`)
      
    } else {

      person_params <- as_tibble(person_params, .name_repair = "minimal")

    }
    
    if(sample_draws>0) {
      
      person_params_draws <- slice(person_params,sample(1:n(),
                                                        size = sample_draws)) %>% 
        mutate(.draw=1:n()) %>% 
        gather(key = legis,value=ideal_pts,-.draw) %>% 
        group_by(legis) %>% 
        mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
               param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
               time_point=stringr::str_extract(legis,'\\[[0-9]+'),
               time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
      
    }
    
    if(aggregated) {
      
      person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
        group_by(legis) %>% 
        summarize(low_pt=quantile(ideal_pts,low_limit),high_pt=quantile(ideal_pts,high_limit),
                  median_pt=median(ideal_pts)) %>% 
        mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
               param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
               time_point=stringr::str_extract(legis,'\\[[0-9]+'),
               time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
      
    } else {
      
      person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
        mutate(param_id=stringr::str_extract(legis,'[0-9]+\\]'),
               param_id=as.numeric(stringr::str_extract(param_id,'[0-9]+')),
               time_point=stringr::str_extract(legis,'\\[[0-9]+'),
               time_point=as.numeric(stringr::str_extract(time_point,'[0-9]+')))
      
      
    }
    
      

    # get ids out 
    
    person_ids <- select(object@score_data@score_matrix,
                         !!quo(person_id),
                         !!quo(time_id),
                         !!quo(group_id)) %>% 
      distinct %>% 
      mutate(person_id_num=as.numeric(!!quo(person_id)),
             time_id_num=as.numeric(factor(!!quo(time_id))),
             group_id_num=as.numeric(!!quo(group_id)))
    
    if(object@use_groups) {
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(param_id='group_id_num',
                                  time_point='time_id_num'))
      
      if(sample_draws>0) {
        
        person_params_draws <-  person_params_draws %>% 
          left_join(person_ids,by=c(param_id='group_id_num',
                                    time_point='time_id_num'))
        
        attr(person_params, "draws") <- person_params_draws
        
      }
      
    } else {
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(param_id='person_id_num',
                                  time_point='time_id_num'))
      
      if(sample_draws>0) {
        
        person_params_draws <-  person_params_draws %>% 
          left_join(person_ids,by=c(param_id='person_id_num',
                                    time_point='time_id_num'))
        
        attr(person_params, "draws") <- person_params_draws
        
      }
      
    }
    
    # fill in missing data
    
    
    
  } else {
    # need to match estimated parameters to original IDs
    if(type=='ideal_pts') {

        person_params <- as_draws_array(object@stan_samples$draws('L_full'))
        
        if(add_cov) {
          
          person_params <- .add_person_cov(person_params,object,object@this_data$legis_pred,
                                           object@this_data$ll,
                                           object@this_data$time,
                                           use_chain)
          
        }
        
        if(!is.null(include)) {
          

          if(length(dim(person_params))>2) {
            
            person_params <- person_params[,,as.numeric(stringr::str_extract(attr(person_params,"dimnames")$variable,'[0-9]+(?=\\])')) %in% include]
            
          } else {
            
            person_params <- person_params[,as.numeric(stringr::str_extract(attr(person_params,"dimnames")$variable,'[0-9]+(?=\\])')) %in% include]
            
          }
          
          
        }
        
        person_params <- as_draws_df(person_params)

        # Suppress warning about dropping draws_df class - we intentionally remove metadata columns
        person_params <- suppressWarnings(
          person_params %>%
            dplyr::select(-`.chain`,-`.iteration`,-`.draw`)
        )
      
      
    } else if(type=='variance') {


        # load time-varying person variances
        # Suppress warning about dropping draws_df class - we intentionally remove metadata columns
        person_params <- object@stan_samples$draws('time_var_free') %>% as_draws_df
        person_params <- suppressWarnings(
          person_params %>%
            dplyr::select(-`.chain`,-`.iteration`,-`.draw`)
        )
        
      
    }
    
    person_params <- person_params %>% gather(key = legis,value=ideal_pts) 
    # get ids out 
    
    person_ids <- tibble(long_name=person_params$legis) %>% 
      distinct
    legis_nums <- stringr::str_extract_all(person_ids$long_name,'[0-9]+',simplify=T)
    person_ids <-   mutate(person_ids,id_num=as.numeric(legis_nums))
    
    person_data <- distinct(select(object@score_data@score_matrix,
                                   person_id,group_id))
    
    
    # add in all data in the person_data object
    if(object@use_groups) {
      person_data <- mutate(person_data,id_num=as.numeric(group_id))

    } else {
      person_data <- mutate(person_data,id_num=as.numeric(person_id))
    }
    
    person_ids <- left_join(person_ids,person_data)
    
    if(aggregated) {
      
      person_params <-  person_params %>% 
        group_by(legis) %>% 
        summarize(low_pt=quantile(ideal_pts,low_limit),high_pt=quantile(ideal_pts,high_limit),
                  median_pt=median(ideal_pts)) %>% 
        left_join(person_ids,by=c(legis='long_name'))
      
    } else {
      
      person_params <-  person_params %>% 
        left_join(person_ids,by=c(legis='long_name'))
      
      
    }
    
    

    
  }
  
  person_params 
}

#' Helper function to create arrays
#' 
#' Function takes a data.frame in long mode and converts it to an array. Function can also repeat a 
#' single matrix to fill out an array.
#' 
#' @param input_matrix Either a data.frame in long mode or a single matrix
#' @param arr_dim If `input_matrix` is a single matrix, `arr_dim` determines the length of the resulting array
#' @param row_var Unquoted variable name that identifies the data.frame column corresponding to the rows (1st dimension) of the array (must be unique)
#' @param col_var_name Unquoted variable name that identifies the data.frame column corresponding names of the columns (2nd dimension) of the array
#' @param col_var_value Unquoted variable name that identifies the data.frame column corresponding to the values that populate the cells of the array
#' @param third_dim_var Unquoted variable name that identifis the data.frame column corresponding to the dimension around which to stack the matrices (3rd dimension of array)
#' @noRd
.create_array <- function(input_matrix,arr_dim=2,row_var=NULL,
                          col_var_name=NULL,
                          col_var_value,third_dim_var=NULL) {
  
  if('matrix' %in% class(input_matrix)) {
    
    # if just a matrix, rep it to hit array dims
    rep_matrix <- rep(c(input_matrix),arr_dim)
    out_array <- array(rep_matrix,dim=c(dim(input_matrix),arr_dim))
    
  } else if('data.frame' %in% class(input_matrix)) {
    
    # assuming data is in long form, select and then spread the bugger
    row_var <- enquo(row_var)
    col_var_name <- enquo(col_var_name)
    col_var_value <- enquo(col_var_value)
    third_dim_var <- enquo(third_dim_var)
    to_spread <- ungroup(input_matrix) %>% select(!!row_var,!!col_var_name,!!third_dim_var,!!col_var_value)
    
    # figure out how big this array should be
    arr_dim <- length(unique(pull(to_spread,!!third_dim_var)))
    
    if(!(nrow(distinct(to_spread))==nrow(to_spread))) stop('Each row in the data must be uniquely identified given row_var, col_var and third_dim_var.')
    
    to_array <- lapply(split(to_spread,pull(to_spread,!!third_dim_var)), function(this_data) {
      # spread and stuff into a list
      spread_it <- try(spread(this_data,key=!!col_var_name,value=!!col_var_value))
      if('try-error' %in% class(spread_it)) {
        print('Failed to find unique covariate values for dataset:')
        print(this_data)
        stop()
      }
      spread_it <- spread_it %>% 
        select(-!!row_var,-!!third_dim_var) %>% as.matrix
      row.names(spread_it) <- unique(pull(this_data,!!row_var))
      return(spread_it)
    })
    # convert to a vector before array-ing it
    long_vec <- c(do.call(c,to_array))
    # BOOM
    out_array <- array(long_vec,
                       dim=c(dim(to_array[[1]]),arr_dim),
                       dimnames=list(row.names=row.names(to_array[[1]]),
                                     colnames=colnames(to_array[[1]]),
                                     stack=unique(pull(to_spread,!!third_dim_var))))
  }
  
  return(out_array)
}

#' Simple function to test for what an input is
#' Default_val should be quoted
#' @noRd
.check_quoted <- function(quoted=NULL,default_val) {
  if(is.null(quoted)) {
    quoted <- default_val
  } else if(inherits(quoted,'character')) {
    quoted <- as.name(quoted)
    quoted <- enquo(quoted)
  } else {
    stop(paste0('Please do not enter a non-character value for ',as.character(default_val)[2]))
  }
}

#' Simple function to provide initial values to Stan given current values of restrict_sd
#' @importFrom stats optimize
#' @noRd
.init_stan <- function(chain_id=NULL,
                       this_data=NULL) {
  
  # test out new chatGPT function
  # this works, oddly enough
  
  generate_initial_values <- function(num_bills, num_legis, 
                                      gp_N_fix, time_proc, num_ls, T, 
                                      ar1_down, ar1_up, pos_discrim, 
                                      LX, SRX, SAX, num_bills_grm, n_cats_rat, 
                                      n_cats_grm, num_var, gp_N, restrict_var, 
                                      num_basis,restrict_ind_high,
                                      restrict_ind_low,
                                      const_type,
                                      num_ordbeta,
                                      discrim_reg_upb,
                                      discrim_reg_lb,
                                      discrim_miss_upb,
                                      discrim_miss_lb) {
    
   out_list <-  list(
      sigma_abs_free = rep( (discrim_miss_lb + (discrim_miss_upb + discrim_miss_lb))/2,num_bills),
      #L_full = rep(0L,num_legis),
      L_full = rnorm(num_legis),
      m_sd_free = runif(gp_N, 0.25, .75),
      ls_int = rnorm(num_ls),
      ls_int_abs = rnorm(num_ls),
      L_tp1_var = if (T > 1 && time_proc != 5) array(rep(0L,num_legis * T), dim = c(T, num_legis)) else array(numeric(0), dim = c(T, 0)),
      L_AR1 = if (T > 1 && time_proc == 3) rep(0.1,num_legis) else numeric(0),
      sigma_reg_free = rep((discrim_reg_lb +  (discrim_reg_upb + discrim_reg_lb))/2,num_bills),
      legis_x = rnorm(LX,sd=0.25),
      sigma_reg_x = rnorm(SRX,sd=0.25),
      sigma_abs_x = rnorm(SAX,sd=0.25),
      B_int_free = rnorm(num_bills,sd=0.25),
      A_int_free = rnorm(num_bills,sd=0.25),
      steps_votes3 = sort(rnorm(n_cats_rat[1] - 1,sd=0.25)),
      steps_votes4 = sort(rnorm(n_cats_rat[2] - 1,sd=0.25)),
      steps_votes5 = sort(rnorm(n_cats_rat[3] - 1,sd=0.25)),
      steps_votes6 = sort(rnorm(n_cats_rat[4] - 1,sd=0.25)),
      steps_votes7 = sort(rnorm(n_cats_rat[5] - 1,sd=0.25)),
      steps_votes8 = sort(rnorm(n_cats_rat[6] - 1,sd=0.25)),
      steps_votes9 = sort(rnorm(n_cats_rat[7] - 1,sd=0.25)),
      steps_votes10 = sort(rnorm(n_cats_rat[8] - 1,sd=0.25)),
      steps_votes_grm3 = if (n_cats_grm[1]>1) t(apply(array(rnorm((n_cats_grm[1] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[1] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[1] - 1)),
      steps_votes_grm4 = if (n_cats_grm[2]>1) t(apply(array(rnorm((n_cats_grm[2] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[2] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[2] - 1)),
      steps_votes_grm5 = if (n_cats_grm[3]>1) t(apply(array(rnorm((n_cats_grm[3] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[3] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[3] - 1)),
      steps_votes_grm6 = if (n_cats_grm[4]>1) t(apply(array(rnorm((n_cats_grm[4] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[4] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[4] - 1)),
      steps_votes_grm7 = if (n_cats_grm[5]>1) t(apply(array(rnorm((n_cats_grm[5] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[5] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[5] - 1)),
      steps_votes_grm8 = if (n_cats_grm[6]>1) t(apply(array(rnorm((n_cats_grm[6] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[6] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[6] - 1)),
      steps_votes_grm9 = if (n_cats_grm[7]>1) t(apply(array(rnorm((n_cats_grm[7] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[7] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[7] - 1)),
      steps_votes_grm10 = if (n_cats_grm[8]>1) t(apply(array(rnorm((n_cats_grm[8] - 1) * num_bills,sd=0.25), dim = c(num_bills, n_cats_grm[8] - 1)), 1, sort)) else array(dim = c(num_bills, n_cats_grm[8] - 1)),
      extra_sd = runif(num_var, 0.5, 1),
      time_var_gp_free = runif(gp_N, 0.25, .75),
      time_var_free = if (T > 1 && time_proc != 4 && restrict_var == 1) runif(num_legis - 1, 0.5, 1) else if (T > 1 && time_proc != 4) runif(num_legis, 0.5, 1) else numeric(0),
      a_raw = if (num_basis > 1) array(rep(0L, num_legis * num_basis), dim = c(num_legis, num_basis)) else array(numeric(0), dim = c(num_legis, 0L)),
      ordbeta_cut = if(num_ordbeta>0) matrix(rep(c(-1,1),times=num_ordbeta),ncol=2,nrow=num_ordbeta,byrow=T) else array(numeric(0),dim=c(1,0)),
      phi = if(num_ordbeta>0) rep(0.25, num_ordbeta) else numeric(0)
    )
   
   # add in informative numbers for fixed parameters
   
   if(const_type==1) {
     
     out_list$L_full[restrict_ind_high] <- this_data$fix_high
     out_list$L_full[restrict_ind_low] <- this_data$fix_low
     
   } else if(const_type==2) {
     
     # need to calculate mean of beta distribution
     
     out_list$sigma_reg_free[restrict_ind_high] <- .mean_gbeta(this_data$restrict_N_high,this_data$restrict_sd_high,
                                                               a=discrim_reg_lb,b=discrim_reg_upb + discrim_reg_lb)
     out_list$sigma_reg_free[restrict_ind_low] <- .mean_gbeta(this_data$restrict_sd_low,
                                                              this_data$restrict_N_low,
                                                              a=discrim_reg_lb,b=discrim_reg_upb + discrim_reg_lb)

   }

   return(out_list)
   
  }
  
  # need to figure out gp_N_fix
  
  if(this_data$time_proc!=4) {
    
    gp_N=0
    gp_N_fix=0
    gp_1=0
    gp_oT=this_data$T
    gp_nT=0
    
  } else {
    
    gp_N=this_data$num_legis
    gp_N_fix=this_data$num_legis-1
    gp_1=1
    gp_nT=this_data$T
    gp_oT=0
    
  }
  
  base_params <- generate_initial_values(num_bills=this_data$num_bills,
                                         num_legis=this_data$num_legis,
                                         gp_N_fix=gp_N_fix,
                                         time_proc=this_data$time_proc,
                                         num_ls=this_data$num_ls,
                                         this_data$T,
                                         this_data$ar1_down, 
                                         this_data$ar1_up, 
                                         this_data$pos_discrim, 
                                         this_data$LX, 
                                         this_data$SRX, 
                                         this_data$SAX, 
                                         this_data$num_bills_grm, 
                                         this_data$n_cats_rat, 
                                         this_data$n_cats_grm, 
                                         this_data$num_var, 
                                         gp_N, 
                                         this_data$restrict_var, 
                                         this_data$num_basis,
                                         this_data$restrict_high,
                                         this_data$restrict_low,
                                         this_data$const_type,
                                         this_data$num_ordbeta,
                                         this_data$discrim_reg_upb,
                                         this_data$discrim_reg_lb,
                                         this_data$discrim_miss_upb,
                                         this_data$discrim_miss_lb)
  
  return(base_params)
  
}

#' used to calculate the true ideal points
#' given that a non-centered parameterization is used.
#' @importFrom posterior as_draws_df as_draws_matrix
#' @noRd
.calc_true_pts <- function(obj) {


  over_time <- rstan::extract(obj@stan_samples,'L_tp1')$L_tp1
  drift <- rstan::extract(obj@stan_samples,'L_full')$L_full
  
  save_array <- environment()
  save_array$array_slot <- array(data=NA,dim=dim(over_time))
  if(obj@use_ar) {
    new_pts <- sapply(1:dim(over_time)[2], function(t) {
      sapply(1:dim(over_time)[3], function(i) {
        if(t==1) {
          save_array$array_slot[,t,i] <- drift[,i]
        } else {
          save_array$array_slot[,t,i] <- over_time[,t,i,drop=F] + drift[,i]
        }
        
      })
      
    })
    new_pts <- save_array$array_slot
  } else {
    over_time[,1,] <- drift
    new_pts <- over_time
  }


  return(new_pts)
}

#' Pre-process rollcall objects
#' @noRd
.prepare_rollcall <- function(rc_obj=NULL,item_id=NULL,time_id=NULL) {
  
  # make the outcome

  score_data <- as_tibble(rc_obj$votes) %>% 
    mutate(person_id=row.names(rc_obj$votes))  %>% 
    gather(key = item_id,value = outcome,-person_id)
  
   # merge in other data
  if(is.null(rc_obj$legis.data$legis.names)) {
    rc_obj$legis.data$legis.names <- row.names(rc_obj$legis.data)
  }
  
  score_data <- left_join(score_data,rc_obj$legis.data,by=c(person_id='legis.names'))
  
  score_data <- mutate(score_data,group_id=party)
  
  # extract time from bill labels if it exists
  if(!is.null(rc_obj$vote.data)) {

    score_data <- left_join(score_data,as_tibble(rc_obj$vote.data),by=c(item_id=item_id))
  } else {
    score_data$time_id <- 1
    time_id <- 'time_id'
  }
  
  item_id <- 'item_id'
  
  return(list(score_data=score_data,
              time_id=time_id,
              item_id=item_id))
  
} 

#' Generate item-level midpoints for binary IRT outcomes
#' @noRd
.item_plot_binary <- function(param_name,object,
                       high_limit=NULL,
                       low_limit=NULL,
                       all=FALSE,
                       aggregated=FALSE,
                       use_chain=NULL) {
  
  if(is.null(use_chain))
    use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  
  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as_draws_array(object@stan_samples$draws(paste0('B_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  reg_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_reg_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_diff <- as_draws_array(object@stan_samples$draws(paste0('A_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_abs_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  
  reg_mid <- reg_diff/reg_discrim
  abs_mid <- abs_diff/abs_discrim
  
  if(inherits(object@score_data@score_matrix$outcome_disc,'factor')) {
    cut_names <- levels(object@score_data@score_matrix$outcome_disc)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome_disc))
  }
  if(!all) {
    reg_data <- tibble(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[2],
                           item_name=param_name)
    
    abs_data <- tibble(item_median=quantile(abs_mid,0.5),
                           item_high=quantile(abs_mid,high_limit),
                           item_low=quantile(abs_mid,low_limit),
                           item_type='Inflated\nDiscrimination',
                           Outcome='Missing',
                           item_name=param_name)
    
    out_d <- bind_rows(reg_data,abs_data)
    
    return(out_d)
    
  } else if(all && aggregated) {
    reg_data_mid <- tibble(`Posterior Median`=quantile(reg_mid,0.5),
                               `High Posterior Interval`=quantile(reg_mid,high_limit),
                               `Low Posterior Interval`=quantile(reg_mid,low_limit),
                           `Item Type`='Non-Inflated Item Midpoint',
                           `Predicted Outcome`=cut_names[2],
                           `Item Name`=param_name,
                           `Parameter`=paste0('A function of other parameters'))
    
    abs_data_mid <- tibble(`Posterior Median`=quantile(abs_mid,0.5),
                               `High Posterior Interval`=quantile(abs_mid,high_limit),
                               `Low Posterior Interval`=quantile(abs_mid,low_limit),
                           `Item Type`='Inflated Item Midpoint',
                           `Item Name`=param_name,
                           `Predicted Outcome`='Missing',
                           `Parameter`=paste0('A function of other parameters'))
    
    reg_data_discrim <- tibble(`Posterior Median`=quantile(reg_discrim,0.5),
                                   `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                   `Item Name`=param_name,
                               `Item Type`='Non-Inflated Discrimination',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=paste0('sigma_reg_free[',param_name,']'))
    
    abs_data_discrim <- tibble(`Posterior Median`=quantile(abs_discrim,0.5),
                                   `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                   `Item Name`=param_name,
                               `Item Type`='Inflated Discrimination',
                               `Predicted Outcome`='Missing',
                               `Parameter`=paste0('sigma_abs_free[',param_name,']'))
    
    reg_data_diff <- tibble(`Posterior Median`=quantile(reg_diff,0.5),
                                `High Posterior Interval`=quantile(reg_diff,high_limit),
                                `Low Posterior Interval`=quantile(reg_diff,low_limit),
                                `Item Name`=param_name,
                                   `Item Type`='Non-Inflated Difficulty',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('B_int_free[',param_name,']'))
    
    abs_data_diff <- tibble(`Posterior Median`=quantile(abs_diff,0.5),
                                `High Posterior Interval`=quantile(abs_diff,high_limit),
                                `Low Posterior Interval`=quantile(abs_diff,low_limit),
                                `Item Name`=param_name,
                                   `Item Type`='Inflated Difficulty',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('A_int_free[',param_name,']'))
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  } else if(all && !aggregated) {
    reg_data_mid <- tibble(Posterior_Sample=as.numeric(reg_mid),
                               `Item Name`=param_name,
                               `Item Type`='Non-Inflated Item Midpoint',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`='A function of other parameters') %>% 
      mutate(Iteration=1:n())
    
    abs_data_mid <- tibble(`Posterior_Sample`=as.numeric(abs_mid),
                               `Item Name`=param_name,
                               `Item Type`='Inflated Item Midpoint',
                               `Predicted Outcome`='Missing',
                               `Parameter`='A function of other parameters') %>% 
      mutate(Iteration=1:n())
    
    reg_data_discrim <- tibble(`Posterior_Sample`=as.numeric(reg_discrim),
                                   `Item Name`=param_name,
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('sigma_reg_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data_discrim <- tibble(`Posterior_Sample`=as.numeric(abs_discrim),
                                   `Item Name`=param_name,
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('sigma_abs_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    reg_data_diff <- tibble(`Posterior_Sample`=as.numeric(reg_diff),
                                `Item Name`=param_name,
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=paste0('B_int_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data_diff <- tibble(`Posterior_Sample`=as.numeric(abs_discrim),
                                `Item Name`=param_name,
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=paste0('A_int_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  }

}

#' Generate item-level midpoints for ordinal-rating scale IRT outcomes
#' @noRd
.item_plot_ord_rs <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                              all=FALSE,
                              aggregated=FALSE,
                              use_chain=NULL) {

  if(is.null(use_chain))
    use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  
  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as_draws_array(object@stan_samples$draws(paste0('B_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  reg_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_reg_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_diff <- as_draws_array(object@stan_samples$draws(paste0('A_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_abs_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()

  # Determine the number of categories to use correct parameter name
  if(inherits(object@score_data@score_matrix$outcome_disc,'factor')) {
    cut_names <- levels(object@score_data@score_matrix$outcome_disc)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome_disc))
  }
  # Remove NA from cut_names if present
  cut_names <- cut_names[!is.na(cut_names) & cut_names != "NA"]
  n_cats <- length(cut_names)

  # Parameter name is steps_votes{N} where N is number of categories
  steps_param <- paste0('steps_votes', n_cats)
  cuts <- as_draws_array(object@stan_samples$draws(steps_param))[,use_chain,] %>% as_draws_df
  abs_mid <- abs_diff/abs_discrim
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    
    reg_data <- tibble(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[c],
                           item_name=param_name)
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- tibble(item_median=quantile(abs_mid,0.5),
                         item_high=quantile(abs_mid,high_limit),
                         item_low=quantile(abs_mid,low_limit),
                         item_type='Inflated\nDiscrimination',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(abs_data,reg_data)
  
  if(!all) {
    
    return(out_d)
  
} else if(all && aggregated) {
  
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    reg_data <- tibble(`Posterior Median`=quantile(reg_mid,0.5),
                           `High Posterior Interval`=quantile(reg_mid,high_limit),
                           `Low Posterior Interval`=quantile(reg_mid,low_limit),
                           `Item Type`='Non-Inflated Item Midpoint',
                           `Predicted Outcome`=cut_names[c],
                           `Parameter`=param_name)
    
    
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- tibble(`Posterior Median`=quantile(abs_mid,0.5),
                         `High Posterior Interval`=quantile(abs_mid,high_limit),
                         `Low Posterior Interval`=quantile(abs_mid,low_limit),
                         `Item Type`='Inflated Item Midpoint',
                         `Predicted Outcome`='Missing',
                         `Parameter`=param_name)
  
  reg_data_discrim <- tibble(`Posterior Median`=quantile(reg_discrim,0.5),
                                 `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                 `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                 `Item Type`='Non-Inflated Discrimination',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name)
  
  abs_data_discrim <- tibble(`Posterior Median`=quantile(abs_discrim,0.5),
                                 `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                 `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                 `Item Type`='Inflated Discrimination',
                                 `Predicted Outcome`='Missing',
                                 `Parameter`=param_name)
  
  reg_data_diff <- tibble(`Posterior Median`=quantile(reg_diff,0.5),
                              `High Posterior Interval`=quantile(reg_diff,high_limit),
                              `Low Posterior Interval`=quantile(reg_diff,low_limit),
                              `Item Type`='Non-Inflated Difficulty',
                              `Predicted Outcome`=cut_names[2],
                              `Parameter`=param_name)
  
  abs_data_diff <- tibble(`Posterior Median`=quantile(abs_discrim,0.5),
                              `High Posterior Interval`=quantile(abs_discrim,high_limit),
                              `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                              `Item Type`='Inflated Difficulty',
                              `Predicted Outcome`='Missing',
                              `Parameter`=param_name)
  
  out_d <- bind_rows(reg_data,abs_data,reg_data_discrim,
                     abs_data_discrim,
                     reg_data_diff,
                     abs_data_diff)
  
  return(out_d)
} else if(all && !aggregated) {
  
  reg_data_mid <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    reg_data_mid <- tibble(Posterior_Sample=reg_mid,
                               `Item Type`='Non-Inflated Item Midpoint',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    
    
    return(reg_data_mid)
  }) %>% bind_rows

  
  abs_data_mid <- tibble(`Posterior_Sample`=abs_mid,
                             `Item Type`='Inflated Item Midpoint',
                             `Predicted Outcome`='Missing',
                             `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  reg_data_discrim <- tibble(`Posterior_Sample`=reg_discrim,
                                 `Item Type`='Non-Inflated Discrimination',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  abs_data_discrim <- tibble(`Posterior_Sample`=abs_discrim,
                                 `Item Type`='Inflated Discrimination',
                                 `Predicted Outcome`='Missing',
                                 `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  reg_data_diff <- tibble(`Posterior_Sample`=reg_diff,
                              `Item Type`='Non-Inflated Difficulty',
                              `Predicted Outcome`=cut_names[2],
                              `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  abs_data_diff <- tibble(`Posterior_Sample`=abs_discrim,
                              `Item Type`='Inflated Difficulty',
                              `Predicted Outcome`='Missing',
                              `Parameter`=param_name) %>% 
    mutate(Iteration=1:n())
  
  out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                     abs_data_discrim,
                     reg_data_diff,
                     abs_data_diff)
  
  return(out_d)
}
  
}

#' Generate item-level midpoints for ordinal-GRM IRT outcomes
#' @noRd
.item_plot_ord_grm <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                              all=FALSE,
                              aggregated=FALSE,
                              use_chain=NULL) {

  if(is.null(use_chain))
    use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  
  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as_draws_array(object@stan_samples$draws(paste0('B_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  reg_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_reg_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_diff <- as_draws_array(object@stan_samples$draws(paste0('A_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_abs_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()

  # Determine the number of categories for THIS SPECIFIC ITEM from ordered_id
  # This is important when different items have different numbers of categories (issue #27)
  item_rows <- object@score_data@score_matrix$item_id == param_name
  if("ordered_id" %in% names(object@score_data@score_matrix) &&
     any(!is.na(object@score_data@score_matrix$ordered_id[item_rows]))) {
    n_cats <- unique(object@score_data@score_matrix$ordered_id[item_rows])
    n_cats <- n_cats[!is.na(n_cats)][1]
  } else {
    # Fallback to counting unique outcomes for this item
    item_outcomes <- object@score_data@score_matrix$outcome_disc[item_rows]
    item_outcomes <- item_outcomes[!is.na(item_outcomes)]
    n_cats <- length(unique(item_outcomes))
  }

  # Get cut names for labeling
  if(inherits(object@score_data@score_matrix$outcome_disc,'factor')) {
    cut_names <- levels(object@score_data@score_matrix$outcome_disc)
  } else {
    cut_names <- as.character(sort(unique(object@score_data@score_matrix$outcome_disc)))
  }
  # Remove NA from cut_names if present
  cut_names <- cut_names[!is.na(cut_names) & cut_names != "NA"]
  # Limit to n_cats for this item
  cut_names <- cut_names[1:min(n_cats, length(cut_names))]

  total_cat <- n_cats - 1  # number of cutpoints for GRM

  # Parameter name is steps_votes_grm{N} where N is number of categories
  steps_param <- paste0('steps_votes_grm', n_cats, '[', param_num, ',', total_cat, ']')
  cuts <- as_draws_array(object@stan_samples$draws(steps_param))[,use_chain,] %>% as_draws_df
  abs_mid <- abs_diff/abs_discrim
  # need to loop over cuts
  
  reg_data <- lapply(1:ncol(cuts), function(c) {
    reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
    
    
    reg_data <- tibble(item_median=quantile(reg_mid,0.5),
                           item_high=quantile(reg_mid,high_limit),
                           item_low=quantile(reg_mid,low_limit),
                           item_type='Non-Inflated\nDiscrimination',
                           Outcome=cut_names[c],
                           item_name=param_name)
    
    return(reg_data)
  }) %>% bind_rows
  
  abs_data <- tibble(item_median=quantile(abs_mid,0.5),
                         item_high=quantile(abs_mid,high_limit),
                         item_low=quantile(abs_mid,low_limit),
                         item_type='Inflated\nDiscrimination',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(abs_data,reg_data)
  
  if(!all) {
    
    return(out_d)
    
  } else if(all && aggregated) {
    
    # need to loop over cuts
    
    reg_data <- lapply(1:ncol(cuts), function(c) {
      reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
      
      reg_data <- tibble(`Posterior Median`=quantile(reg_mid,0.5),
                             `High Posterior Interval`=quantile(reg_mid,high_limit),
                             `Low Posterior Interval`=quantile(reg_mid,low_limit),
                             `Item Type`='Non-Inflated Item Midpoint',
                             `Predicted Outcome`=cut_names[c],
                             `Parameter`=param_name)
      
      
      
      return(reg_data)
    }) %>% bind_rows
    
    abs_data <- tibble(`Posterior Median`=quantile(abs_mid,0.5),
                           `High Posterior Interval`=quantile(abs_mid,high_limit),
                           `Low Posterior Interval`=quantile(abs_mid,low_limit),
                           `Item Type`='Inflated Item Midpoint',
                           `Predicted Outcome`='Missing',
                           `Parameter`=param_name)
    
    reg_data_discrim <- tibble(`Posterior Median`=quantile(reg_discrim,0.5),
                                   `High Posterior Interval`=quantile(reg_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(reg_discrim,low_limit),
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=param_name)
    
    abs_data_discrim <- tibble(`Posterior Median`=quantile(abs_discrim,0.5),
                                   `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                   `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=param_name)
    
    reg_data_diff <- tibble(`Posterior Median`=quantile(reg_diff,0.5),
                                `High Posterior Interval`=quantile(reg_diff,high_limit),
                                `Low Posterior Interval`=quantile(reg_diff,low_limit),
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=param_name)
    
    abs_data_diff <- tibble(`Posterior Median`=quantile(abs_discrim,0.5),
                                `High Posterior Interval`=quantile(abs_discrim,high_limit),
                                `Low Posterior Interval`=quantile(abs_discrim,low_limit),
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=param_name)
    
    out_d <- bind_rows(reg_data,abs_data,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  } else if(all && !aggregated) {
    
    reg_data_mid <- lapply(1:ncol(cuts), function(c) {
      reg_mid <- (reg_diff+cuts[[c]])/reg_discrim
      
      reg_data_mid <- tibble(Posterior_Sample=reg_mid,
                                 `Item Type`='Non-Inflated Item Midpoint',
                                 `Predicted Outcome`=cut_names[2],
                                 `Parameter`=param_name) %>% 
        mutate(Iteration=1:n())
      
      
      
      return(reg_data_mid)
    }) %>% bind_rows
    
    
    abs_data_mid <- tibble(`Posterior_Sample`=abs_mid,
                               `Item Type`='Inflated Item Midpoint',
                               `Predicted Outcome`='Missing',
                               `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    reg_data_discrim <- tibble(`Posterior_Sample`=reg_discrim,
                                   `Item Type`='Non-Inflated Discrimination',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    abs_data_discrim <- tibble(`Posterior_Sample`=abs_discrim,
                                   `Item Type`='Inflated Discrimination',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    reg_data_diff <- tibble(`Posterior_Sample`=reg_diff,
                                `Item Type`='Non-Inflated Difficulty',
                                `Predicted Outcome`=cut_names[2],
                                `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    abs_data_diff <- tibble(`Posterior_Sample`=abs_discrim,
                                `Item Type`='Inflated Difficulty',
                                `Predicted Outcome`='Missing',
                                `Parameter`=param_name) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data_mid,abs_data_mid,reg_data_discrim,
                       abs_data_discrim,
                       reg_data_diff,
                       abs_data_diff)
    
    return(out_d)
  }  
}

#' Generate item-level midpoints for binary latent-space outcomes
#' @noRd
.item_plot_ls <- function(param_name,object,
                              high_limit=NULL,
                              low_limit=NULL,
                          aggregated=F,
                          use_chain=NULL) {
  
  if(is.null(use_chain))
    use_chain <- 1:dim(as_draws_array(object@stan_samples$draws("L_full")))[2]
  
  # first need to get num of the parameter
  
  param_num <- which(levels(object@score_data@score_matrix$item_id)==param_name)
  
  # now get all the necessary components
  
  reg_diff <- as_draws_array(object@stan_samples$draws(paste0('B_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  reg_discrim <- as_draws_array(object@stan_samples$draws(paste0('sigma_reg_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  abs_diff <- as_draws_array(object@stan_samples$draws(paste0('A_int_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  item_int <- as_draws_array(object@stan_samples$draws(paste0('sigma_abs_free[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  ideal_int <- as_draws_array(object@stan_samples$draws(paste0('ls_int[',param_num,']')))[,use_chain,] %>% as_draws_matrix()
  
  if(inherits(object@score_data@score_matrix$outcome_disc,'factor')=='factor') {
    cut_names <- levels(object@score_data@score_matrix$outcome_disc)
  } else {
    cut_names <- as.character(unique(object@score_data@score_matrix$outcome_disc))
  }
  
  reg_data <- tibble(item_median=quantile(reg_diff,0.5),
                         item_high=quantile(reg_diff,high_limit),
                         item_low=quantile(reg_diff,low_limit),
                         item_type='Non-Inflated\nItem\nIdeal Point',
                         Outcome=cut_names[2],
                         item_name=param_name)
  
  abs_data <- tibble(item_median=quantile(abs_diff,0.5),
                         item_high=quantile(abs_diff,high_limit),
                         item_low=quantile(abs_diff,low_limit),
                         item_type='Inflated\nItem\nIdeal Point',
                         Outcome='Missing',
                         item_name=param_name)
  
  out_d <- bind_rows(reg_data,abs_data)
  
  
  
  if(!all) {
    
    return(out_d)
    
  } else if(all && aggregated) {
    reg_data <- tibble(item_median=quantile(reg_diff,0.5),
                           item_high=quantile(reg_diff,high_limit),
                           item_low=quantile(reg_diff,low_limit),
                           item_type='Non-Inflated Item Ideal Point',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('B_int_free[',param_num,']'))
    
    abs_data <- tibble(item_median=quantile(abs_diff,0.5),
                           item_high=quantile(abs_diff,high_limit),
                           item_low=quantile(abs_diff,low_limit),
                           item_type='Inflated Item Ideal Point',
                           Outcome='Missing',
                           item_name=param_name,
                           Parameter=paste0('A_int_free[',param_num,']'))
    
    ideal_int <- tibble(item_median=quantile(ideal_int,0.5),
                           item_high=quantile(ideal_int,high_limit),
                           item_low=quantile(ideal_int,low_limit),
                           item_type='Ideal Point Intercept',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('sigma_reg_free[',param_num,']'))
    
    item_int <- tibble(item_median=quantile(item_int,0.5),
                           item_high=quantile(item_int,high_limit),
                           item_low=quantile(item_int,low_limit),
                           item_type='Item Intercept',
                           Outcome=cut_names[2],
                           item_name=param_name,
                           Parameter=paste0('sigma_abs_free[',param_num,']'))
    
    out_d <- bind_rows(reg_data,abs_data,ideal_int,item_int)
    
    return(out_d)
  } else if(all && !aggregated) {
    reg_data <- tibble(Posterior_Sample=reg_diff,
                               `Item Name`=param_name,
                               `Item Type`='Non-Inflated Item Ideal Point',
                               `Predicted Outcome`=cut_names[2],
                               `Parameter`=paste0('B_int_free[',param_num,']')) %>% 
      mutate(Iteration=1:n())
    
    abs_data <- tibble(`Posterior_Sample`=abs_diff,
                               `Item Name`=param_name,
                               `Item Type`='Inflated Item Ideal Point',
                               `Predicted Outcome`='Missing',
                               `Parameter`=paste0('A_int_free[',param_num,']')) %>% 
      mutate(Iteration=1:n())
    
    ideal_int <- tibble(`Posterior_Sample`=ideal_int,
                                   `Item Name`=param_name,
                                   `Item Type`='Ideal Point Intercept',
                                   `Predicted Outcome`=cut_names[2],
                                   `Parameter`=paste0('sigma_reg_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    item_int<- tibble(`Posterior_Sample`=item_int,
                                   `Item Name`=param_name,
                                   `Item Type`='Item Intercept',
                                   `Predicted Outcome`='Missing',
                                   `Parameter`=paste0('sigma_abs_free[',param_name,']')) %>% 
      mutate(Iteration=1:n())
    
    out_d <- bind_rows(reg_data,abs_data,
                       ideal_int,item_int)
    
    return(out_d)
  }
  
}

#' a slightly hacked function to extract parameters as I want to
#' @noRd
.extract_nonp <- function(object, pars, permuted = TRUE, 
                                inc_warmup = FALSE, include = TRUE) {
            # Extract the samples in different forms for different parameters. 
            #
            # Args:
            #   object: the object of "stanfit" class 
            #   pars: the names of parameters (including other quantiles) 
            #   permuted: if TRUE, the returned samples are permuted without
            #     warming up. And all the chains are merged. 
            #   inc_warmup: if TRUE, warmup samples are kept; otherwise, 
            #     discarded. If permuted is TRUE, inc_warmup is ignored. 
            #   include: if FALSE interpret pars as those to exclude
            #
            # Returns:
            #   If permuted is TRUE, return an array (matrix) of samples with each
            #   column being the samples for a parameter. 
            #   If permuted is FALSE, return array with dimensions
            #   (# of iter (with or w.o. warmup), # of chains, # of flat parameters). 
            
            if (object@mode == 1L) {
              cat("Stan model '", object@model_name, "' is of mode 'test_grad';\n",
                  "sampling is not conducted.\n", sep = '')
              return(invisible(NULL)) 
            } else if (object@mode == 2L) {
              cat("Stan model '", object@model_name, "' does not contain samples.\n", sep = '') 
              return(invisible(NULL)) 
            } 
            
            if(!include) pars <- setdiff(object@sim$pars_oi, pars)
            pars <- if (missing(pars)) object@sim$pars_oi else .check_pars_second(object@sim, pars) 
            pars <- .remove_empty_pars(pars, object@sim$dims_oi)
            tidx <- .pars_total_indexes(object@sim$pars_oi, 
                                       object@sim$dims_oi, 
                                       object@sim$fnames_oi, 
                                       pars) 
            
            n_kept <- object@sim$n_save - object@sim$warmup2
            fun1 <- function(par_i) {
              # sss <- sapply(tidx[[par_i]], get_kept_samples2, object@sim)
              # if (is.list(sss))  sss <- do.call(c, sss)
              # the above two lines are slower than the following line of code
              sss <- do.call(cbind, lapply(tidx[[par_i]], .get_kept_samples2, object@sim)) 
              dim(sss) <- c(sum(n_kept), object@sim$dims_oi[[par_i]]) 
              dimnames(sss) <- list(iterations = NULL)
              attr(sss,'num_chains') <- object@sim$chains
              attr(sss,'chain_order') <- rep(1:object@sim$chains,each=dim(sss)[1]/object@sim$chains)

              sss 
            } 
            
            if (permuted) {
              slist <- lapply(pars, fun1) 
              names(slist) <- pars 
              return(slist) 
            } 
            
            tidx <- unlist(tidx, use.names = FALSE) 
            tidxnames <- object@sim$fnames_oi[tidx] 
            sss <- lapply(tidx, .get_kept_samples2, object@sim, inc_warmup) 
            sss2 <- lapply(sss, function(x) do.call(c, x))  # concatenate samples from different chains
            sssf <- unlist(sss2, use.names = FALSE) 
            
            n2 <- object@sim$n_save[1]  ## assuming all the chains have equal iter 
            if (!inc_warmup) n2 <- n2 - object@sim$warmup2[1] 
            dim(sssf) <- c(n2, object@sim$chains, length(tidx)) 
            cids <- sapply(object@stan_args, function(x) x$chain_id)
            dimnames(sssf) <- list(iterations = NULL, chains = paste0("chain:", cids), parameters = tidxnames)
            sssf 
          }


#' we are going to modify this rstan function so that it no longer permutes
#' just delete the last term -- maybe submit PR to rstan
#' @noRd
.get_kept_samples2 <- function(n, sim) {

  # a different implementation of get_kept_samples 
  # It seems this one is faster than get_kept_samples 
  # TODO: to understand why it is faster? 
  lst <- vector("list", sim$chains)
  for (ic in 1:sim$chains) { 
    if (sim$warmup2[ic] > 0) 
      lst[[ic]] <- sim$samples[[ic]][[n]][-(1:sim$warmup2[ic])]
    else 
      lst[[ic]] <- sim$samples[[ic]][[n]]
  } 
  out <- do.call(c, lst)
}

#' another hacked function
#' @noRd
.check_pars_second <- function(sim, pars) {
  #
  # Check if all parameters in pars are parameters for which we saved
  # their samples
  #
  # Args:
  #   sim: The sim slot of class stanfit
  #   pars: a character vector of parameter names
  #
  # Returns:
  #   pars without white spaces, if any, if all are valid
  #   otherwise stop reporting error
  if (missing(pars)) return(sim$pars_oi)
  allpars <- c(sim$pars_oi, sim$fnames_oi)
  .check_pars(allpars, pars)
}

#' another hacked function
#' @noRd
.check_pars <- function(allpars, pars) {
  pars_wo_ws <- gsub('\\s+', '', pars)
  m <- which(match(pars_wo_ws, allpars, nomatch = 0) == 0)
  if (length(m) > 0)
    stop("no parameter ", paste(pars[m], collapse = ', '))
  if (length(pars_wo_ws) == 0)
    stop("no parameter specified (pars is empty)")
  unique(pars_wo_ws)
}

#' yet another hacked function
#' @noRd
.remove_empty_pars <- function(pars, model_dims) {
  #
  # Remove parameters that are actually empty, which
  # could happen when for exmample a user specify the
  # following stan model code:
  #
  # transformed data { int n; n <- 0; }
  # parameters { real y[n]; }
  #
  # Args:
  #   pars: a character vector of parameters names
  #   model_dims: a named list of the parameter dimension
  #
  # Returns:
  #   A character vector of parameter names with empty parameter
  #   being removed.
  #
  ind <- rep(TRUE, length(pars))
  model_pars <- names(model_dims)
  if (is.null(model_pars)) stop("model_dims need be a named list")
  for (i in seq_along(pars)) {
    p <- pars[i]
    m <- match(p, model_pars)
    if (!is.na(m) && prod(model_dims[[p]]) == 0)  ind[i] <- FALSE
  }
  pars[ind]
}

#' yet another hacked function
#' @noRd
.pars_total_indexes <- function(names, dims, fnames, pars) {
# Obtain the total indexes for parameters (pars) in the
# whole sequences of names that is order by 'column major.'
# Args:
#   names: all the parameters names specifying the sequence of parameters
#   dims:  the dimensions for all parameters, the order for all parameters
#          should be the same with that in 'names'
#   fnames: all the parameter names specified by names and dims
#   pars:  the parameters of interest. This function assumes that
#     pars are in names.
# Note: inside each parameter (vector or array), the sequence is in terms of
#   col-major. That means if we have parameter alpha and beta, the dims
#   of which are [2,2] and [2,3] respectively.  The whole parameter sequence
#   are alpha[1,1], alpha[2,1], alpha[1,2], alpha[2,2], beta[1,1], beta[2,1],
#   beta[1,2], beta[2,2], beta[1,3], beta[2,3]. In addition, for the col-majored
#   sequence, an attribute named 'row_major_idx' is attached, which could
#   be used when row major index is favored.

starts <- .calc_starts(dims)
par_total_indexes <- function(par) {
  # for just one parameter
  #
  p <- match(par, fnames)
  # note that here when `par' is a scalar, it would
  # match one of `fnames'
  if (!is.na(p)) {
    names(p) <- par
    attr(p, "row_major_idx") <- p
    return(p)
  }
  p <- match(par, names)
  np <- .num_pars(dims[[p]])
  if (np == 0) return(NULL)
  idx <- starts[p] + seq(0, by = 1, length.out = np)
  names(idx) <- fnames[idx]
  attr(idx, "row_major_idx") <- starts[p] + .idx_col2rowm(dims[[p]]) - 1
  idx
}
idx <- lapply(pars, FUN = par_total_indexes)
nulls <- sapply(idx, is.null)
idx <- idx[!nulls]
names(idx) <- pars[!nulls]
idx
}

#yet another hacked function
#' @noRd
.calc_starts <- function(dims) {
  len <- length(dims)
  s <- sapply(unname(dims), function(d)  .num_pars(d), USE.NAMES = FALSE)
  cumsum(c(1, s))[1:len]
}

#' yet another hacked function
#' @noRd
.num_pars <- function(d) prod(d)

#' yet another hacked function
#' @noRd
.idx_col2rowm <- function(d) {
# Suppose an iteration of samples for an array parameter is ordered by
# col-major. This function generates the indexes that can be used to change
# the sequences to row-major.
# Args:
#   d: the dimension of the parameter
len <- length(d)
if (0 == len) return(1)
if (1 == len) return(1:d)
idx <- aperm(array(1:prod(d), dim = d))
return(as.vector(idx))
}

#' A wrapper around na_if that also works on factors
#' @noRd
.na_if <- function(x,to_na=NULL,discrete_mods=NULL) {
  
    if(is.factor(x)) {
      levels(x)[levels(x)==to_na] <- NA
    } else {
      x <- na_if(x,to_na)
    }
  
    return(x)
  
}

#' Calculate priors for Gaussian processes
#' @noRd
.gp_prior <- function(time_points=NULL) {

  # need to calculate minimum and maximum difference between *any* two points
  diff_time <- diff(time_points)
  min_diff <- min(diff_time)+1
  # divide max_diff by 2 to constrain the prior away from very large values
  max_diff <- abs(time_points[1]-time_points[2])*2
  
  # now run the stan program with the data
  
  fit <- sampling(object = stanmodels[['gp_prior_tune']], iter=1, warmup=0, chains=1,
              seed=5838298, algorithm="Fixed_param")
  params <- extract(fit)
  
  return(list(a=c(params$a),
              b=c(params$b)))
  
}

#' Function to calculate IRFs
#' @noRd
.irf <- function( time=1,shock=1,
                  adj_in=NULL,
                  y_1=0,
                  total_t=10,
                  old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1) {
    x_1 <- shock
  } else {
    x_1 <- 0
  }
  
  print(paste0('Now processing time point ',time))
  
  # Calculate current values of y and x given posterior uncertainty
  
  output <- tibble(y_shock= adj_in*y_1 + x_1,
                       time=time,
                       iter=1:length(adj_in))
  
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    .irf(time=time+1,
         adj_in=adj_in,
         y_1=output$y_shock,
         total_t=total_t,
         old_output=new_output)
  } else {
    return(new_output)  
  }
}

#' Function to create table/matrix of which rows of the data
#' correspond to which model types.
#' @noRd
.count_cats <- function(modelpoints=NULL,
                        billpoints=NULL,
                        Y_int=NULL,
                        discrete=NULL,
                        within_chain=NULL,
                        pad_id=NULL) {

  if(length(Y_int)>1 && any(unique(modelpoints) %in% c(3,4,5,6))) {
      
      # count cats for ordinal models 
      
      get_counts <- group_by(tibble(modelpoints=modelpoints[discrete==1],
                                    billpoints=billpoints[discrete==1],
                                    Y_int),billpoints) %>% 
        group_by(modelpoints,billpoints) %>% 
        summarize(num_cats=length(unique(Y_int))) %>% 
        mutate(num_cats_rat=if_else(modelpoints==3 & num_cats<3,
                                3L,
                                num_cats),
               num_cats_rat=if_else(modelpoints==4 & num_cats<4,
                                3L,
                                num_cats_rat),
               order_cats_rat=as.numeric(factor(num_cats_rat)),
               num_cats_grm=if_else(modelpoints==5 & num_cats<3,
                                    3L,
                                    num_cats),
               num_cats_grm=if_else(modelpoints==6 & num_cats<4,
                                    3L,
                                    num_cats_grm),
               order_cats_grm=as.numeric(factor(num_cats_grm))) 
      
      num_cats_rat <- sort(unique(get_counts$num_cats_rat))
      num_cats_grm <- sort(unique(get_counts$num_cats_grm))
      
      # need to zero out non-present categories
      
      n_cats_rat <- ifelse(3:10 %in% num_cats_rat,3:10,1L)
      n_cats_grm <- ifelse(3:10 %in% num_cats_grm,3:10,1L)
      
      # join the data back together
      
      out_data <- left_join(tibble(modelpoints=modelpoints[discrete==1],
                                   billpoints=billpoints[discrete==1],
                                   Y_int),
                            select(get_counts,
                                   -num_cats_rat,
                                   -num_cats_grm),
                            by=c("modelpoints","billpoints")) 
      out_data$order_cats_grm[is.na(out_data$order_cats_grm)] <- 0L
      out_data$order_cats_rat[is.na(out_data$order_cats_rat)] <- 0L
      
    } else {
      
      out_data <- tibble(order_cats_grm=rep(0L,length(modelpoints[discrete==1])),
                         order_cats_rat=rep(0L,length(modelpoints[discrete==1])))
      n_cats_rat <- rep(1L,length(3:10))
      n_cats_grm <- rep(1L,length(3:10))
    }
  
  return(list(order_cats_rat=out_data$order_cats_rat,
              order_cats_grm=out_data$order_cats_grm,
              n_cats_rat=n_cats_rat,
              n_cats_grm=n_cats_grm))
}

#' Function to figure out how to remove missing values from
#' data before running models.
#' @noRd
.remove_nas <- function( Y_int=NULL,
                        Y_cont=NULL,
                        discrete=NULL,
                        legispoints=NULL,
                        billpoints=NULL,
                        timepoints=NULL,
                        modelpoints=NULL,
                        ordered_id=NULL,
                        idealdata=NULL,
                        time_ind=NULL,
                        time_proc=NULL,
                        const_type=NULL,
                        legis_sd=NULL,
                        restrict_sd_high=NULL,
                        restrict_sd_low=NULL,
                        restrict_N_high=NULL,
                        restrict_N_low=NULL,
                        restrict_high=NULL,
                        restrict_low=NULL,
                        ar_prior=NULL,
                        diff_reg_sd=NULL,
                        diff_miss_sd=NULL,
                        discrim_reg_scale=NULL,
                        discrim_reg_shape=NULL,
                        discrim_miss_scale=NULL,
                        discrim_miss_shape=NULL,
                        fix_high=NULL,
                        fix_low=NULL) {
  

  # need to determine which missing values should not be considered
  # only remove missing values if non-inflated model is used
  
  # figure out if there are any missing values
  
  some_missing_cont <- any(modelpoints %in% c(2,4,6,8,10,12,14,16)) && any(as.numeric(idealdata@miss_val[2]) %in% Y_cont)
  some_missing_disc <- any(modelpoints %in% c(2,4,6,8,10,12,14,16)) && any(idealdata@miss_val[1] %in% Y_int)

  if(length(Y_cont)>1 && !is.na(idealdata@miss_val[2])) {

      Y_cont <- ifelse(modelpoints %in% c(10,12,16),
                       Y_cont,
                       .na_if(Y_cont,as.numeric(idealdata@miss_val[2])))
      
  }

  if(length(Y_int)>1 && !is.na(idealdata@miss_val[1])) {

      Y_int <- if_else(modelpoints %in% c(0,2,
                                                      4,
                                                      6,
                                                      8,
                                                      14),
                      Y_int,
                      .na_if(Y_int,idealdata@miss_val[1]))
    
    # need to downward adjust Y_int
    # convert from factor back to numeric as we have dealt with missing data
    # drop unused levels
    # need to get back to zero index
    if(!any(c(7,8) %in% modelpoints)) {
      
      Y_int <- as.numeric(Y_int)
      
    } else {
      
      # need to handle Poisson, which is tricky
      
      Y_int_old <- Y_int
      
      # need to do a custom conversion for mixed outcomes
      
      if(all(modelpoints %in% c(7,8))) {
        
        # easiest version, all Poisson, simply convert
        # suppressWarnings: "Missing" converts to NA intentionally
        Y_int <- suppressWarnings(as.numeric(as.character(Y_int)))
        max_Y_int <- if(any(!is.na(Y_int))) max(Y_int, na.rm=TRUE) else 0
        Y_int <- ifelse(Y_int_old=="Joint Posterior", max_Y_int + 2,
                        Y_int)
        Y_int <- ifelse(Y_int_old=="Missing", max_Y_int + 1,
                        Y_int)
        
        rm(Y_int_old)
        
      } else {
        
        # mixed outcome, more tricky
        # suppressWarnings: "Missing" converts to NA intentionally
        Y_int_poisson <- suppressWarnings(as.numeric(as.character(Y_int)))
        Y_int_disc <- as.numeric(Y_int)
        
        # check for which has bigger max

        max_poisson <- if(any(!is.na(Y_int_poisson))) max(Y_int_poisson, na.rm=TRUE) else 0
        max_disc <- if(any(!is.na(Y_int_disc))) max(Y_int_disc, na.rm=TRUE) else 0
        
        if(max_poisson>max_disc) {
          
          Y_int_poisson <- ifelse(Y_int_old=="Joint Posterior", max_poisson + 2,
                                 Y_int_poisson)
          Y_int_poisson <- ifelse(Y_int_old=="Missing" , max_poisson + 1,
                                  Y_int_poisson)
          
          Y_int_disc <- ifelse(Y_int_old=="Joint Posterior", max_poisson + 2,
                               Y_int_disc)
          Y_int_disc <- ifelse(Y_int_old=="Missing", max_poisson + 1,
                               Y_int_disc)
          
        } else {
          
          # revert poisson to discrete for missing values coding
          
          if(all(c("Joint Posterior","Missing") %in% unique(Y_int_old))) {
            
            Y_int_poisson <- ifelse(Y_int_old=="Joint Posterior", max_disc,
                                Y_int_poisson)
            Y_int_poisson<- ifelse(Y_int_old=="Missing", max_disc - 1,
                                   Y_int_poisson)
            
          } else {
            
            Y_int_poisson <- ifelse(Y_int_old %in% c("Joint Posterior","Missing"),
                                    max_disc, Y_int_poisson)
            
          }
          
        }
        
        Y_int <- ifelse(modelpoints %in% c(7,8),
                        Y_int_poisson,
                        Y_int_disc)
        
        rm(Y_int_old,Y_int_poisson, Y_int_disc)
        
      }
      
      
    }

      # need to convert binary outcomes to start at 0
        
      # if missing data present, only adjust bottom two numbers
      
      if(any(c(1,2,13,14) %in% unique(modelpoints))) {

        Y_int_subset <- Y_int[modelpoints %in% c(1,2,13,14)]
        min_val <- if(any(!is.na(Y_int_subset))) min(Y_int_subset, na.rm=TRUE) else 0
        Y_int[modelpoints %in% c(1,2,13,14) & (Y_int %in% sort(unique(Y_int_subset))[1:2])] <- Y_int[modelpoints %in% c(1,2,13,14) & (Y_int %in% sort(unique(Y_int_subset))[1:2])] - min_val

      }
      
      if(any(c(3,4,5,6) %in% unique(modelpoints))) {

        # convert ordinal outcomes to start at 1
        # missing data number a bit trickier to avoid

        Y_int_ord_subset <- Y_int[modelpoints %in% c(3,4,5,6)]
        min_ord <- if(any(!is.na(Y_int_ord_subset))) min(Y_int_ord_subset, na.rm=TRUE) else 0
        max_ord <-  Y_int + ordered_id

        conditions <- !is.na(Y_int) & modelpoints %in% c(3,4,5,6) & Y_int <= max_ord

        if(any(conditions)) {
          min_cond <- if(any(!is.na(Y_int[conditions]))) min(Y_int[conditions], na.rm=TRUE) else 1
          Y_int[conditions] <- Y_int[conditions] - (min_cond - 1)
        }


      }

  }
  
  idealdata@Y_int <- Y_int
  idealdata@Y_cont <- Y_cont

  # now need to calculate the true remove NAs
  # if within chain, we by definition won't have any NAs
  remove_nas_cont <- !is.na(Y_cont)
  remove_nas_int <- !is.na(Y_int)

  # this works because the data were sorted in id_make
  if(length(Y_cont)>1 && length(Y_int)>1) {

      remove_nas <- remove_nas_int & remove_nas_cont
    
  } else if(length(Y_cont)>1) {
    remove_nas <- remove_nas_cont
  } else {
    remove_nas <- remove_nas_int
  }
   
    if(length(Y_cont)>1) {
      Y_cont <- Y_cont[remove_nas]
      N_cont <- length(Y_cont)
    } else {
      N_cont <- 0
      Y_cont <- array(dim=c(0)) + 0
    }
    
    if(length(Y_int)>1) {
      Y_int <- Y_int[remove_nas]
      N_int <- length(Y_int)
    } else {
      N_int <- 0
      Y_int <- array(dim=c(0)) + 0L
    }
    
    N <- pmax(N_int, N_cont)
    
    legispoints <- legispoints[remove_nas]
    billpoints <- billpoints[remove_nas]
    timepoints <- timepoints[remove_nas]
    modelpoints <- modelpoints[remove_nas]
    ordered_id <- ordered_id[remove_nas]
    discrete <- discrete[remove_nas]
    
    # no padding necessary
    
    if(any(unique(modelpoints) %in% c(1,13)) && length(table(Y_int[modelpoints %in% c(1,13)]))>3) {
      stop('Too many values in score matrix for a binary model. Choose a different model_type.')
    } else if(any(unique(modelpoints) %in% c(2,14)) && length(table(Y_int[modelpoints %in% c(2,14)]))>4) {
      stop("Too many values in score matrix for a binary model. Choose a different model_type.")
    }
    
    # use zero values for map_rect stuff
    
    all_int_array <- array(dim=c(0,0)) + 0L
    all_cont_array <- array(dim=c(0,0)) + 0L
    
    Y_cont_map <- 0
    N_cont_map <- 0
    Y_int_map <- 0
    N_int_map <- 0
    N_cont_map <- 0
    N_map <- 0
    
    # create covariates
    
    # switch this up so that if it's just personcov0 it'll be a 
    # zero-length matrix
    
    if(idealdata@person_cov[1]=="personcov0") {
      
      legis_pred <- matrix(data=NA_real_,
                           nrow=0,ncol=0)
      
    } else {
      
      legis_pred <- as.matrix(select(idealdata@score_matrix,
                                     idealdata@person_cov))[remove_nas,,drop=F]
      
    }
    
    if(idealdata@item_cov[1]=="itemcov0") {
      
      srx_pred <- matrix(data=NA_real_,
                           nrow=0,ncol=0)
      
    } else {
      
      srx_pred <- as.matrix(select(idealdata@score_matrix,
                                   idealdata@item_cov))[remove_nas,,drop=F]
      
    }
    
    if(idealdata@item_cov_miss[1]=="itemcovmiss0") {
      
      sax_pred <- matrix(data=NA_real_,
                         nrow=0,ncol=0)
      
    } else {
      
      sax_pred <- as.matrix(select(idealdata@score_matrix,
                                   idealdata@item_cov_miss))[remove_nas,,drop=F]
      
    }
    

    
    LX <- ncol(legis_pred)
    SRX <- ncol(srx_pred)
    SAX <- ncol(sax_pred)
    
    max_Y_int <- if(length(Y_int) > 0 && any(!is.na(Y_int))) max(Y_int, na.rm=TRUE) else -Inf
    if(!is.infinite(max_Y_int) && some_missing_disc) {

      if(N_cont>0) {

        # Top level is always joint posterior

        y_int_miss <- max_Y_int - 1

      } else {

        y_int_miss <- max_Y_int

      }



    } else {

      y_int_miss <- 0

    }

    max_Y_cont <- if(length(Y_cont) > 0 && any(!is.na(Y_cont))) max(Y_cont, na.rm=TRUE) else -Inf
    if(!is.infinite(max_Y_cont) && some_missing_cont) {

      if(N_int>0) {

        y_cont_miss <- max_Y_cont - 1

      } else {

        y_cont_miss <- max_Y_cont

      }


    } else {

      y_cont_miss <- 0

    }
 

  max_t <- if(length(timepoints) > 0 && any(!is.na(timepoints))) max(timepoints, na.rm=TRUE) else 1L
  num_bills <- if(length(billpoints) > 0 && any(!is.na(billpoints))) max(billpoints, na.rm=TRUE) else 0L
  num_legis <- if(length(legispoints) > 0 && any(!is.na(legispoints))) max(legispoints, na.rm=TRUE) else 0L
  
  if(any(c(5,6) %in% modelpoints)) {
    num_bills_grm <- num_bills
  } else {
    num_bills_grm <- 0L
  }
  
  if(any(c(13,14) %in% modelpoints)) {
    num_ls <- num_legis
  } else {
    num_ls <- 0L
  }
  
  
  
  
  # now need to determine number of categories

  # need to calculate number of categories for ordinal models
  
  if(N_int>0) {
    
    order_cats_rat <- ordered_id
    order_cats_grm <- ordered_id
    
  } else {
    
    order_cats_rat <- array(dim=c(0)) + 0L
    order_cats_grm <- array(dim=c(0)) + 0L
    
  }

    
    
    if(any(modelpoints %in% c(3,4))) {
      n_cats_rat <- unique(order_cats_rat)
    } else {
      n_cats_rat <- 0
    } 
    
    if(any(modelpoints %in% c(5,6))) {
      n_cats_grm <- unique(order_cats_grm)
    } else {
      n_cats_grm <- 0
    }
  
  n_cats_rat <- sapply(3:10,function(s) {
    if(s %in% n_cats_rat) {
      s
    } else {
      1
    }
  })
  
  n_cats_grm <- sapply(3:10,function(s) {
    if(s %in% n_cats_grm) {
      s
    } else {
      1
    }
  })
  

    
    if(length(time_ind)==1) {
      tibble_time <- tibble(time_ind=rep(time_ind,nrow(idealdata@score_matrix)))
    }
  
  
    return(list(Y_int=Y_int,
                Y_cont=Y_cont,
                legispoints=legispoints,
                billpoints=billpoints,
                timepoints=timepoints,
                modelpoints=modelpoints,
                remove_nas=remove_nas,
                N=N,
                y_cont_miss=y_cont_miss,
                y_int_miss=y_int_miss,
                discrete=discrete,
                max_t=max_t,
                num_bills=num_bills,
                num_legis=num_legis,
                num_ls=num_ls,
                num_bills_grm=num_bills_grm,
                N_cont=N_cont,
                N_int=N_int,
                order_cats_rat=order_cats_rat,
                order_cats_grm=order_cats_grm,
                n_cats_rat=n_cats_rat,
                n_cats_grm=n_cats_grm,
                legis_pred=legis_pred,
                srx_pred=srx_pred,
                sax_pred=sax_pred,
                LX=LX,
                SRX=SRX,
                SAX=SAX,
                idealdata=idealdata))
  
  
}

# Need functions to calculate predicted outcomes

#' Bernoulli
#' @noRd
.cov_bern <- function(lin_val=NULL,...) {
  
  mean(plogis(lin_val)-0.5)
  
}

#' Ordinal outcomes
#' @noRd
.cov_ord <- function(lin_val=NULL,
                      covp=NULL,
                      K=null,
                      ...) {
  if(K==1) {
    1 - mean(plogis(lin_val - covp[,1]))
  } else if(K>1 && K<K) {
    mean(plogis(lin_val - covp[,1]) - plogis(lin_val - covp[,2]))
  } else {
    plogis(lin_val - covp[,1])
  }
  
  
}

#' Poisson
#' @noRd
.cov_pois <- function(lin_val,...) {
  mean(exp(lin_val))
}

#' Normal
#' @noRd
.cov_norm <- function(lin_val,...) {
  mean(lin_val)
}

#' Log-Normal
#' @noRd
.cov_lnorm <- function(lin_val,...) {
  mean(exp(lin_val))
}

#' Latent-Space
#' @noRd
.cov_latsp <- function(lin_val,...) {
  mean(plogis(lin_val)-0.5)
}

#' How to find cutpoints for id_plot_cov function
#' @noRd
.get_cuts_cov <- function(k=NULL,
                          m=NULL,
                          i=NULL,
                          sigma_all=NULL,
                          K=NULL,
                          obj=NULL,
                          these_items=NULL) {
  
  
  if(m %in% c(3,4)) {
    
    # easy peesy, just get the right intercept for k
    
    if(k==1) {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k,"]"))[[1]])
    } else if(k>1 && k<K) {
      cutp <- cbind(as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k-1,"]"))[[1]]),
                    as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k,"]"))[[1]]))
    } else {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",k-1,"]"))[[1]])
    }
    
    return(cutp)
    
  } else {
    
    # need to determine the right graded response intercept based on sigma_all then return the cutpoint
    item_num <- these_items[i]
    if(k==1) {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("grm_steps_votes",K,"[",item_num,",",k,"]"))[[1]])
    } else if(k>1 && k<K) {
      cutp <- cbind(as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k-1,"]"))[[1]]),
                    as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k,"]"))[[1]]))
    } else {
      cutp <- as.matrix(rstan::extract(obj@stan_samples,paste0("steps_votes",K,"[",item_num,",",k-1,"]"))[[1]])
    }
  }
  
  return(cutp)
}

#' Function to square data for map_rect
#' @noRd
.make_sum_vals <- function(this_data,map_over_id=NULL,use_groups=FALSE,
                           remove_nas=NULL) {
  
  this_data <- this_data %>% 
    filter(remove_nas)
  
  # need to save original order to reconvert if necessary
  
  this_data$orig_order <- 1:nrow(this_data)
  
  # need a matrix equal to each ID and row number for where it starts/ends
  
  if(map_over_id=="persons") {
    if(use_groups) {
      
      sum_vals <- this_data %>% 
        mutate(rownum=row_number()) %>% 
        group_by(group_id) %>% 
        filter(row_number() %in% c(1,n())) %>% 
        select(group_id,rownum) %>% 
        mutate(type=c("start","end")[1:n()]) %>% 
        spread(key="type",value = "rownum") %>% 
        ungroup %>% 
        select(group_id,start,end) %>% 
        mutate(group_id=as.numeric(group_id),
               end=coalesce(end,start))
      
      
    } else {
        
        sum_vals <- this_data %>% 
          mutate(rownum=row_number()) %>% 
          group_by(person_id) %>% 
          filter(row_number() %in% c(1,n())) %>% 
          select(person_id,rownum) %>% 
          mutate(type=c("start","end")[1:n()]) %>% 
          spread(key="type",value = "rownum") %>% 
          ungroup %>% 
          select(person_id,start,end) %>% 
          mutate(person_id=as.numeric(person_id),
                 end=coalesce(end,start))
      
    }
  } else {
    
    sum_vals <- this_data %>% 
      mutate(rownum=row_number()) %>% 
      group_by(item_id) %>% 
      filter(row_number() %in% c(1,n())) %>% 
      select(item_id,rownum) %>% 
      mutate(type=c("start","end")[1:n()]) %>% 
      spread(key="type",value = "rownum") %>% 
      ungroup %>% 
      select(item_id,start,end) %>% 
      mutate(item_id=as.numeric(item_id),
             end=coalesce(end,start))
    
  }
  
  return(list(sum_vals=sum_vals,this_data=this_data))
  
}


#' Need new function to re-create time-varying ideal points given reduce sum
#' @importFrom tidyr unite
#' @importFrom tidybayes spread_draws gather_draws
#' @noRd
.get_varying <- function(obj,
                         time_id=NULL,
                         person_id=NULL,
                         use_chain=NULL) {
  
  if(obj@use_method %in% c("pathfinder","laplace")) use_chain <- 1
  
  num_chains <- dim(as_draws_array(obj@stan_samples$draws("L_full")))[2]
  
  if(is.null(use_chain)) {
    
    use_chain <- 1:num_chains
    
  }
  
  if(obj@use_groups) {
    obj@score_data@score_matrix$person_id <- obj@score_data@score_matrix$group_id
  }
  
  if(obj@map_over_id=="items") {
    
    # needs to be in the same format, varying in T then person
      
      all_time <- as_draws_array(obj@stan_samples$draws("L_tp1"))[,use_chain,] %>% as_draws_matrix()
    
  } else {
    
    if(obj@time_proc!=5) 
      L_tp1_var <- as_draws_array(obj@stan_samples$draws("L_tp1_var"))[,use_chain,] %>% as_draws_matrix()
    
    rebuilt <- TRUE
    
    
    if(obj@time_proc==2 && length(unique(obj@score_data@score_matrix$time_id))<obj@time_center_cutoff) {
      
        
        L_full <- as_draws_array(obj@stan_samples$draws("L_full"))[,use_chain,] %>% as_draws_matrix()
        
        if(obj@restrict_var) {
          
          time_var_free <- as_draws_array(obj@stan_samples$draws("time_var_full"))[,use_chain,] %>% as_draws_matrix()
          
        } else {
          
          time_var_free <- as_draws_array(obj@stan_samples$draws("time_var_free"))[,use_chain,] %>% as_draws_matrix()
          
        }
      
      #make a grid, time varying fastest
      
      time_grid <- expand.grid(1:length(unique(obj@score_data@score_matrix$time_id)),
                               unique(as.numeric(obj@score_data@score_matrix$person_id)))
      
      time_func <- function(t=NULL,
                            points=NULL,
                            prior_est=NULL,
                            time_var_free=NULL,
                            initial=NULL,
                            L_full=NULL,
                            p=NULL,
                            L_tp1_var=NULL) {
        
        if(obj@restrict_var) {
          
          time_fix_sd <- obj@time_fix_sd
          p_time <- p - 1
          
        } else {
          
          time_fix_sd <- time_var_free[,p]
          p_time <- p
          
        }
        
        if(p>1) {
          
          if(t==2) {
            
            prior_est <- initial + time_var_free[,p_time]*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
            
            prior_est <- cbind(initial,prior_est)
            
          } else {
            
            this_t <- prior_est[,t-1]  + time_var_free[,p_time]*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
            prior_est <- cbind(prior_est,this_t)
            
            
          }
          
          if(t<max(points)) { 
            
            time_func(t=t+1,
                      points=points,
                      prior_est=prior_est,
                      time_var_free=time_var_free,
                      p=p,
                      L_full=L_full,
                      L_tp1_var=L_tp1_var)
          } else {
            # break recursion
            
            out_d <- as_tibble(prior_est, .name_repair = "minimal") 
            names(out_d) <- as.character(1:length(unique(obj@score_data@score_matrix$time_id)))
            
            out_d <- mutate(out_d,person_id=p,
                            iter=1:n())
            
            return(out_d)
          }
          
        } else {
          
          if(t==2) {
            
            prior_est <- initial + time_fix_sd*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
            
            prior_est <- cbind(initial,prior_est)
            
          } else {
            
            this_t <- prior_est[,t-1]  + time_fix_sd*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
            prior_est <- cbind(prior_est,this_t)
            
            
          }
          
          if(t<max(points)) { 
            
            time_func(t=t+1,
                      points=points,
                      prior_est=prior_est,
                      time_var_free=time_var_free,
                      p=p,
                      L_full=L_full,
                      L_tp1_var=L_tp1_var)
          } else {
            # break recursion
            
            out_d <- as_tibble(prior_est, .name_repair = "minimal") 
            names(out_d) <- as.character(1:length(unique(obj@score_data@score_matrix$time_id)))
            
            out_d <- mutate(out_d,person_id=p,
                            iter=1:n())
            
            return(out_d)
          }
          
        }
        
        
        # we don't do anything here because we need to return results from the
        # enclosing function call above
        
      }
      
      all_time <- lapply(unique(as.numeric(obj@score_data@score_matrix$person_id)), 
                         function (p) {
        
          initial <- L_tp1_var[,(time_grid$Var1==1 & time_grid$Var2==p)]
        
          time_func(t=2,
                           points=1:length(unique(obj@score_data@score_matrix$time_id)),
                           time_var_free=time_var_free,
                           initial=initial,
                           p=p,
                           L_tp1_var=L_tp1_var)
          
          
        }) %>% bind_rows()
      
      # need to reformat by spreading in correct manner
      # one row per sample
      # make joint time-person IDs
      
      all_time <- gather(all_time,"time_id",value="estimate",
                         -person_id,-iter) %>% 
        mutate(time_id=as.numeric(time_id)) %>% 
        arrange(person_id,time_id) %>% 
        unite(col='key',time_id,person_id) %>% 
        mutate(key2=factor(key,levels=unique(key)),
               key3=as.numeric(key2)) %>% 
        select(-key,-key2) %>% 
        spread(key="key3",value="estimate") %>% 
        select(-iter) %>% 
        as.matrix
      
      time_grid <- expand.grid(1:length(unique(obj@score_data@score_matrix$time_id)),
                               unique(as.numeric(obj@score_data@score_matrix$person_id)))
      
      colnames(all_time) <- paste0("L_tp1[",time_grid$Var1,",",time_grid$Var2,"]")

      
    } else if(obj@time_proc==3  && length(unique(obj@score_data@score_matrix$time_id))<obj@time_center_cutoff) {
        
        L_full <- as_draws_array(obj@stan_samples$draws("L_full"))[,use_chain,] %>% as_draws_matrix()
        
        time_var_free <- as_draws_array(obj@stan_samples$draws("time_var_free"))[,use_chain,] %>% as_draws_matrix()
        
        L_AR1 <- as_draws_array(obj@stan_samples$draws("L_AR1"))[,use_chain,] %>% as_draws_matrix()
        
      #make a grid, time varying fastest
      
      time_grid <- expand.grid(1:length(unique(obj@score_data@score_matrix$time_id)),
                               unique(as.numeric(obj@score_data@score_matrix$person_id)))
      
      # what we use for the recursion
      
      time_func2 <- function(t=NULL,
                            points=NULL,
                            prior_est=NULL,
                            time_var_free=NULL,
                            initial=NULL,
                            L_AR1=NULL,
                            L_full=NULL,
                            p=NULL,
                            L_tp1_var=NULL) {
        
          
        if(obj@restrict_var) {
          
          time_fix_sd <- obj@time_fix_sd
          p_time <- p - 1
          
        } else {
          
          time_fix_sd <- time_var_free[,p]
          p_time <- p
          
        }
        

          if(p>1) {
            
            if(t==2) {
              
              prior_est <- initial + L_AR1[,p]*initial + time_var_free[,p_time]*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
              
              prior_est <- cbind(initial,prior_est)
              
            } else {
              
              this_t <- initial + L_AR1[,p]*prior_est[,t-1]  + time_var_free[,p_time]*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
              prior_est <- cbind(prior_est,this_t)
              
              
            }
            
            if(t<max(points)) { 
              
              time_func2(t=t+1,
                        points=points,
                        prior_est=prior_est,
                        time_var_free=time_var_free,
                        p=p,
                        initial=initial,
                        L_AR1=L_AR1,
                        L_full=L_full,
                        L_tp1_var=L_tp1_var)
            } else {
              # break recursion
              
              out_d <- as_tibble(prior_est, .name_repair = "minimal") 
              names(out_d) <- as.character(1:length(unique(obj@score_data@score_matrix$time_id)))
              
              out_d <- mutate(out_d,person_id=p,
                              iter=1:n())
              
              return(out_d)
            }
            
          } else {
            
            if(t==2) {
              
              prior_est <- initial + L_AR1[,p]*initial + time_fix_sd*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
              
              prior_est <- cbind(initial,prior_est)
              
            } else {
              
              this_t <- initial + L_AR1[,p]*prior_est[,t-1]  + time_fix_sd*L_tp1_var[,(time_grid$Var1==t & time_grid$Var2==p)]
              prior_est <- cbind(prior_est,this_t)
              
              
            }
            
            if(t<max(points)) { 
            
              time_func2(t=t+1,
                        points=points,
                        prior_est=prior_est,
                        time_var_free=time_var_free,
                        p=p,initial=initial,
                        L_AR1=L_AR1,
                        L_full=L_full,
                        L_tp1_var=L_tp1_var)
            } else {
              # break recursion
              
              out_d <- as_tibble(prior_est, .name_repair = "minimal") 
              names(out_d) <- as.character(1:length(unique(obj@score_data@score_matrix$time_id)))
              
              out_d <- mutate(out_d,person_id=p,
                              iter=1:n())
              
              return(out_d)
            }
            
          }
        
        
        # we don't do anything here because we need to return results from the
        # enclosing function call above
        
      }
      
      all_time <- lapply(unique(as.numeric(obj@score_data@score_matrix$person_id)), 
                         function (p) {
                           
                           initial <- L_tp1_var[,(time_grid$Var1==1 & time_grid$Var2==p)]
                           
                           out_d <- time_func2(t=2,
                                     points=1:length(unique(obj@score_data@score_matrix$time_id)),
                                     time_var_free=time_var_free,
                                     initial=initial,
                                     p=p,
                                     L_full=L_full,
                                     L_AR1=L_AR1,
                                     L_tp1_var=L_tp1_var)
                           
                           return(out_d)
                           
                         }) %>% bind_rows()
      
      # need to reformat by spreading in correct manner
      # one row per sample
      # make joint time-person IDs
      
      all_time <- gather(all_time,"time_id",value="estimate",
                         -person_id,-iter) %>% 
                  mutate(time_id=as.numeric(time_id)) %>% 
                  arrange(person_id,time_id) %>% 
                  unite(col='key',time_id,person_id) %>% 
                  mutate(key2=factor(key,levels=unique(key)),
                         key3=as.numeric(key2)) %>% 
                  select(-key,-key2) %>% 
                  spread(key="key3",value="estimate") %>% 
        select(-iter) %>% 
        as.matrix
      
      time_grid <- expand.grid(1:length(unique(obj@score_data@score_matrix$time_id)),
                               unique(as.numeric(obj@score_data@score_matrix$person_id)))
      
      colnames(all_time) <- paste0("L_tp1[",time_grid$Var1,",",time_grid$Var2,"]")

      
    } else if(obj@time_proc==5) { 
      
      L_full <- as_draws_array(obj@stan_samples$draws("L_full"))[,use_chain,] %>% as_draws_matrix()
      
      time_var_free <- as_draws_array(obj@stan_samples$draws("time_var_free"))[,use_chain,] %>% as_draws_matrix()
      
      stan_data <- obj@this_data
      
      a_raw <- as_draws_array(obj@stan_samples$draws("a_raw"))[,use_chain,] %>% tidybayes::spread_draws(a_raw[ll,basis])
      B <- stan_data$B
      time_ind <- stan_data$time_ind
      
      #loop over persons
      
      over_persons <- lapply(unique(stan_data$ll), function(l) {
        
        this_a <- filter(a_raw, ll==l) %>% 
          ungroup %>% select(basis,a_raw,.draw,.iteration,.chain) %>% 
          spread(key="basis",value="a_raw") %>% 
          select(-.chain, -.iteration, -.draw) %>% as.matrix
        
        out_mat <- this_a %*% B
        
        out_mat
        
      })
      
      all_time <- do.call(cbind, over_persons)
      
      rm(over_persons)
      
      # make the object to return
      
      time_grid <- expand.grid(1:length(unique(stan_data$time)),
                               unique(as.numeric(stan_data$ll)))
      
      colnames(all_time) <- paste0("L_tp1[",time_grid$Var1,",",time_grid$Var2,"]")
              
      } else {
      
      # GP or random walk and AR(1) but with centered time series  
      
      rebuilt <- FALSE
      
      all_time <- as_draws_array(obj@stan_samples$draws("L_tp1_var")) %>% as_draws_matrix()
      
      time_grid <- expand.grid(1:length(unique(obj@score_data@score_matrix$time_id)),
                               unique(as.numeric(obj@score_data@score_matrix$person_id)))
      
      # for consistency
      
      colnames(all_time) <- paste0("L_tp1[",time_grid$Var1,",",time_grid$Var2,"]")
      
    } 
    
  } # end of if statement differentiating between mapping over items vs. persons
    
    return(all_time)
}

#' Function to add in time-varying covariates to person time-varying ideal points
#' @noRd
.add_person_cov <- function(all_time, 
                            obj,
                            legis_x,
                            person_id,
                            time_id,
                            use_chain) {
  
  if(!is.null(legis_x) && sum(c(legis_x))!=0) {
    
    # need to do an adjustment by re-calculating ideal point scores and including hierarchical covariates
    print("Adding in hierarchical covariates values to the time-varying person scores.")
    
    if(is.numeric(legis_x) & !is.matrix(legis_x)) {
      
      # convert vector to matrix
      
      legis_x <- matrix(legis_x, ncol=1)
      
    }
    
    b <- as_draws_array(obj@stan_samples$draws("legis_x"))[,use_chain,] %>% as_draws_matrix()
    
    # loop over draws d for memory efficiency
    
    print("Collapsing covariates to person and time IDs.")
    
    legis_x <- apply(legis_x, 2, function(c) {
      
      aggregate(c, by=list(person_id, time_id), mean) %>% 
        arrange(Group.1, Group.2) %>% 
        pull(x)
      
    })
    
    df_id <- tibble(time_id=time_id,
                    person_id=person_id) %>% 
      distinct %>% 
      arrange(person_id, time_id)
    
    # missing covariate values (values not observed in data) are set to 0 
    
    cov_vals <- legis_x %*% t(b) %>% t
    
    if(length(unique(df_id$time_id))>1) {
      
      colnames(cov_vals) <- paste0("L_tp1[",df_id$time_id,",",df_id$person_id,"]")
      
    } else {
      
      colnames(cov_vals) <- paste0("L_full[",df_id$person_id,"]")
      
    }
    
    # for person X time combinations for which we don't have data, set covariate = 0
    
    if(length(unique(df_id$time_id))==1) {
      
      all_time <- as_draws_matrix(all_time)
      
      all_attrs <- attributes(all_time)
      
      all_class <- class(all_time)
      
    }
      
      all_time <- sapply(colnames(all_time), function(cn)  {
        
        if(cn %in% colnames(cov_vals)) {
          
          return(all_time[,cn] + cov_vals[,cn])
          
        } else {
          
          return(all_time[,cn])
          
        }
        
      })
      
      if(length(unique(df_id$time_id))==1) { 
        
        attributes(all_time) <- all_attrs
        class(all_time) <- all_class
        # convert back to draws array
        all_time <- as_draws_array(all_time)
      }
    
    print("Done!")
    
  }
  
  return(all_time)
  
}

.mean_gbeta <- function(alpha, beta, a = -1, b = +1) {
  a + (b - a) * (alpha / (alpha + beta))
}

.reverse_two <- function(x) {
  # only character or factor supported
  if (!is.character(x) && !is.factor(x)) {
    stop("`x` must be a character or factor vector")
  }
  
  # handle factors: just flip the level labels
  if (is.factor(x)) {
    if (length(levels(x)) != 2) {
      stop("factor `x` must have exactly two levels")
    }
    levels(x) <- rev(levels(x))
    return(x)
  } else {
    
    # handle character vectors
    vals <- unique(x)
    if (length(vals) != 2) {
      stop("character `x` must have exactly two unique values")
    }
    # if x == vals[1] → vals[2], else → vals[1]
    return(ifelse(x == vals[1], vals[2], vals[1]))
    
    
  }
  
  
}