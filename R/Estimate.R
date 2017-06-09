#' This is the constructor fucnction for the idealdata object, which is necessary to estimate an idealstan model.
#' @import rstan
#' @import dplyr
#' @importFrom tidyr gather spread
#' @import bayesplot
#' @import rstantools
#' @import Rcpp
#' @import methods
#' @useDynLib idealstan, .registration = TRUE
#' @export
make_idealdata <- function(vote_data=NULL,simul_data=NULL,
                           legis_cov=NULL,bill_cov_reg=NULL,
                           bill_cov_abs=NULL,
                           legis_data=NULL,bill_data=NULL,
                           abs_vote=NA,yes_vote=3L,no_vote=1L,abst_vote=2L,
                           ordinal=TRUE,time=NULL,
                           exclude_level=NULL,inflate=TRUE,simulation=FALSE) {

  if(ordinal==TRUE & inflate==TRUE) {
    votes <- c(no_vote,abst_vote,yes_vote,abs_vote)
    vote_int <- as.integer(factor(votes,levels=votes))
    names(vote_int) <- votes
    vote_labels <-  c('No','Abstain','Yes','Absent')
    abs_vote <- vote_int[length(vote_int)]
  } else if(ordinal==FALSE & inflate==TRUE) {
    votes <- c(no_vote,yes_vote,abs_vote)
    vote_int <- as.integer(factor(votes,levels=votes))
    names(vote_int) <- votes
    vote_labels <-  c('No','Yes','Absent')
    abs_vote <- vote_int[length(vote_int)]
  } else {
    votes <- c(no_vote,yes_vote)
    vote_int <- as.integer(factor(votes,levels=votes))
    names(vote_int) <- votes
    vote_labels <-  c('No','Yes')
    abs_vote <- NA
  }
  
  if(class(vote_data)=='matrix') {
    
    
    # Register all possible votes as integers, then before running the model we can change them if need be.
    cleaned <- vote_data %>% as_data_frame %>% mutate_all(funs(factor(.,levels=votes))) %>% mutate_all(funs(as.integer(.))) %>% as.matrix
  } else if(class(vote_data)=='rollcall') {
    
  } else {
    stop('Please provide either a matrix or a rollcall object as the vote_data argument.')
  }
  if(!is.null(time)) {
    if(length(time)!=nrow(cleaned)) {
      stop('Time vector must be same length as the number of columns in the vote matrix.')
    }
    time <- as.numeric(factor(time))
    max_t <- max(time)
  } else {
    time <- rep(1,ncol(cleaned))
  }
  
  #before doing this, need to ensure that 1) all legislators in legis_data have votes and 
  #2) the rows were correctly ordered to match vote_data <-> legis_data
  if(!is.null(legis_cov)) {
    if('data.frame' %in% class(legis_cov)) {
      legis_cov <- as.matrix(legis_cov)
      
    } else if('matrix' %in% class(legis_cov)) {
      legis_cov <- array(legis_cov,dim(c(ncol(legis_cov),nrow(legis_cov),max_t)))
    }

  } else {
    legis_cov <- array(c(rep(1,nrow(legis_data)),1,1),dim=c(1,nrow(legis_data),1))
  }
  
  if(!is.null(bill_cov_reg)) {
    if(nrow(bill_cov)!=ncol(cleaned)) {
      stop('Bill covariate data must be same length as the number of columns of vote matrix.')
    }
  } else {
    bill_cov_reg <- matrix(rep(1,ncol(cleaned)),nrow=ncol(cleaned),ncol=1)
  }
  if(!is.null(bill_cov_abs)) {
    if(nrow(bill_cov_abs)!=ncol(cleaned)) {
      stop('Bill covariate data must be same length as the number of columns of vote matrix.')
    }
  } else {
    bill_cov_abs <- matrix(rep(1,ncol(cleaned)),nrow=ncol(cleaned),ncol=1)
  }
  
  legis_data$legis.names <- row.names(vote_data)
  
  row.names(cleaned) <- as.character(1:nrow(cleaned))

  outobj <- new('idealdata',
      vote_matrix=cleaned,
      legis_data=legis_data,
      legis_cov=legis_cov,
      time=time,
      bill_cov_reg=bill_cov_reg,
      bill_cov_abs=bill_cov_abs,
      vote_labels=vote_labels,
      vote_int=vote_int,
      vote_count=length(votes) - length(exclude_level),
      abs_vote=abs_vote)
  
  if(simulation==TRUE) {
    outobj@simul_data <- simul_data
    outobj@simulation <- simulation
  }
  return(outobj)
}

#' Estimate an idealstan model using an idealdata object.
#' @export
estimate_ideal <- function(idealdata=NULL,model_type=2,use_subset=FALSE,sample_it=FALSE,
                           subset_party=NULL,subset_legis=NULL,sample_size=20,
                           nchains=4,niters=2000,use_vb=FALSE,nfix=1,restrict_params='bill',
                           pin_vals=NULL,restrict_rows=NULL,restrict_type='constrain_oneway',
                           fixtype='vb',warmup=floor(niters/2),ncores=NULL,
                           auto_id=FALSE,...) {
  
  
  if(use_subset==TRUE || sample_it==TRUE) {
    idealdata <- subset_ideal(idealdata,use_subset=use_subset,sample_it=sample_it,subset_party=subset_party,
                              subset_legis=subset_legis,sample_size=sample_size)
  }
  
  
    hier_type <- suppressWarnings(.get_hier_type(idealdata))
    idealdata@stanmodel <- stanmodels[['irt_standard']]
   
    #Using an un-identified model with variational inference, find those parameters that would be most useful for
    #constraining/pinning to have an identified model for full Bayesian inference
  
  num_legis <- nrow(idealdata@vote_matrix)
  num_bills <- ncol(idealdata@vote_matrix)
  legispoints <- rep(1:num_legis,times=num_bills)
  billpoints <- rep(1:num_bills,each=num_legis)
  timepoints <- idealdata@time[billpoints]
  avg_particip <- apply(idealdata@vote_matrix,1,function(x) {
    if(is.na(idealdata@abs_vote)) {
      count_abs <- sum(is.na(x))
    } else {
      count_abs <- sum(x==idealdata@abs_vote,na.rm=TRUE)
    }
    particip_rate <- 1 - (count_abs/length(x))
    return(particip_rate)
  }) 
  avg_particip <- scale(avg_particip)[,1]
  Y <- c(idealdata@vote_matrix)

  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
    remove_nas <- !is.na(Y)
    Y <- Y[remove_nas]
    legispoints <- legispoints[remove_nas]
    billpoints <- billpoints[remove_nas]
    timepoints <- timepoints[remove_nas]

  this_data <- list(N=length(Y),
                    T=max(idealdata@time),
                    Y=Y,
                    hier_type=hier_type,
                    num_legis=num_legis,
                    num_bills=num_bills,
                    ll=legispoints,
                    bb=billpoints,
                    time=timepoints,
                    LX=dim(idealdata@legis_cov)[1],
                    SRX=ncol(idealdata@bill_cov_reg),
                    SAX=ncol(idealdata@bill_cov_abs),
                    legis_pred=idealdata@legis_cov,
                    srx_pred=idealdata@bill_cov_reg,
                    sax_pred=idealdata@bill_cov_abs,
                    particip=avg_particip,
                    model_type=model_type)
  
  idealdata <- id_model(object=idealdata,fixtype=fixtype,model_type=model_type,this_data=this_data,
                        nfix=nfix,restrict_params=restrict_params,restrict_rows=restrict_rows,
                        restrict_type=restrict_type,pin_vals=pin_vals,
                        auto_id=auto_id)

  # Now remake the data to reflect the constrained parameters
  
  num_legis <- nrow(idealdata@vote_matrix)
  num_bills <- ncol(idealdata@vote_matrix)
  legispoints <- rep(1:num_legis,times=num_bills)
  billpoints <- rep(1:num_bills,each=num_legis)
  timepoints <- idealdata@time[billpoints]
  avg_particip <- apply(idealdata@vote_matrix,1,function(x) {
    if(is.na(idealdata@abs_vote)) {
      count_abs <- sum(is.na(x))
    } else {
      count_abs <- sum(x==idealdata@abs_vote,na.rm=TRUE)
    }
    particip_rate <- 1 - (count_abs/length(x))
    return(particip_rate)
  }) 
  avg_particip <- scale(avg_particip)[,1]
  Y <- c(idealdata@vote_matrix)
  
  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
  remove_nas <- !is.na(Y)
  Y <- Y[remove_nas]
  legispoints <- legispoints[remove_nas]
  billpoints <- billpoints[remove_nas]
  timepoints <- timepoints[remove_nas]
  
  this_data <- list(N=length(Y),
                    T=max(idealdata@time),
                    Y=Y,
                    hier_type=hier_type,
                    num_legis=num_legis,
                    num_bills=num_bills,
                    ll=legispoints,
                    bb=billpoints,
                    num_fix_high=idealdata@restrict_num_high,
                    num_fix_low=idealdata@restrict_num_low,
                    constrain_par=idealdata@param_fix,
                    constraint_type=idealdata@constraint_type,
                    LX=dim(idealdata@legis_cov)[1],
                    SRX=ncol(idealdata@bill_cov_reg),
                    SAX=ncol(idealdata@bill_cov_abs),
                    legis_pred=idealdata@legis_cov,
                    srx_pred=idealdata@bill_cov_reg,
                    sax_pred=idealdata@bill_cov_abs,
                    time=timepoints,
                    particip=avg_particip,
                    model_type=model_type,
                    pin_vals=idealdata@restrict_vals)
  
  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=ncores,
                         this_data=this_data,use_vb=use_vb,...)
  
  outobj@model_type <- model_type
  
  return(outobj)
  
}
