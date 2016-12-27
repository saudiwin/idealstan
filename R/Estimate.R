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
make_idealdata <- function(vote_data=NULL,legis_data=NULL,bill_data=NULL,
                           votes=NULL,abs_vote=NA) {
  
  if(class(vote_data)=='matrix') {
    
    votes <- c(votes,abs_vote)
    
    # Register all possible votes as integers, then before running the model we can change them if need be.
    cleaned <- vote_data %>% as_data_frame %>% mutate_all(funs(factor(.,levels=votes))) %>% mutate_all(funs(as.integer(.))) %>% as.matrix
  } else if(class(vote_data)=='rollcall') {
    
  } else {
    stop('Please provide either a matrix or a rollcall object as the vote_data argument.')
  }
  new('idealdata',
      vote_matrix=cleaned,
      legis_data=legis_data,
      vote_labels=votes,
      vote_count=length(votes),
      abs_vote=abs_vote)
}

#' Estimate an idealstan model using an idealdata object.
#' @export
estimate_ideal <- function(idealdata=NULL,use_subset=FALSE,sample_it=FALSE,sample_size=NULL,
                           nchains=4,niters=2000,
                           fixtype='vb',warmup=floor(niters/2),ncores=NULL,modeltype='ratingscale_absence_inflate',...) {
  
  if(modeltype=='ratingscale_absence_inflate') {
    to_use <- stanmodels$ordinal_split_absence
    to_use_vb <- stanmodels$ordinal_split_absence_nofix
  } else if(modeltype=='grm_absence_inflate') {
    to_use <- stanmodels$grm_split_absence
    to_use_vb <- stanmodels$grm_split_absence_nofix
  }
  
  num_legis <- nrow(idealdata@vote_matrix)
  num_bills <- ncol(idealdata@vote_matrix)
  legispoints <- rep(1:num_legis,times=num_bills)
  billpoints <- rep(1:num_bills,each=num_legis)
  avg_particip <- apply(idealdata@vote_matrix,1,function(x) {
    count_abs <- sum(x==idealdata@vote_count)
    particip_rate <- 1 - (count_abs/length(x))
    return(particip_rate)
  }) 
  avg_particip <- scale(avg_particip)[,1]
  Y <- c(idealdata@vote_matrix)
  if(!grepl('absence',modeltype)) {
    remove_nas <- !(Y==votecount)  
    Y <- Y[remove_nas]
    legispoints <- legispoints[remove_nas]
    billpoints <- billpoints[remove_nas]
  }
  
  this_data <- list(N=length(Y),
                    Y=Y,
                    num_legis=num_legis,
                    num_bills=num_bills,
                    ll=legispoints,
                    bb=billpoints,
                    particip=avg_particip)
  
  idealdata <- id_model(object=idealdata,fixtype=fixtype,to_use=to_use_vb,this_data=this_data)
  
  
  
  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=NULL,to_use=to_use,
                         this_data=this_data,...)
  
  outobj@model_type <- modeltype
  
  return(outobj)
  
}
