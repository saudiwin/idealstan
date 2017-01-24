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
                           votes=NULL,abs_vote=NA,exclude_level=NULL) {
  
  if(class(vote_data)=='matrix') {
    
    votes <- c(votes,abs_vote)
    
    # Register all possible votes as integers, then before running the model we can change them if need be.
    cleaned <- vote_data %>% as_data_frame %>% mutate_all(funs(factor(.,levels=votes,exclude=exclude_level))) %>% mutate_all(funs(as.integer(.))) %>% as.matrix
  } else if(class(vote_data)=='rollcall') {
    
  } else {
    stop('Please provide either a matrix or a rollcall object as the vote_data argument.')
  }
  new('idealdata',
      vote_matrix=cleaned,
      legis_data=legis_data,
      vote_labels=votes,
      vote_count=length(votes) - length(exclude_level),
      abs_vote=abs_vote)
}

#' Estimate an idealstan model using an idealdata object.
#' @export
estimate_ideal <- function(idealdata=NULL,use_subset=FALSE,sample_it=FALSE,
                           subset_party=NULL,subset_legis=NULL,sample_size=20,
                           nchains=4,niters=2000,use_vb=FALSE,nfix=10,
                           fixtype='vb',warmup=floor(niters/2),ncores=NULL,modeltype='binary_absence_inflate',...) {
  
  
  if(use_subset==TRUE || sample_it==TRUE) {
    idealdata <- subset_ideal(idealdata,use_subset=use_subset,sample_it=sample_it,subset_party=subset_party,
                              subset_legis=subset_legis,sample_size=sample_size)
  }
  

    to_use <- stanmodels[[modeltype]]
  
  num_legis <- nrow(idealdata@vote_matrix)
  num_bills <- ncol(idealdata@vote_matrix)
  legispoints <- rep(1:num_legis,times=num_bills)
  billpoints <- rep(1:num_bills,each=num_legis)
  avg_particip <- apply(idealdata@vote_matrix,1,function(x) {
    count_abs <- sum(x==idealdata@vote_count,na.rm=TRUE)
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
  if(grepl('binary',modeltype)) {
    remove_nas <- !is.na(Y)
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
  
  idealdata <- id_model(object=idealdata,fixtype=fixtype,modeltype=modeltype,this_data=this_data,
                        nfix=nfix)
  if(idealdata@param_fix=='sigma_abs') {
    to_use <- stanmodels[[paste0(modeltype,'_fix_sigma_abs')]]
  }
  
  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=ncores,to_use=to_use,
                         this_data=this_data,use_vb=use_vb,...)
  
  outobj@model_type <- modeltype
  
  return(outobj)
  
}
