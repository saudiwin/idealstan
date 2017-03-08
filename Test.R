# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)
require(ggplot2)
require(bayesplot)

newdata <- readKH(file=url('http://amypond.sscnet.ucla.edu/rollcall/static/S114.ord'))

to_use <- newdata$votes 
to_use <- apply(to_use,2,function(x) {
  y <- recode(x,`1`=3L,`6`=1L,`7`=2L,`9`=4L)
  return(y)
})

all_vals <- table(to_use)

#Save row names (Congresspeople)

rownames(to_use) <- rownames(newdata$votes)

idealdata <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                           abs_vote = '4')

estimated_full <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=TRUE,ncores = 4,
                            use_vb = FALSE)

estimated_vb <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=FALSE,ncores = 4,
                            use_vb = TRUE)



# Now try a binary inflated model by excluding the abstention category (2)

ideal_data_binary <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                                    abs_vote = '4',exclude_level='2')

estimated_binary <- estimate_ideal(idealdata=ideal_data_binary,use_subset = FALSE,ncores = 4,
                                   use_vb = FALSE,nfix=c(2,2),restrict_params ='person',sample_it=FALSE,sample_size=30)

all_out <- rstan::extract(estimated_binary@stan_samples,permuted=FALSE)

if(estimated_binary@use_vb==FALSE) {
  all_out <- as.array(all_out)
  mcmc_violin(all_out,pars='sigma_abs_open[1]') + theme_minimal()
  mcmc_violin(all_out,regex_pars='L_restrict_high') + theme_minimal()
  mcmc_violin(all_out,regex_pars='L_restrict_low') + theme_minimal()
} else {
  mcmc_dens(all_out,pars='sigma_abs_open[1]')
  mcmc_dens(all_out,regex_pars='sigma_abs_restrict')
}


