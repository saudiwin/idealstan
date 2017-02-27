# Test file to show how package works

# Import latest senate data using pscl
require(pscl)
require(dplyr)
require(idealstan)
require(tidyr)

newdata <- readKH(file=url('http://amypond.sscnet.ucla.edu/rollcall/static/S114.ord'))

to_use <- newdata$votes 
to_use <- apply(to_use,2,function(x) {
  y <- recode(x,`1`=3L,`6`=1L,`7`=2L,`9`=4L)
  return(y)
})

all_vals <- table(to_use)

idealdata <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                           abs_vote = '4')

estimated_full <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=TRUE,ncores = 4,
                            use_vb = FALSE)

estimated_vb <- estimate_ideal(idealdata=idealdata,use_subset = FALSE,sample_it=FALSE,ncores = 4,
                            use_vb = TRUE)



# Now try a binary inflated model by excluding the abstention category (2)

ideal_data_binary <- make_idealdata(vote_data=to_use,legis_data=newdata$legis.data,votes=as.character(names(all_vals[1:3])),
                                    abs_vote = '4',exclude_level='2')

estimated_binary <- estimate_ideal(idealdata=ideal_data_binary,use_subset = FALSE,sample_it=FALSE,ncores = 4,
                                   use_vb = FALSE,nfix=c(1,5),fixparams ='person')

# estimated_binary_vb <- estimate_ideal(idealdata=ideal_data_binary,use_subset = FALSE,sample_it=FALSE,ncores = 4,
#                                    use_vb = TRUE,nfix=1)

lookat_params <- rstan::extract(estimated_binary@stan_samples,permuted=FALSE)
lookat_params <- lookat_params[,1,]
sigmas_est <- lookat_params[,grepl('sigma_abs_open\\[',colnames(lookat_params))]
sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
  summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05))

sigmas <- arrange(sigmas_est,avg)

# lookat_params <- rstan::extract(estimated_binary_vb@stan_samples,permuted=FALSE)
# lookat_params <- lookat_params[,1,]
# sigmas_est <- lookat_params[,grepl('sigma_abs_open\\[',colnames(lookat_params))]
# sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
#   summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05))
# 
# sigmas_vb <- arrange(sigmas_est,avg)