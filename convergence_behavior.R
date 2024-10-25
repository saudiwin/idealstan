# study of number of pathfinder paths & convergence behavior

library(idealstan)
library(tidyverse)

set.seed(20241020)


# number of paths 

paths <- 1:100

# number of sims 

sims <- 1:10

results <- lapply(paths, function(p) {
  
  lapply(sims, function(s) {
    
    bin_irt_2pl_sim <- id_sim_gen(num_person=100,num_bills=5,ordinal=F,inflate=F,
                                  diff_sd=1,
                                  reg_discrim_sd = 1,
                                  absence_discrim_sd = 1)
    
    bin_irt_2pl_est <- id_estimate(idealdata=bin_irt_2pl_sim,
                                   model_type=1,init_pathfinder = TRUE,
                                   restrict_ind_high = as.character(sort(bin_irt_2pl_sim@simul_data$true_person,
                                                                         decreasing=T,
                                                                         index=T)$ix[1]),
                                   restrict_ind_low = as.character(sort(bin_irt_2pl_sim@simul_data$true_person,
                                                                        decreasing=F, 
                                                                        index=T)$ix[1]),
                                   fixtype='prefix',
                                   person_sd = 3,
                                   nchains=4,ncores=4,compile_optim = F,num_pathfinder_paths = p)
    
    # get summaries
    
    summaries <- bin_irt_2pl_est@summary
    
    # return max rhat
    
    tibble(draw=s,
           path_num=p,
           max_rhat=max(summaries$rhat,na.rm=T),
           ll_rhat=summaries$rhat[summaries$variable=="lp__"])
    
  }) %>% bind_rows
  
}) %>% bind_rows

results %>% 
  ggplot(aes(y=ll_rhat,x=path_num)) +
  geom_point() +
  stat_smooth(method="lm") +
  ggthemes::theme_clean()
  