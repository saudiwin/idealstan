# Run a simulation to test performance of map-rect given number of cores

library(idealstan)
library(tidyverse)
library(tictoc)
library(ggplot2)


# test both static and dynamic models

# static first
# parallelize by item
# randomize number of persons/bills + number of cores

set.seed(7299481)

nsims <- 2000

over_static_sims <- lapply(1:nsims, function(i) {
  
  print(paste0("Now on iteration ",i))
  
  num_cores <- floor(runif(1,1,parallel::detectCores()+1))
  num_person <- floor(runif(1,10,200))
  num_bills <- floor(runif(1,10,200))
  
  tic()
  
  bin_irt_2pl_sim <- id_sim_gen(num_person=num_person,num_bills=num_bills,
                                ordinal=F,inflate=F,
                                diff_sd=1,
                                reg_discrim_sd = 1,
                                absence_discrim_sd = 1)
  
  bin_irt_2pl_est <- id_estimate(idealdata=bin_irt_2pl_sim,
                                 model_type=1,
                                 restrict_ind_high = as.character(sort(bin_irt_2pl_sim@simul_data$true_person,
                                                                       decreasing=T,
                                                                       index=T)$ix[1]),
                                 restrict_ind_low = as.character(sort(bin_irt_2pl_sim@simul_data$true_person,
                                                                      decreasing=F, 
                                                                      index=T)$ix[1]),
                                 fixtype='prefix',map_over_id = "items",
                                 nchains=1,ncores=num_cores,niters = 500,warmup=500,
                                 init_pathfinder = T)
  
  end_time <- toc()
  
  return(tibble(ellapsed_time=end_time$toc - end_time$tic,
                num_bills=num_bills,
                num_person=num_person,
                num_cores=num_cores))
  
  
}) %>% bind_rows

saveRDS(over_static_sims, "over_static_sims.rds")

over_static_sims %>% 
  ggplot(aes(y=ellapsed_time,x=num_cores)) +
  stat_summary(fun.data="mean_cl_boot")

