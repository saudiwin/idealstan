# test new identification options
# March 19, 2019

require(idealstan)
require(dplyr)
require(ggplot2)

bin_irt_2pl_sim <- id_sim_gen(num_person=20,num_bills=200,ordinal=F,inflate=F,
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)
print(bin_irt_2pl_sim@simul_data$true_person)
bin_irt_2pl_est <- id_estimate(idealdata=bin_irt_2pl_sim,
                               model_type=1,
                               fixtype='vb_partial',
                               person_sd = 3,
                               discrim_miss_sd = 1,
                               discrim_reg_sd = 1,
                               diff_reg_sd = 1,
                               diff_miss_sd = 1,
                               restrict_sd=0.01,
                               nchains=3,ncores=3,
                               id_refresh=100)
