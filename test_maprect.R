# test map_rect

require(idealstan)
require(dplyr)

bin_irt_2pl_sim <- id_sim_gen(num_person=4,num_bills=3000,ordinal=F,inflate=T,
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

# randomly remove some items and some persons to make it harder to square the matrix

#bin_irt_2pl_sim@score_matrix <- sample_frac(bin_irt_2pl_sim@score_matrix,.8)

print(bin_irt_2pl_sim@simul_data$true_person)

Sys.setenv(STAN_NUM_THREADS = 1)

# test for time it takes to do map_rect

map_rect_est <- system.time(id_estimate(idealdata=bin_irt_2pl_sim,
                               model_type=1,
                               within_chain="threads",
                               map_over_id="persons",
                               fixtype='prefix',
                               restrict_ind_high = which(bin_irt_2pl_sim@simul_data$true_person==max(bin_irt_2pl_sim@simul_data$true_person)),
                               restrict_ind_low=which(bin_irt_2pl_sim@simul_data$true_person==min(bin_irt_2pl_sim@simul_data$true_person)),
                               nchains=1,ncores=4,niters=1000,
                               id_refresh=100))

solo_est <- system.time(id_estimate(idealdata=bin_irt_2pl_sim,
                                    model_type=1,
                                    within_chain="none",
                                    map_over_id="persons",
                                    fixtype='prefix',
                                    restrict_ind_high = which(bin_irt_2pl_sim@simul_data$true_person==max(bin_irt_2pl_sim@simul_data$true_person)),
                                    restrict_ind_low=which(bin_irt_2pl_sim@simul_data$true_person==min(bin_irt_2pl_sim@simul_data$true_person)),
                                    nchains=1,ncores=1,niters=1000,
                                    id_refresh=100))



