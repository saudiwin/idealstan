# test map_rect

require(idealstan)
require(dplyr)

bin_irt_2pl_sim <- id_sim_gen(num_person=20,num_bills=200,ordinal=F,inflate=T,
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

# randomly remove some items and some persons to make it harder to square the matrix

bin_irt_2pl_sim@score_matrix <- sample_frac(bin_irt_2pl_sim@score_matrix,.8)

print(bin_irt_2pl_sim@simul_data$true_person)

bin_irt_2pl_est <- id_estimate(idealdata=bin_irt_2pl_sim,
                               model_type=1,
                               within_chain="threads",
                               map_over_id="persons",
                               fixtype='prefix',
                               nchains=3,ncores=3,
                               id_refresh=100)


