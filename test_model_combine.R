# test new identification options
# March 19, 2019

require(idealstan)
require(dplyr)
require(ggplot2)

# to test this, we'll generate two different models then recombine

bin_irt_2pl_sim <- id_sim_gen(num_person=10,num_bills=100,ordinal=F,inflate=F,
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

 norm_irt_2pl_sim <- id_sim_gen(num_person=10,num_bills=100,ordinal=F,inflate=F,
                              model_type="normal",
                              diff_sd=1,
                              reg_discrim_sd = 1,
                              absence_discrim_sd = 1)

# combine objects

comb_obj <- bin_irt_2pl_sim

comb_obj@simul_data$true_person <- (comb_obj@simul_data$true_person +
  norm_irt_2pl_sim@simul_data$true_person)/2

norm_irt_2pl_sim@score_matrix$item_id <- factor(as.numeric(norm_irt_2pl_sim@score_matrix$item_id) +
  max(as.numeric(bin_irt_2pl_sim@score_matrix$item_id)))

comb_obj@score_matrix <- bind_rows(comb_obj@score_matrix,
                                   norm_irt_2pl_sim@score_matrix)

comb_obj@score_matrix <-mutate(comb_obj@score_matrix,model_id=c(rep(1,nrow(bin_irt_2pl_sim@score_matrix)),
                                                rep(9,nrow(bin_irt_2pl_sim@score_matrix))))


# create combined data object

comb_obj_tostan <- id_make(comb_obj@score_matrix,
                           miss_val = c(NA,NA))

# run a pretend model

est_comb_obj <- id_estimate(comb_obj_tostan)
