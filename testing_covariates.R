library(idealstan)

# working on covariates


sim_data <- id_sim_gen(num_person = 100,num_items = 100,
  person_cov="binary",person_cov_effect=.5)

sim_fit <- id_estimate(sim_data,nchains=1,niters = 1000,ncores=4,
                restrict_ind_high=31, restrict_ind_low=38,const_type="items")

id_plot_legis(sim_fit,show_true = T)

id_plot_cov(sim_fit)

id_plot_items(sim_fit,show_true=T,item_type = "observed")
