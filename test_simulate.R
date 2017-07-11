# Generate data for each coded model and see if they are properly identified
# Calculate RMSE compared to true values
# See if HPDs have approximately accurate coverage

require(idealstan)
require(dplyr)
require(bayesplot)
require(rstan)
require(shinystan)
model_types <- c('absence')

# Now we're going to use the real test function


# true_legis <- one_model@simul_data$true_legis
# high_leg <- which(true_legis==max(true_legis))
# low_leg <- which(true_legis==min(true_legis))
# high_leg_pin <- max(true_legis)
# low_leg_pin <- min(true_legis)

test_out <- id_test(legis_range=c(10,50),

                           model_type = 4,
                           ncores = 4,
                           nfix=2,
                           restrict_type='constrain_twoway',
                           restrict_params='legis',
                           fixtype='pinned')

# restrict_params <- test_out@vote_data@restrict_count

# Let's look at full Bayesian inference

ggplot(test_out$est_models,aes(y=avg,x=iter)) + stat_smooth() + 
  facet_wrap(~ID,scales = 'free_y') + theme_minimal()

ggplot(test_out,aes(y=estimate,x=iter)) + theme_minimal()+
  stat_smooth(aes(colour=model_type)) + facet_wrap(~param,nrow = 3)
# 
# all_params <- extract_samples(test_out)
# all_legis <- apply(all_params$L_full,3,mean)
# true_legis <- true_legis[as.numeric(row.names(test_out@vote_data@vote_matrix)),]
# data_frame(all_legis,true_legis) %>% View
# hist_rhats(test_out)
# plot_sims(test_out)
# plot_sims(test_out,type='residual')
# try again, this time identify the sigma absences
one_model <- id_sim_gen(absence=T,
                             ordinal=T,
                             num_legis =100,
<<<<<<< HEAD
                             num_bills=100,
                              absence_discrim_sd = .25,
                             reg_discrim_sd = .25,
=======
                             num_bills=200,
                              absence_discrim_sd = 2,
                             reg_discrim_sd = 2,
>>>>>>> cba415c778e7fe517b09995ddcf8e1f5d209d19e
                             absence_diff_mean = 0.5,
                             diff_sd = .25)
true_sigma_abs <- one_model@simul_data$true_abs_discrim
high_abs <- sort(true_sigma_abs,decreasing=TRUE,index.return=TRUE)
low_abs <- sort(true_sigma_abs,index.return=TRUE)
true_sigma_reg <- one_model@simul_data$true_reg_discrim
high_reg <- sort(true_sigma_reg,decreasing=TRUE,index.return=TRUE)
low_reg <- sort(true_sigma_reg,index.return=TRUE)
high_abs_pin <- max(true_sigma_abs)
low_abs_pin <- min(true_sigma_abs)
true_legis <- one_model@simul_data$true_legis
high_leg <- sort(true_legis,decreasing = T,index.return=T)
low_leg <- sort(true_legis,index.return=T)
high_leg_pin <- max(true_legis)
low_leg_pin <- min(true_legis)

# restrict_ind_high=c(high_abs$ix[1:5],low_abs$ix[1:5]),
# pin_vals = c(high_abs$x[1:5],low_abs$x[1:5])


 test_out <- id_estimate(idealdata = one_model,
                            model_type = 4,
                            use_vb = FALSE,
                            ncores = 4,
                            nfix=2,
                            restrict_type='constrain_twoway',
                            restrict_params='legis',
                            restrict_ind_high=c(high_leg$ix[1:5]),
                            restrict_ind_low = low_leg$ix[1:5],
                            #pin_vals = c(high_leg$x[1],low_leg$x[1]),
                            fixtype='constrained',
<<<<<<< HEAD
                            discrim_reg_sd =5,
                            discrim_abs_sd = 5,
=======
                            discrim_reg_sd =2,
                            discrim_abs_sd = 2,
>>>>>>> cba415c778e7fe517b09995ddcf8e1f5d209d19e
                            legis_sd=1,
                            diff_abs_sd=5,
                            diff_reg_sd=5,
                            restrict_sd=1)
 all_predict <- posterior_predict(test_out)
 bayesplot::ppc_bars(c(test_out@vote_data@vote_matrix),all_predict)
 
 coverages <- id_sim_coverage(test_out)  
  lapply(coverages,function(x) mean(x$avg))
  #apply(test_out@vote_data@vote_matrix,2,table)
  #apply(test_out@vote_data@vote_matrix,1,table)
  id_plot_rhats(test_out)
  id_plot_sims(test_out)
  
  id_plot_sims(test_out,type='residual')

all_pars <- summary(test_out)
filter(all_pars,par_type=='A_int_full') %>% ggplot(aes(x=posterior_median)) + geom_histogram()
all_params <- rstan::extract(test_out@stan_samples)
all_abs_discrim <- apply(all_params$sigma_abs_full,2,median)
all_reg_discrim <- apply(all_params$sigma_reg_full,2,median)
all_legis <- apply(all_params$L_full,3,median)
compare_legis <- data_frame(all_legis,high_pt=apply(all_params$L_full,3,quantile,.95),true_legis[as.numeric(row.names(test_out@vote_data@vote_matrix))],
                            low_pt=apply(all_params$L_full,3,quantile,.05))
compare_reg_discrim <- data_frame(all_reg_discrim,high_pt=apply(all_params$sigma_reg_full,2,quantile,.95),true_sigma_reg[as.numeric(colnames(test_out@vote_data@vote_matrix))],
                            low_pt=apply(all_params$sigma_reg_full,2,quantile,.05))
compare_abs_discrim <- data_frame(all_abs_discrim,high_pt=apply(all_params$sigma_abs_full,2,quantile,.95),true_sigma_abs[as.numeric(colnames(test_out@vote_data@vote_matrix))],
                                  low_pt=apply(all_params$sigma_abs_full,2,quantile,.05))
