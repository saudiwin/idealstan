# Generate data for each coded model and see if they are properly identified
# Calculate RMSE compared to true values
# See if HPDs have approximately accurate coverage

require(idealstan)
require(dplyr)
require(bayesplot)
require(rstan)
require(shinystan)
model_types <- c('absence')

one_model <- simulate_absence(num_legis=20)
true_legis <- one_model@simul_data$true_legis
high_leg <- which(true_legis==max(true_legis))
low_leg <- which(true_legis==min(true_legis))
high_leg_pin <- max(true_legis)
low_leg_pin <- min(true_legis)
test_out <- estimate_ideal(idealdata = one_model,
                           model_type = 4,
                           use_vb = FALSE,
                           ncores = 4,
                           nfix=2,
                           restrict_type='constrain_twoway',
                           restrict_params='legis',
                           restrict_ind_high=c(high_leg,low_leg),
                           pin_vals = c(high_leg_pin,low_leg_pin),
                           fixtype='pinned')
restrict_params <- test_out@vote_data@restrict_count

all_params <- extract_samples(test_out)
all_legis <- apply(all_params$L_full,3,mean)
all_sigma_reg <- apply(all_params$sigma_reg_full,2,mean)
true_sigma_reg <- test_out@vote_data@simul_data$true_reg_discrim %>% as.numeric
hist_rhats(test_out)
plot_sims(test_out)
plot_sims(test_out,type='residual')
# try again, this time identify the sigma absences
true_sigma_abs <- one_model@simul_data$true_abs_discrim
high_abs <- sort(true_sigma_abs,decreasing=TRUE,index.return=TRUE)
low_abs <- sort(true_sigma_abs,index.return=TRUE)
high_abs_pin <- max(true_sigma_abs)
low_abs_pin <- min(true_sigma_abs)

test_out <- estimate_ideal(idealdata = one_model,
                           model_type = 4,
                           use_vb = FALSE,
                           ncores = 4,
                           nfix=2,
                           restrict_type='constrain_twoway',
                           restrict_params='discrim_abs',
                           restrict_ind_high=c(high_abs$ix[1:5],low_abs$ix[1:5]),
                           pin_vals = c(high_abs$x[1:5],low_abs$x[1:5]),
                           fixtype='pinned')
restrict_params <- test_out@vote_data@restrict_count

all_params <- extract_samples(test_out)
all_legis <- apply(all_params$L_full,3,mean)
all_sigma_abs <- apply(all_params$sigma_abs_full,2,mean)
true_sigma_abs <- test_out@vote_data@simul_data$true_abs_discrim[as.numeric(colnames(test_out@vote_data@vote_matrix)),] %>% as.numeric
hist_rhats(test_out)
plot_sims(test_out)
plot_sims(test_out,type='residual')

