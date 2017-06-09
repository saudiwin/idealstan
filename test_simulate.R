# Generate data for each coded model and see if they are properly identified
# Calculate RMSE compared to true values
# See if HPDs have approximately accurate coverage

require(idealstan)
require(dplyr)
require(bayesplot)

model_types <- c('absence')

one_model <- simulate_absence()
test_out <- estimate_ideal(idealdata = one_model,
                           model_type = 4,
                           use_vb = FALSE,
                           ncores = 4,
                           nfix=2,
                           restrict_params='legis')
all_types <- lapply(model_types, function(m) {
  out_models <- test_idealstan(legis_range=c(10,20),simul_type='absence',ncores=4,auto_id=FALSE,use_vb=FALSE)
  
  
  
  
})

plot_sims(all_types[[1]]$regular[[1]])
