# Generate data for each coded model and see if they are properly identified
# Calculate RMSE compared to true values
# See if HPDs have approximately accurate coverage

require(idealstan)
require(dplyr)
require(bayesplot)

model_types <- c('absence')


all_types <- lapply(model_types, function(m) {
  out_models <- test_idealstan(legis_range=c(10,20),simul_type='absence')
  
  
  
  
})