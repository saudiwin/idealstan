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
                           nfix=1,
                           restrict_type='constrain_twoway',
                           restrict_params='legis')
restrict_params <- test_out@vote_data@restrict_count

compile_old <- stan_model('old_code/ratingscale_absence_inflate_constrain_person.stan')
vote_matrix <- one_model@vote_matrix
vote_matrix <- vote_matrix[c((1:nrow(vote_matrix)[-restrict_params]),restrict_params),]
Y <- c(vote_matrix)
N <- length(Y)
num_legis <- nrow(vote_matrix)
num_bills <- ncol(vote_matrix)
legispoints <- rep(1:num_legis,times=num_bills)
billpoints <- rep(1:num_bills,each=num_legis)
timepoints <- one_model@time[billpoints]
avg_particip <- apply(vote_matrix,1,function(x) {
  if(is.na(one_model@abs_vote)) {
    count_abs <- sum(is.na(x))
  } else {
    count_abs <- sum(x==one_model@abs_vote,na.rm=TRUE)
  }
  particip_rate <- 1 - (count_abs/length(x))
  return(particip_rate)
}) 
this_data <- list(N=N,
                  Y=Y,
                  num_legis=num_legis,
                  num_bills=num_bills,
                  ll=legispoints,
                  bb=billpoints,
                  particip=avg_particip,
                  restrict=1)
old_model <- sampling(compile_old,cores=4,data=this_data)

all_types <- lapply(model_types, function(m) {
  out_models <- test_idealstan(legis_range=c(10,20),simul_type='absence',ncores=4,auto_id=FALSE,use_vb=FALSE)
  
  
  
  
})

plot_sims(all_types[[1]]$regular[[1]])
