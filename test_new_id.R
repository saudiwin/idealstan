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
                               fixtype='prefix',
                               
                               person_sd = 3,
                               nchains=3,ncores=3,
                               id_refresh=100)

model_code <- 'functions {
  real id_params(vector p, int high, int low, 
                  real fix_high, 
                  real fix_low,
                  real sd_fix,
                  real mean_val,
                  real sd_val) {
    
    int N = num_elements(p);
    real prob_dens; // hold the calculated probability density
    
    prob_dens = 0;
    
    // use different types of indexing depending on placement of 
    // values to fix in the vector
    if(high>low) {
      
      if(low>1) {
        prob_dens += normal_lpdf(p[1:(low-1)]|mean_val,sd_val);
      }
      
      prob_dens += normal_lpdf(p[low]|fix_low,sd_fix);
      
        if(high>(low+1)) {
          prob_dens += normal_lpdf(p[(low+1):(high-1)]|mean_val,
                                    sd_val);
          }
          
      prob_dens += normal_lpdf(p[high]|fix_high,
                                        sd_fix);
                                        
        if(high<N) {
          prob_dens += normal_lpdf(p[(high+1):N]|mean_val,sd_val);
        }
      
      
      
    } else {

      if(high>1) {
        prob_dens += normal_lpdf(p[1:(high-1)]|mean_val,sd_val);
      }
      
      prob_dens += normal_lpdf(p[high]|fix_high,sd_fix);
      
        if(low>(high+1)) {
          prob_dens += normal_lpdf(p[(high+1):(low-1)]|mean_val,
                                    sd_val);
          }
          
      prob_dens += normal_lpdf(p[low]|fix_low,
                                        sd_fix);
        if(low<N) {
          prob_dens += normal_lpdf(p[(low+1):N]|mean_val,sd_val);
        }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
}
data {
}
parameters {
}
model {
}
'


test_mod <- rstan::stan_model(model_code=model_code)
rstan::expose_stan_functions(test_mod)
