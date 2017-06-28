# look at how bimodal priors work for discrimination parameters
# from this blog post: http://benediktehinger.de/blog/science/how-to-use-bimodal-priors-for-bayesian-data-analysis-in-stan/
library(rstan)
model <- "
data { 
real sd_1;
real sd_2;
}
parameters {
real mu;
} 
transformed parameters {
}
model {
//mu ~ normal(10,1);
//mu ~ normal(-10,1);
target += log_sum_exp(normal_lpdf(mu|-1,sd_1),normal_lpdf(mu|1,sd_2));
}"
samples <- stan(model_code=model, 
                iter=2000, 
                chains=4, 
                thin=1,
                data=list(sd_1=0.5,
                          sd_2=0.5))

ggmcmc::ggs_density(ggmcmc::ggs(samples))+theme_minimal()

target += log_sum_exp(normal_lpdf(sigma_abs_free|-1,abs_discrim_sd),normal_lpdf(sigma_abs_free|1,abs_discrim_sd));
target += log_sum_exp(normal_lpdf(sigma_reg_free|-1,reg_discrim_sd),normal_lpdf(sigma_reg_free|1,reg_discrim_sd));
