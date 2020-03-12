# need to check ordered parameters

require(rstan)
require(dplyr)

test_ord <- stan_model("test_stan_ordered.stan")


samp_ord <- sampling(test_ord,data=list(num_bills_grm=10,
                                        n_cats_grm=c(3,6)))

print(samp_ord)
