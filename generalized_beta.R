library(betafunctions)
library(cmdstanr)

# screw around to make a generalized beta distribution in Stan

# make a density function first in R and check with betafunctions

dbetagen <- function(x, l, u, alpha, beta) {
  
  term1 <- (alpha-1)*log(x - l) 
  term2 <- (beta  - 1)*log(u - x)
  term3 <- (alpha + beta - 1)*log(u - l)
  
  term1 + term2 - term3 - base::lbeta(alpha,beta)
  
  
}

log_beta_function <- function(x, alpha, beta, l, u) {
  bfunc <- beta(alpha, beta)
  log_bfunc <- log(bfunc) # Logarithm of the beta function
  term1 <- (alpha - 1) * log(x - l) # Logarithm of (x - l)^(alpha - 1)
  term2 <- (beta - 1) * log(u - x)  # Logarithm of (u - x)^(beta - 1)
  term3 <- (alpha + beta - 1) * log(u - l) # Logarithm of (u - l)^(alpha + beta - 1)
  
  # Sum the logarithms instead of multiplying the terms
  result <- term1 + term2 - term3 - log_bfunc
  return(result)
}


dbetagen(x = 0.5, l = 0.25, u = 0.75, alpha = 5, beta = 3)
log(dBeta.4P(x = 0.5, l = 0.25, u = 0.75, alpha = 5, beta = 3))
log_beta_function(x=0.5, alpha=5,beta=3, l=0.25, u=0.75)

# generate data from 4 parameter Beta family, then fit and recover from model

y <- rBeta.4P(500, -1, 1, 0.1,1000)
y2 <- rBeta.4P(500, -1, 1, 1000,0.1)
hist(y2)

this_mod <- "
functions{
  
  //PDF for 4-parameter beta family
  real genbeta_lpdf(real y, real alpha, real beta, real gamma, real delta) {
    real x = (y - gamma) / delta;
    return (alpha - 1) * log(x) + (beta - 1) * log1m(x) - log(delta) - lbeta(alpha, beta);
  }
  
}


}
data {
  int N;
  real y[N];
  real upb;
  real lbd;
  real scale_prior;
  real shape_prior;
}
parameters {

  real scale;
  real shape;

}
model {

  scale ~ exponential(1);
  shape ~ exponential(1);
  
  target += genbeta_lpdf(y|scale, shape, lbd, lbd + ubd);

}
"

