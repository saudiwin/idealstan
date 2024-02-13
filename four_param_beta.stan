functions {
  real four_param_beta_lpdf(real y, real alpha, real beta, real gamma, real delta) {
    real x = (y - gamma) / delta;
    return (alpha - 1) * log(x) + (beta - 1) * log1m(x) - log(delta) - lbeta(alpha, beta);
  }
}

data {
  int<lower=0> n;
  real<lower=0> y[n];
  real gamma;
  real<lower=0> delta;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  // Priors (can be adjusted based on domain knowledge)
  alpha ~ normal(2, 1);
  beta ~ normal(2, 1);
  //gamma ~ normal(1, 2);
  //delta ~ normal(2, 2);

  // Likelihood
  for (i in 1:n) {
    y[i] ~ four_param_beta(alpha, beta, gamma, delta);
  }
}
