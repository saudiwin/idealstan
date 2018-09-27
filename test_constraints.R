# need to test how constrains work in stan

require(rstan)


true_val <- 1
restrict_sd <- .01

stan_code <- '
data {
real true_val;
real<lower=0> restrict_sd;
}
parameters {
real<lower=0> est_val;
}
model {
est_val ~ normal(true_val,restrict_sd);
}

'

compiled_code <- stan_model(model_code=stan_code)

test_samp <- sampling(compiled_code,data=list(true_val=true_val))

stan_plot(test_samp)

get_pars <- as.data.frame(test_samp)

mean(get_pars$est_val)

# do the conversion
y <- log(true_val)
convert_val <- dnorm(exp(true_val),restrict_sd)*exp(true_val)
