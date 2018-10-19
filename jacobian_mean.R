N <- 24
J <- 1000

vec1 <- rep(1/N,N)
vec2 <- rep(1/N,N)

r <- vec1 %*% vec2

I <- diag(x=1,nrow=N,ncol=N)

I <- I

# from wikipedia

log(abs((1/N - vec1 %*% I %*% vec2)))

# we need a vector valued function for the jacobian
N <- 100
X <- rnorm(n=N)

# first create vector of X derivatives

X_deriv <- rep(1/N,N)
X_adjust <- sum(log(X_deriv))

# then add in actual value adjustment
# this does vary with X

N_adj <- log(abs(sqrt(N)*sum(-X)))

# total change

N_adj + X_adjust

# test by calculating with NumDeriv

require(numDeriv)

# make our "mean function"

mean_func <- function(x) {
  sum(x)/length(x)
}

# calculate gradient

sum(log(abs(grad(mean_func,X,method='simple'))))



# I think this is something else

stan_code <- "
functions {


/*
  Jacobian correction for correcting the mean
  
  Only needs to be used once to save the value as transformed data

@param N size of parameter vector being constrained
@param N_real size of parameter vector being constrained
@return log absolute determinant of the Jacobian of the transformation

*/
  real jacob_mean(int N, real N_real) {
    vector[N] col_vec;
    real num_div;
    real density;
    
    num_div = 1/N_real;

    col_vec = rep_vector(num_div,N);

    density = sum(log(col_vec));

    return density;

  }
} data {

} parameters {

} model {

}

"

comp_model <- stan_model(model_code=stan_code)
stan_func <- expose_stan_functions(comp_model)

jacob_mean(N,N)

