data {
int<lower=1> N;
int<lower=1> T;
real x[T];

real<lower=0> rho[N];
real<lower=0> alpha;
real<lower=0> sigma;
}

transformed data {
//create one covariance matrix for each legislator
matrix[T, T] cov[N];
matrix[T, T] L_cov[N];
for(n in 1:N) {
cov[n] =   cov_exp_quad(x, alpha, rho[n])
+ diag_matrix(rep_vector(1e-10, T));
L_cov[n] = cholesky_decompose(cov[n]);
}


}

parameters {}
model {}

generated quantities {
matrix[N,T] f;
matrix[N,T] Y;
for(n in 1:N) {
f[n,] = multi_normal_cholesky_rng(rep_vector(0, T), L_cov[n])';
    for(t in 1:T) {
      Y[n,t] = normal_rng(f[n,t],sigma);
    }
    
  }  
}
