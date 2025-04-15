data {
int<lower=1> N;
int<lower=1> T;
array[T] real x;
vector[N] ideal_pts;
array[N] real<lower=0> rho;
array[N] real<lower=0> alpha;
real<lower=0> sigma;
}

transformed data {
//create one covariance matrix for each legislator
array[N] matrix[T, T] cov;
array[N] matrix[T, T] L_cov;
for(n in 1:N) {
cov[n] =   gp_exp_quad_cov(x, alpha[n], rho[n])
+ diag_matrix(rep_vector(sigma, T));
L_cov[n] = cholesky_decompose(cov[n]);
}


}

parameters {}
model {}

generated quantities {
matrix[N,T] f;
matrix[N,T] Y;
for(n in 1:N) {
f[n,] = multi_normal_cholesky_rng(rep_vector(0, T) + ideal_pts[n], L_cov[n])';
    for(t in 1:T) {
      //Y[n,t] = normal_rng(f[n,t],sigma);
      Y[n,t] = f[n,t];
    }
    
  }  
}
