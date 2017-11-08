data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int<lower=1,upper=J> jj[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n
  int<lower=1> tt[N]; // t for observation N
  int<lower=0> y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
}
parameters {    
  vector[K] delta;                  // discriminations
  real mean_beta;     //mean citizen response
  matrix[T,J] alpha;               // ability of student j - mean ability
  vector[K] beta;                // difficulty of question k
  vector<lower=0>[4] adj;
  vector<lower=0>[4] ts_sigma;
  vector<lower=0,upper=1>[2] gamma1;
  vector<lower=0,upper=1>[2] gamma2;
  real mean_delta;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
}

model {
  alpha[1,] ~ normal(start_vals,0.1);
   //alpha[1,3] ~ normal(start_vals[1],.01);
   //alpha[1,2] ~ normal(0,1);
   //alpha[1,4] ~ normal(0,1);
  gamma1 ~ normal(0,5);
  gamma2 ~ normal(0,5);
  ts_sigma ~ normal(-0.5,1);
  adj ~ normal(0,1);
  mean_delta ~ normal(0,2);
  mean_beta ~ normal(0,2);
  sigma_beta ~ normal(0,2);
  sigma_delta ~ normal(0,2);
  //pre-coup gammas

  alpha[2:(coup-1),1] ~ normal(alpha[1:(coup-2),1] - gamma1[1]*(alpha[1:(coup-2),1] - (adj[2]/adj[1])*alpha[1:(coup-2),2]),
1);
  alpha[2:(coup-1),2] ~ normal(alpha[1:(coup-2),2] - gamma1[1]*(alpha[1:(coup-2),2] - (adj[1]/adj[2])*alpha[1:(coup-2),1]),
1);
  alpha[2:(coup-1),3] ~ normal(alpha[1:(coup-2),3] - gamma1[2]*(alpha[1:(coup-2),3] - (adj[4]/adj[3])*alpha[1:(coup-2),4]),
      1);
  alpha[2:(coup-1),4] ~ normal(alpha[1:(coup-2),4] - gamma1[2]*(alpha[1:(coup-2),4] - (adj[3]/adj[4])*alpha[1:(coup-2),3]),
        1);

  
  //post-coup gammas
  
  alpha[coup:T,1] ~ normal(alpha[(coup-1):(T-1),1] - gamma2[1]*(alpha[(coup-1):(T-1),1] - (adj[2]/adj[1])*alpha[(coup-1):(T-1),2]),
    1);
  alpha[coup:T,2] ~ normal(alpha[(coup-1):(T-1),2] - gamma2[1]*(alpha[(coup-1):(T-1),2] - (adj[1]/adj[2])*alpha[(coup-1):(T-1),1]),
    1);
  alpha[coup:T,3] ~ normal(alpha[(coup-1):(T-1),3] - gamma2[2]*(alpha[(coup-1):(T-1),3] - (adj[4]/adj[3])*alpha[(coup-1):(T-1),4]),
      1);
  alpha[coup:T,4] ~ normal(alpha[(coup-1):(T-1),4] - gamma2[2]*(alpha[(coup-1):(T-1),4] - (adj[3]/adj[4])*alpha[(coup-1):(T-1),3]),
        1);

  beta ~ normal(0,2);          // informative true prior
  delta ~ normal(0,2);       // informative true prior
  for(n in 1:N)
    y[n] ~ poisson_log(delta[kk[n]]*alpha[tt[n],jj[n]] - beta[kk[n]]);
}
