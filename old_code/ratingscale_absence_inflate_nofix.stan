

data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
    vector[num_legis] particip;
  
}

transformed data {
	int m;                         // # steps
	// Need to recode the ordinal outcome to always equal 3 but to allow the two subsets of outcomes
	int absence[N];
	int Y_new[N];
	Y_new = Y;
  for(n in 1:N) {
    if(Y[n]>3) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }
  m=3;
}

parameters {
  vector[num_legis] L_free;
  vector[num_bills] B_yes;
  vector[num_bills] sigma;
  vector [num_bills] B_abs;
  vector [num_bills] sigma_abs_open;
  ordered[m-1] steps_votes;
  real avg_particip;
}

transformed parameters {
vector[num_legis] L_open;
L_open = L_free;
}

model {	
  vector[N] pi1;
  vector[N] pi2;
  sigma ~ normal(0,5);
  L_free ~ normal(0,1);
  sigma_abs_open ~normal(0,5);
  avg_particip ~ normal(0,5);
  
  for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
	
  B_yes ~ normal(0,5);
  B_abs ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma[bb[n]] *  L_open[ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_open[bb[n]] * L_open[ll[n]] - B_abs[bb[n]] + avg_particip * particip[ll[n]];
  if(absence[n]==1) {
	  1 ~ bernoulli_logit(pi2[n]);
  } else {
    0 ~ bernoulli_logit(pi2[n]);
    Y[n] ~ ordered_logistic(pi1[n],steps_votes);
  }
  }
}

