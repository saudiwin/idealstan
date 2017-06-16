

data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int restrict;
    vector[num_legis] particip;
  
}

transformed data {
	int m;                         // # steps
	int absence[N];
	
	m = max(Y) - 1;
  for(n in 1:N) {
    if(Y[n]>m) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }

}

parameters {
  vector[num_bills] sigma_abs_full;
  vector[num_legis-(restrict*2)] L_free;
  vector[num_bills] sigma_full;
  vector<upper=0>[restrict] L_restrict_low;
  vector<lower=0>[restrict] L_restrict_high;

  vector[num_bills] B_yes;
  vector [num_bills] B_abs;
  ordered[m-1] steps_votes;
  real avg_particip;
}

transformed parameters {
  
  vector[num_legis] L_full;
  L_full = append_row(L_free,append_row(L_restrict_high,L_restrict_low));
}

model {	
  vector[N] pi1;
  vector[N] pi2;
  sigma_full ~ normal(0,5);
  L_free ~ normal(0,1);
  L_restrict_low ~ normal(0,5);
  L_restrict_high ~ normal(0,5);
  sigma_abs_full ~normal(0,5);
  avg_particip ~ normal(0,5);
  
  for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
	
  B_yes ~ normal(0,5);
  B_abs ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_full[bb[n]] * L_full[ll[n]] - B_abs[bb[n]] + avg_particip * particip[ll[n]];
  if(absence[n]==1) {
	  1 ~ bernoulli_logit(pi2[n]);
  } else {
    0 ~ bernoulli_logit(pi2[n]);
    Y[n] ~ ordered_logistic(pi1[n],steps_votes);
  }
  }


  
}

