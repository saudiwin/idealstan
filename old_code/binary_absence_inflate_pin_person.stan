

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
	int absence[N];
	int Y_new[N];
	vector[1] L_restrict_high;
	vector[1] L_restrict_low;
	// Put a logical catch for any binary-coded values that are not 0/1
	if(max(Y)>1) {
	  for(n in 1:N)
	    Y_new[n] = Y[n] - min(Y);
	} else {
	  Y_new=Y;
	}
  for(n in 1:N) {
    if(Y[n]>1) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }
  L_restrict_high[1]=1;
  L_restrict_low[1]=-1;
}

parameters {
  vector[num_legis-(restrict*2)] L_free;
  vector[num_bills] B_yes;
  vector[num_bills] sigma_full;
  vector [num_bills] B_abs;
  vector [num_bills] sigma_abs_open;
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
  L_free ~ normal(0,5);
  sigma_abs_open ~normal(0,5);
  avg_particip ~ normal(0,5);
	
  B_yes ~ normal(0,5);
  B_abs ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_open[bb[n]] * L_full[ll[n]] - B_abs[bb[n]] + avg_particip * particip[ll[n]];
  if(absence[n]==1) {
	  1 ~ bernoulli_logit(pi2[n]);
  } else {
    0 ~ bernoulli_logit(pi2[n]);
    Y_new[n] ~ bernoulli_logit(pi1[n]);
  }
  }


  
}

