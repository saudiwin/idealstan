

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
	int Y_new[N];
	// Put a logical catch for any binary-coded values that are not 0/1
	if(max(Y)>1) {
	  for(n in 1:N)
	    Y_new[n] = Y[n] - min(Y);
	} else {
	  Y_new=Y;
	}
}

parameters {
  vector[num_legis-(restrict*2)] L_free;
  vector<lower=0>[restrict] L_restrict_high;
  vector<upper=0>[restrict] L_restrict_low;
  vector[num_bills] B_yes;
  vector[num_bills] sigma_full;
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
  L_restrict_high ~ normal(0,1);
  L_restrict_low ~ normal(0,1);
	
  B_yes ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
  }

    Y_new ~ bernoulli_logit(pi1);

}

