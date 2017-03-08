

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
  vector[num_legis] L_full;
  vector[num_bills] B_yes;
  vector[num_bills-restrict] sigma_free;
  vector<upper=0>[restrict] sigma_restrict;
}

transformed parameters {
vector[num_bills] sigma_full;
sigma_full = append_row(sigma_free,sigma_restrict);
}

model {	
  vector[N] pi1;
  vector[N] pi2;
  sigma_free ~ normal(0,5);
  sigma_restrict ~normal(0,5);
  L_full ~ normal(0,1);
	
  B_yes ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] * L_full[ll[n]] - B_yes[bb[n]];
}
    Y_new ~ bernoulli_logit(pi1);
  
}

