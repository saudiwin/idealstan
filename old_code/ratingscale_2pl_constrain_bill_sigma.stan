

data {
  int N;
  int Y[N];
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int restrict;
  
}

transformed data {
  	int m; 
  	m = max(Y);
}

parameters {
  vector[num_legis] L_full;
  vector[num_bills-restrict] sigma_free;
  vector<upper=0>[restrict] sigma_restrict;

  vector[num_bills] B_yes;
  ordered[m-1] steps_votes;
}

transformed parameters {
  
  vector[num_bills] sigma_full;
  sigma_full = append_row(sigma_free,sigma_restrict);
}

model {	
  vector[N] pi1;
  sigma_free ~ normal(0,5);
  sigma_restrict ~normal(0,5);
  L_full ~ normal(0,1);
  
  for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
	
  B_yes ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
    Y[n] ~ ordered_logistic(pi1[n],steps_votes);
  }


}


