

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
  vector[num_legis] L_free;
  vector[num_bills] B_yes;
  vector[num_bills] sigma;
}

model {	
  vector[N] pi1;
  
  sigma ~ normal(0,5);
  L_free ~ normal(0,1);
  B_yes ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma[bb[n]] *  L_free[ll[n]] - B_yes[bb[n]];
  }

    Y_new ~ bernoulli_logit(pi1);

}

