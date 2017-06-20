
#include "license.stan"

data {
  int N;
  int T;
  int Y[N];
  
  /* Use this to set the type of IRT Model to run
  1= basic IRT 2 Pl (no inflation)
  2= basic IRT 2 PL (with inflation)
  3 = ratingscale IRT (no inflation)
  4= ratingscale IRT (with inflation)
  5 = grm IRT (no inflation)
  6= grm IRT (with inflation)
  7= ZIP IRT
  */
  int model_type;
  
  int hier_type;
  int LX;
  int SRX;
  int SAX;
  // int auto_reg;
  // int ar_lag;
  // int ma_lag;
  // int i_lag;
  // int with_absence;
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int constrain_par;
  int constraint_type;
  int num_fix_high;
  int num_fix_low;
  int ll[N];
  int bb[N];
  int time[N];
  vector[num_legis] particip;
  //matrix[num_legis,LX] legis_pred[T];
  //matrix[num_bills,SRX] srx_pred;
  //matrix[num_bills,SAX] sax_pred;
  //vector[num_fix_high] pin_vals;
}

transformed data {
	int m;                         // # steps
	int absence[N]; // need to create absence indicator
	// int num_constrain_l;
	// int num_constrain_sa;
	// int num_constrain_sr;
	int Y_new[N];
	if(model_type==2||model_type==4||model_type==6) {
	  //count down one if model is inflated
	  m = max(Y) - 1;
	} else {
	  m= max(Y);
	}
	
  for(n in 1:N) {
      if(Y[n]>m) {
        absence[n]=1;
      } else {
        absence[n]=0;
      }
      if(model_type==1||model_type==2) {
        //need to change outcome for binomial models
        if(min(Y)>0) {
          Y_new[n] = Y[n] - min(Y);
        } else {
          Y_new[n] = Y[n];
        }
      }
  }
  
  
    // num_constrain_l=num_fix_high;
    // num_constrain_sr=0;
    // num_constrain_sa=0;

}

parameters {
  vector[num_bills] sigma_abs_free;
  //vector[num_legis-num_constrain_l] L_free[T];
  vector[num_legis-num_fix_high-num_fix_low] L_free;
  vector[num_bills] sigma_reg_free;
  vector<upper=0>[num_fix_low] restrict_low;
  //vector<lower=0>[num_constrain_sr+num_constrain_l+num_constrain_sa] restrict_high[T];
  vector<lower=0>[num_fix_high] restrict_high;
  // vector[LX] legis_x;
  // vector[SRX] sigma_reg_x;
  // vector[SAX] sigma_abs_x;
  // vector[LX] legis_x_cons;
  // vector[SRX] sigma_reg_x_cons;
  // vector[SAX] sigma_abs_x_cons;
  vector[num_bills] B_yes;
  vector[num_bills] B_abs;
  ordered[m-1] steps_votes;
  //ordered[m-1] steps_votes_grm[num_bills];
  real avg_particip;
}

transformed parameters {
  
  //vector[num_legis] L_full[T];
  vector[num_legis] L_full;
  vector[num_bills] sigma_abs_full;
  vector[num_bills] sigma_reg_full;
  
    // for(t in 1:T) {
    //   L_full[t] = append_row(L_free[t],restrict_high[t]);
    // }
  L_full = append_row(L_free,append_row(restrict_high,restrict_low));
  sigma_abs_full = sigma_abs_free;
  sigma_reg_full = sigma_reg_free;
  
}

model {	
  //vectors to hold model calculations
  
  vector[N] pi1;
  vector[N] pi2;
          
    if(hier_type==8 && constraint_type==3 && constrain_par==1) {
        restrict_high ~ normal(0,5);
        restrict_low ~ normal(0,5);
          L_free ~ normal(0,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              restrict_high[t] ~ normal(restrict_high[t-1],1);
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5); 
         }
  

  avg_particip ~ normal(0,5);
  
  for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
	
  B_yes ~ normal(0,5);
  B_abs ~ normal(0,5);
  // for(b in 1:num_bills) {
  // steps_votes_grm[b] ~ normal(0,5);
  // }
  //model

if(model_type==4) {
  //ratingscale inflation
  //time[n],
    for(n in 1:N) {

      pi1[n] = sigma_reg_free[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_free[bb[n]] * L_full[ll[n]] - B_abs[bb[n]] + avg_particip*particip[ll[n]];
      if(absence[n]==1) {
	      1 ~ bernoulli_logit(pi2[n]);
      } else {
        0 ~ bernoulli_logit(pi2[n]);
        Y[n] ~ ordered_logistic(pi1[n],steps_votes);
      }

  }

  
}

}


