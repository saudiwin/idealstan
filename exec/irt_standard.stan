
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
  matrix[num_legis,LX] legis_pred[T];
  matrix[num_bills,SRX] srx_pred;
  matrix[num_bills,SAX] sax_pred;
  vector[num_fix_high] pin_vals;
  real reg_discrim_sd;
  real abs_discrim_sd;
  real legis_sd;
  real diff_sd;
  real restrict_sd;
}

transformed data {
	int m;                         // # steps
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
	int num_constrain_sa;
	int num_constrain_sr;
	int Y_new[N];
	if(model_type==4||model_type==6) {
	  //count down one if model is inflated
	  m = max(Y) - 1;
	} else if(model_type==1||model_type==2) {
	  //binary models
	  m = 2;
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
  //determine how many and which parameters to constrain
  #include "create_constrained.stan"

}

parameters {
  vector[num_bills-num_constrain_sa] sigma_abs_free;
  vector[num_legis-num_constrain_l] L_free[T];
  vector[num_bills-num_constrain_sr] sigma_reg_free;
  vector<upper=0>[num_fix_low] restrict_low[T];
  vector<lower=0>[num_fix_high] restrict_high[T];
  vector[num_fix_high] pinned_pars[T];
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[LX] legis_x_cons;
  vector[SRX] sigma_reg_x_cons;
  vector[SAX] sigma_abs_x_cons;
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  ordered[m-1] steps_votes;
  ordered[m-1] steps_votes_grm[num_bills];
  real avg_particip;
}

transformed parameters {
  
  vector[num_legis] L_full[T];
  vector[num_bills] sigma_abs_full;
  vector[num_bills] sigma_reg_full;
  
  vector[num_bills] B_int_full;
  vector[num_bills] A_int_full;
  
  //add in a paramter to the intercepts to prevent additive aliasing

  B_int_full[2:num_bills] = B_int_free;
  A_int_full[2:num_bills] = A_int_free;
  B_int_full[1] = 0.0;
  A_int_full[1] = 0.0;
  //B_int_full = B_int_free;
  //A_int_full = A_int_free;
  //combine constrained and unconstrained parameters
  #include "build_params.stan"
  
  
}

model {	
  //vectors to hold model calculations
  vector[N] pi1;
  vector[N] pi2;
  
    legis_x ~ normal(0,5);
  legis_x_cons ~ normal(0,5);
  sigma_abs_x ~ normal(0,5);
  sigma_reg_x ~ normal(0,5);
  sigma_abs_x_cons ~ normal(0,5);
  sigma_reg_x_cons ~ normal(0,5);

  avg_particip ~ normal(0,5);
  if(model_type>3 && model_type<8) {
     for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
  } else {
    steps_votes ~ normal(0,5);
  }
 
	
  B_int_free ~ normal(0,diff_sd);
  A_int_free ~ normal(0,diff_sd);
  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  
  //priors for legislators and bill parameters
  #include "modeling_statement_v5.stan"
  
  //all model types

  #include "model_types.stan"

/* This file sets up the various types of IRT models that can be run in idealstan */

  
}

