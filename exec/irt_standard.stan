
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
  int LX;
  int SRX;
  int SAX;
  int use_ar;
  // int ar_lag;
  // int ma_lag;
  // int i_lag;
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int num_fix_high;
  int num_fix_low;
  int ll[N];
  int bb[N];
  int time[N];
  vector[N] exog_data;
  matrix[num_legis,LX] legis_pred[T];
  matrix[num_bills,SRX] srx_pred;
  matrix[num_bills,SAX] sax_pred;
  real diff; // difference between high and low constrained parameters
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real restrict_sd;
  real restrict_low_bar;
  real restrict_high_bar;
  real time_sd;
}

transformed data {
	int m;                         // # steps
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
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
      if(Y[n]>m || Y[n]==(-9998)) {
        absence[n]=1;
      } else {
        absence[n]=0;
      }
      if(model_type==1) {
        //need to change outcome for binomial models
        if(max(Y)==2) {
          Y_new[n] = Y[n] - 1;
        } else {
          Y_new[n] = Y[n];
        }
      } else if(model_type==2) {
         if(max(Y)==3) {
          Y_new[n] = Y[n] - 1;
        } else {
          Y_new[n] = Y[n];
        }
      }
  }
  //determine how many and which parameters to constrain
  #include "create_constrained.stan"

}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis-num_constrain_l] L_free; // first T=1 params to constrain
  vector[num_legis] L_tp1[T]; // all other params can float
  vector[num_legis] L_AR1; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  vector<lower=restrict_high_bar>[num_fix_high] restrict_high;
  vector[num_fix_high] pinned_pars;
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
  ordered[num_fix_low+num_fix_high] restrict_ord[T];
  real exog_param;
  //real<lower=0> time_sd;
}

transformed parameters {
  
  vector[num_legis] L_full;
  vector[num_bills] sigma_abs_full;
  vector[num_bills] sigma_reg_full;
  
  vector[num_bills] B_int_full;
  vector[num_bills] A_int_full;
  vector[1] restrict_low;
  
  restrict_low = restrict_high - diff;
  
  //add in a paramter to the intercepts to prevent additive aliasing
  
  B_int_full = B_int_free;
  //B_int_full[1] = 0.0;
  A_int_full = A_int_free;
  //A_int_full[1]=0.0;
  //A_int_full[2:num_bills] = A_int_free;
  //combine constrained and unconstrained parameters
  #include "build_params_v2.stan"
  
  
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
  //time_sd ~ lognormal(1.7,.3);
  L_AR1 ~ normal(0,1); // these parameters shouldn't get too big
  if(model_type>2 && model_type<8) {
     for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
  } else {
    steps_votes ~ normal(0,5);
  }
  L_free ~normal(legis_pred[1, 1:(num_legis - num_fix_high-num_fix_low), ] * legis_x, legis_sd);
	L_tp1[1] ~ normal(legis_pred[1, 1:(num_legis), ] * legis_x,legis_sd/time_sd);
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);

  exog_param ~ normal(0,5);
  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  for(t in 1:T)
    restrict_ord[t] ~ normal(0,5);
  
  //priors for legislators and bill parameters
  #include "modeling_statement_v9.stan"
  
  //all model types

  #include "model_types.stan"

/* This file sets up the various types of IRT models that can be run in idealstan */

  
}

