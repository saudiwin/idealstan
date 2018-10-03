
#include /chunks/license.stan

functions {
#include /chunks/stationary_functions.stan
}

data {
  int N;
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T;
  int Y_int[N_int]; // integer outcome
  real Y_cont[N_cont]; // continuous outcome
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
  //vector[N] exog_data;
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
  real ar_sd;
  int sample_stationary;
}

transformed data {
	int m;                         // missing value
	int m_step; // number of ordinal categories
	int absence[N]; // need to create absence indicator
	int Y_new[N];
	real m_cont;
	
	// need to assign a type of outcome to Y based on the model (discrete or continuous)
	// to do this we need to trick Stan into assigning to an integer. 
	
#include /chunks/change_outcome.stan
}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis] L_free;
  //vector[num_legis] L_tp1_free[T]; // all other params can float
  vector[T-1] L_tp1_var; // non-centered variance
  vector<lower=-1,upper=1>[num_legis] L_AR1; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[LX] legis_x_cons;
  vector[SRX] sigma_reg_x_cons;
  vector[SAX] sigma_abs_x_cons;
  ordered[m_step-1] steps_votes;
  ordered[m_step-1] steps_votes_grm[num_bills];
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  real<lower=0> extra_sd;
  //real<lower=0> time_sd;
}

transformed parameters {

  vector[num_legis] L_full;
  vector[num_legis] L_tp1[T]; // all other params can float
  
  L_full=L_free;
  
  if(T>1) {
    if(use_ar==1) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan

    } else {
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    }
  }
  
  L_tp1[1] = L_full;

}

model {
  //vectors to hold model calculations
  vector[N] pi1;
  vector[N] pi2;


  if(T==1) {
    L_free ~normal(legis_pred[1, 1:(num_legis), ] * legis_x, legis_sd);
  } else {
    L_free ~ normal(0,legis_sd);
  }


  sigma_abs_free ~ normal(0,discrim_abs_sd);
  sigma_reg_free ~ normal(0,discrim_reg_sd);
  legis_x ~ normal(0,5);
  sigma_reg_x ~ normal(srx_pred[num_bills, ] * sigma_reg_x,5);
  sigma_abs_x ~ normal(sax_pred[num_bills, ] * sigma_abs_x,5);
  legis_x_cons ~ normal(0,5);;
  sigma_reg_x_cons ~ normal(0,5);
  sigma_abs_x_cons ~ normal(0,5);
  L_AR1 ~ normal(0,ar_sd); // these parameters shouldn't get too big
  extra_sd ~ exponential(1);
  //time_sd ~ exponential(5);
  if(model_type>2 && model_type<5) {
    for(i in 1:(m_step-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5);
    }
  } else {
    steps_votes ~ normal(0,5);
  }
  /* if(sample_stationary==1) {
    target += jacobian_stationary(L_AR1_r);
  } */

  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);

  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  //model

#include /chunks/model_types.stan



}

