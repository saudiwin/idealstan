
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
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int num_fix_high;
  int num_fix_low;
  int ll[N];
  int bb[N];
  int time[N];
  matrix[num_legis,LX] legis_pred[T];
  matrix[num_bills,SRX] srx_pred;
  matrix[num_bills,SAX] sax_pred;
  real diff; // difference between high and low constrained parameters
  real diff_high;
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real restrict_sd;
  real ar_sd;
  int sample_stationary;
  real time_sd;
  int restrict_var;
  real restrict_var_high;
}

transformed data {
	int m;                         // missing value
	real m_cont; // missing value if continuous
	int m_step; // number of ordinal categories
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
	int Y_new[N];
	int num_var_free; // whether to restrict variance parameters
	int num_var_restrict;
	
	// need to assign a type of outcome to Y based on the model (discrete or continuous)
	// to do this we need to trick Stan into assigning to an integer. 
	
#include /chunks/change_outcome.stan
	
  //determine how many and which parameters to constrain
#include /chunks/create_constrained.stan

}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis - num_constrain_l] L_free; // first T=1 params to constrain
  vector[num_legis] L_tp1_var[T-1]; // non-centered variance
  vector<lower=-.99,upper=.99>[num_legis-1] L_AR1_free; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  vector[num_fix_high] restrict_high;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[LX] legis_x_cons;
  vector[SRX] sigma_reg_x_cons;
  vector[SAX] sigma_abs_x_cons;
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  ordered[m_step-1] steps_votes;
  ordered[m_step-1] steps_votes_grm[num_bills];
  real<lower=0> extra_sd;
  real<lower=-.9,upper=.9> ar_fix;
  vector<lower=0>[num_var_free] time_var;
  vector<lower=0,upper=restrict_var_high>[num_var_restrict] time_var_restrict;
  
}

transformed parameters {

  vector[num_legis] L_full;
  vector[num_legis] L_AR1;
  vector[num_legis] L_tp1[T]; // all other params can float
  vector[1] restrict_low;
  

  restrict_low = restrict_high - diff;

  L_AR1 = append_row(L_AR1_free,ar_fix);
  
    //combine constrained and unconstrained parameters
#include /chunks/build_params_v2.stan

  
  if(T>1) {
    if(use_ar==1) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan

    } else {
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    }
  } else {
    L_tp1[1] = L_full;
  }
  
  //convert unconstrained parameters to only sample in the constrained stationary space 
  //code from Jeffrey Arnold 
  //https://github.com/stan-dev/math/issues/309
  /*if(sample_stationary==1) {
    L_AR1_r = constrain_stationary(L_AR1_free);
    L_AR1 = pacf_to_acf(L_AR1_r);
    
  } else {
    L_AR1 = L_AR1_free;
  } */

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
  extra_sd ~ exponential(1);
  ar_fix ~ normal(0,1);
  L_AR1_free ~ normal(0,ar_sd);

  if(model_type>2 && model_type<5) {
     for(i in 1:(m_step-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5);
  }
  } else {
    steps_votes ~ normal(0,5);
  }
  if(T==1) {
    L_free ~normal(legis_pred[1, 1:(num_legis - num_constrain_l), ] * legis_x, legis_sd);
  } else {
    L_free ~ normal(0,legis_sd);
  }
  
  for(t in 1:(T-1)) {
    L_tp1_var[t] ~ normal(0,1);
  }

  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);

  //exog_param ~ normal(0,5);
  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  

  //priors for legislators and bill parameters
#include /chunks/modeling_statement_v9.stan

  //all model types

#include /chunks/model_types.stan

/* This file sets up the various types of IRT models that can be run in idealstan */


}

