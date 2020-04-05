
#include /chunks/license.stan

functions {
#include /chunks/stationary_functions.stan
#include /chunks/jacobians.stan
#include /chunks/calc_rlnorm_gp.stan
#include /chunks/id_params.stan
#include /chunks/r_in.stan
#include /chunks/map_rect.stan
}

data {
  int N; // total number of observations
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T; // number of time points
  int Y_int[N_int]; // integer outcome
  int within_chain; // whether to use map_rect
  real Y_cont[N_cont]; // continuous outcome
  int y_int_miss; // missing value for integers
  real y_cont_miss; // missing value for continuous data
  int S; // number of shards
  int S_int; // shard length for integers
  int S_cont; // shard length for reals
  int S_type; // whether shards are based on persons or items
  int int_shards[S,S_int]; // integer shards
  real cont_shards[S,S_cont]; // real shards
  int LX; // legislator/person covariates
  int SRX; // observed item predictors
  int SAX; // missing item predictors
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int num_bills_grm;
  int num_ls;
  int ll[N]; // persons/legislators id
  int bb[N]; // items/bills id
  int time[N]; // time point id
  int mm[N]; // model counter id
  matrix[N,(N>0) ? LX:0] legis_pred;
  matrix[N,(N>0) ? SRX:0] srx_pred;
  matrix[N,(N>0) ? SAX:0] sax_pred;
  int mod_count; // total number of models
  int tot_cats; // total number of possible ordinal outcomes
  int n_cats_rat[tot_cats]; // how many outcomes per outcome size int he data
  int n_cats_grm[tot_cats]; // how many outcomes per outcome size int he data
  int order_cats_rat[N_int]; // indicator for whether an observation comes from a certain ordinal model
  int order_cats_grm[N_int]; // indicator for whether an observation comes from a certain ordinal model
  int const_type; // whether to constrain persons (1) or item discriminations (2)
  int restrict_high; // position of high valued fixed parameter
  int restrict_low; // position of low valued fixed parameter
  real fix_high; // value to fix high parameter to
  real fix_low; // value to fix low parameter to
  real sd_fix; // SD of fixed parameter (should be very low)
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  //real restrict_sd;
  real ar_sd;
  real time_sd;
  //int restrict_var;
  //real restrict_var_high;
  //real restrict_mean_val[2];
  //int restrict_mean_ind[8];
  //int restrict_mean;
  int time_proc;
  real time_ind[T]; // the actual indices/values of time points, used for Gaussian processes
  int zeroes; // whether to use traditional zero-inflation for bernoulli and poisson models
  real gp_sd_par; // residual variation in GP
  real num_diff; // number of time points used to calculate GP length-scale prior
  real m_sd_par; // the marginal standard deviation of the GP
  int min_length; // the minimum threshold for GP length-scale prior
}

transformed data {
	int m;                         // missing value
	real m_cont; // missing value if continuous
	int m_step; // number of ordinal categories
	int num_constrain_l;
	//int num_var_free; // whether to restrict variance parameters
	//int num_var_restrict;
	real num_legis_real; // used to adjust jacobian for mean restriction
	int gp_N; // use for creating zero-length arrays if gp not used
	int gp_N_fix; // same but for fixed parameters
	int gp_1; // zero-length gp-related scalars
	int gp_nT; // used to make L_tp1 go to model block if GPs are used
	int gp_oT; // used to make L_tp1 go to model block if GPs are used
	vector[1] gp_length; 
	int vP;
	int dP;
	
	// set mean of log-normal distribution for GP length-scale prior
	
	if(time_proc==4) {
	  gp_length = gp_prior_mean(time_ind,num_diff,min_length);
	} else {
	  gp_length = [0.0]';
	}
	
	
	//reset these values to use GP-specific parameters
	if(time_proc!=4) {
	  gp_N=0;
	  gp_N_fix=0;
	  gp_1=0;
	  gp_oT=T;
	  gp_nT=0;

	} else {
	  gp_N=num_legis;
	  gp_N_fix=num_legis-1;
	  gp_1=1;
	  gp_nT=T;
	  gp_oT=0;
	}
	
	if(within_chain==1) {
	  if(S_type==1) {
	    // if we map over people, then only one varying parameter per shard
	    if(time_proc==4) {
	      vP = 1*T + 4;
	    } else if(time_proc==3) {
	      vP = 1*T + 3;
	    } else if(time_proc==2) {
	      vP = 1*T + 2;
	    } else if(T==1) {
	      if(num_ls==0) {
	         vP = 1;
	      } else {
	        vP = 2;
	      }
	    }
	    
	    // include one extra for the extra_sd parameter
	    dP = 4*num_bills + (sum(n_cats_rat) - 8) + (sum(n_cats_grm) - 8)*num_bills_grm + LX + SRX + SAX + 1;
	  } else {
	    // 6 parameters for all item parameters
	    vP = 4; 
	    dP = num_legis*T + num_ls + (sum(n_cats_rat) - 8) + (sum(n_cats_grm) - 8)*num_bills_grm + LX + SRX + SAX + 1;
	  }
	  
	} else {
	  vP = 0;
	  dP = 0;
	}
	
	// need to assign a type of outcome to Y based on the model (discrete or continuous)
	// to do this we need to trick Stan into assigning to an integer. 
	
//#include /chunks/change_outcome.stan
	
  //determine how many and which parameters to constrain
#include /chunks/create_constrained.stan

// determine whether to restrict variance or not

// if(restrict_var==1) {
//   num_var_restrict=num_legis;
//   num_var_free=0;
// } else {
//   num_var_restrict=0;
//   num_var_free=num_legis;
// }

  num_legis_real = num_legis; // promote N to type real
  
  
}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis] L_full; // first T=1 params to constrain
  vector<lower=0>[gp_N_fix] m_sd_free; // marginal standard deviation of GP
  vector<lower=0>[gp_N_fix] gp_sd_free; // residual GP variation in Y
  vector[num_legis] L_tp2[gp_nT]; // additional L_tp1 for GPs only
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[T>1 ? num_legis : 0] L_tp1_var[T-1]; // non-centered variance
  vector<lower=-.99,upper=.99>[(T>1 && time_proc==3) ? num_legis : 0] L_AR1; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  //vector[1] restrict_high;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  ordered[n_cats_rat[1]-1] steps_votes3;
  ordered[n_cats_rat[2]-1] steps_votes4;
  ordered[n_cats_rat[3]-1] steps_votes5;
  ordered[n_cats_rat[4]-1] steps_votes6;
  ordered[n_cats_rat[5]-1] steps_votes7;
  ordered[n_cats_rat[6]-1] steps_votes8;
  ordered[n_cats_rat[7]-1] steps_votes9;
  ordered[n_cats_rat[8]-1] steps_votes10;
  ordered[n_cats_grm[1]-1] steps_votes_grm3[num_bills_grm];
  ordered[n_cats_grm[2]-1] steps_votes_grm4[num_bills_grm];
  ordered[n_cats_grm[3]-1] steps_votes_grm5[num_bills_grm];
  ordered[n_cats_grm[4]-1] steps_votes_grm6[num_bills_grm];
  ordered[n_cats_grm[5]-1] steps_votes_grm7[num_bills_grm];
  ordered[n_cats_grm[6]-1] steps_votes_grm8[num_bills_grm];
  ordered[n_cats_grm[7]-1] steps_votes_grm9[num_bills_grm];
  ordered[n_cats_grm[8]-1] steps_votes_grm10[num_bills_grm];
  real<lower=0> extra_sd;
  //real<lower=-.9,upper=.9> ar_fix;
  vector[gp_N_fix] time_var_gp_free;
  vector<lower=0>[(T>1 && time_proc!=4) ? num_legis-1 : 0] time_var_free;
  
}

transformed parameters {

  vector[T>1 ? num_legis : 0] L_tp1[T];
  vector[T>1 ? num_legis : 0] time_var_full;
  vector[gp_N] time_var_gp_full;
  vector[gp_N] m_sd_full;
  vector[gp_N] gp_sd_full;
  vector[vP] varparams[S]; // varying parameters if map_rect
  vector[dP] dparams; // stacked (constant parameters) if map_rect
  
  if(T>1) {
    time_var_full = append_row([time_sd]',time_var_free);
  }
  
    //combine constrained and unconstrained parameters
    //#include /chunks/build_params_v2.stan

  if(within_chain==0 || (within_chain==1 && S_type==0)) {
    if(T>1) {
    if(time_proc==3) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan
    } else if(time_proc==2){
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    } else if(time_proc==4) {
      L_tp1 = L_tp2; // just copy over the variables, saves code if costs a bit of extra memory
                      // should be manageable memory loss
      m_sd_full = append_row([m_sd_par]',
                              m_sd_free);
      gp_sd_full = append_row([gp_sd_par]',
                              gp_sd_free);
      time_var_gp_full = append_row(gp_length,
                                      time_var_gp_free);
    }  
  } 
  }

  
    // create map_rect parameters only if S>0
  
  if(S>0) {
#include /chunks/map_build_params.stan
  }
}

model {
  
  legis_x ~ normal(0,5);
  sigma_abs_x ~ normal(0,5);
  sigma_reg_x ~ normal(0,5);
  extra_sd ~ exponential(1);
  
  if(time_proc==3 && (within_chain==0 || (within_chain==1 && S_type==0)) && T>1) {
    L_AR1 ~ normal(0,ar_sd);
  }
  

#include /chunks/ord_steps_calc.stan  
  
  if(time_proc==4 && (within_chain==0 || (within_chain==1 && S_type==0)) && T>1)  {
    {
    matrix[T, T] cov[gp_N]; // zero-length if not a GP model
    matrix[T, T] L_cov[gp_N];// zero-length if not a GP model
// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   cov_exp_quad(time_ind, m_sd_full[n], time_var_full[n])
      + diag_matrix(rep_vector(square(gp_sd_full[n]),T));
  L_cov[n] = cholesky_decompose(cov[n]);

  to_vector(L_tp2[,n]) ~ multi_normal_cholesky(rep_vector(0,T) + L_full[n], L_cov[n]); 
  
    
}
    }
  }
  if(T>1 && time_proc!=4 && (within_chain==1 && S_type==0)) {
    for(t in 1:(T-1)) {
      L_tp1_var[t] ~ normal(0,1);
    }
  }
  
  if(within_chain==0 || (within_chain==1 && S_type==0))  {
    ls_int ~ normal(0,legis_sd);
  }
  
  //m_sd_free ~ inv_gamma(m_sd_par[2],1); // tight prior on GP marginal standard deviation ("bumps")
  
  if(T>1 && (within_chain==0 || (within_chain==1 && S_type==0))) {
    time_var_free ~ normal(0,1); // tight-ish prior on additional variances
    time_var_gp_free ~ normal(0,1); // tight-ish prior on additional variances
    gp_sd_free ~ normal(0,2);
    m_sd_free ~ normal(0,2);
  }
  

// add correction for time-series models
  

  //priors for legislators and bill parameters
  
if(within_chain==1 && S_type==1) {
  // don't create priors for persons if map_rect is used on persons
  sigma_reg_free ~ normal(0, discrim_reg_sd);
  sigma_abs_free ~ normal(0,discrim_abs_sd);
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
} else if(within_chain==1 && S_type==0) {
  // don't create priors for items
  L_full ~ normal(0,legis_sd);
} else {
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
#include /chunks/fix_priors.stan   
}


  //all model types

// use map_rect or don't use map_rect
if(within_chain==1) {
  target += sum(map_rect(overT,dparams,varparams,cont_shards,int_shards));
  // print("log density before =", target());
  // print(sum(map_rect(overT,dparams,varparams,cont_shards,int_shards)));
  // print("log density after =", target());
} else {
#include /chunks/model_types_mm.stan 
}

}
generated quantities {
/*
vector[num_legis] L_tp2[gp_nT]; // equal to T if GPs are used
  row_vector[gp_nT] calc_values; // used to hold values from legis_preds
  if(time_proc==4) {
  
  for(n in 1:gp_N) {
    // generate estimated ideal points here to avoid creating all the correlation matrices
    for(t in 1:T) {
        calc_values[t] = legis_pred[t, n, ] * legis_x;
    }
    L_tp2[,n] = multi_normal_cholesky(calc_values * legis_x, L_cov[n]); 
    
  }
  */
}

