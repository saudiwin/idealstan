
#include /chunks/license.stan

functions {
  
  // use this to set a prior on discriminations that makes them either +1 or -1
  real genbeta_lpdf(real y, real alpha, real beta, real lb, real lb_offset) {
    
    real x = (y - lb) / lb_offset;
    
    return (alpha - 1) * log(x) + (beta - 1) .* log1m(x) - log(lb_offset) - lbeta(alpha, beta);
  }
  
    real genbeta_vec_lpdf(vector y, real alpha, real beta, real lb, real lb_offset) {
    
    int length_y = num_elements(y);
    real log_prob;
    real log_offset = log(lb_offset);
    real calc_lbeta = lbeta(alpha, beta);
    
    vector[length_y] x = (y - lb) ./ lb_offset;
    vector[length_y] vec_offset = rep_vector(log_offset, length_y);
    vector[length_y] vec_lbeta = rep_vector(calc_lbeta, length_y);
    
    return sum( (alpha - 1) .* log(x) + (beta - 1) .* log1m(x) - vec_offset - vec_lbeta);
  }
  
int r_in(int pos,array[] int pos_var) {
  
  for (p in 1:(size(pos_var))) {
    if (pos_var[p]==pos) {
      // can return immediately, as soon as find a match
      return p;
    }
  }
  return 0;
}
  
#include /chunks/stationary_functions.stan
#include /chunks/jacobians.stan
#include /chunks/calc_rlnorm_gp.stan
#include /chunks/id_params.stan
#include /chunks/id_params2.stan
#include /chunks/map_func.stan
#include /chunks/ordbeta.stan

}

data {
  int N; // total number of observations
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T_spline; // whether to use splines
  int num_basis; // number of basis functions in spline matrix
  int T; // number of time points
  int grainsize;
  int prior_only; // don't sample the likelihood (prior predictive checks)
  array[N_int] int Y_int; // integer outcome
  array[N_cont] real Y_cont; // continuous outcome
  int ignore;
  int y_int_miss; // missing value for integers
  int<lower=0, upper=1> pos_discrim; // whether to constrain all discrimination parameters to be positive (removes need for further identification conditions)
  real y_cont_miss; // missing value for continuous data
  int S;
  int S_type; // whether shards are based on persons or items
  int LX; // legislator/person covariates
  int SRX; // observed item predictors
  int SAX; // missing item predictors
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int num_bills_grm;
  int num_ls;
  array[N] int ll; // persons/legislators id
  array[N] int bb; // items/bills id
  array[N] int time; // time point id
  array[N] int mm; // model counter id
  matrix[(ignore==1) ? (num_legis * T) : 0,3] ignore_db;
  matrix[N,(N>0) ? LX:0] legis_pred;
  matrix[N,(N>0) ? SRX:0] srx_pred;
  matrix[N,(N>0) ? SAX:0] sax_pred;
  matrix[num_basis, T_spline] B; // spline matrix (if used)
  int mod_count; // total number of models
  int tot_cats; // total number of possible ordinal outcomes
  array[tot_cats] int n_cats_rat; // how many outcomes per outcome size int he data
  array[tot_cats] int n_cats_grm; // how many outcomes per outcome size int he data
  array[N_int] int order_cats_rat; // indicator for whether an observation comes from a certain ordinal model
  array[N_int] int order_cats_grm; // indicator for whether an observation comes from a certain ordinal model
  int const_type; // whether to constrain persons (1) or item discriminations (2)
  int num_restrict_high; // number of items/persons constrained to be high
  int num_restrict_low; // number of items/persons constrained to be low
  array[num_restrict_high] int restrict_high; // position of high valued fixed parameter
  array[num_restrict_low] int restrict_low; // position of low valued fixed parameter
  array[num_restrict_high] real fix_high; // value to fix high parameter to
  array[num_restrict_low] real fix_low; // value to fix low parameter to
  real discrim_reg_upb;
  real discrim_reg_lb;
  real discrim_miss_upb;
  real discrim_miss_lb;
  real discrim_reg_scale;
  real discrim_reg_shape;
  real discrim_abs_scale;
  real discrim_abs_shape;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real<lower=0> restrict_sd_high;
  real<lower=0> restrict_sd_low;
  real<lower=0> restrict_N_high;
  real<lower=0> restrict_N_low;
  real ar_sd;
  real time_sd;
  real time_var_sd; // over-time variance of persons
  real ar1_up;  // upper ar1 limit
  real ar1_down; // lower ar1 limit
  real inv_gamma_beta;
  int<lower=2> center_cutoff;
  int restrict_var; // whether to fix the over-time variance of the first person to a value
  array[S,3] int sum_vals; // what to loop over for reduce sum
  int time_proc;
  array[T] real time_ind; // the actual indices/values of time points, used for Gaussian processes
  int zeroes; // whether to use traditional zero-inflation for bernoulli and poisson models
  int num_var;
  array[num_bills] int type_het_var;
  int<lower=0,upper=2> debug_mode;
  int<lower=0> num_ordbeta;
  vector[num_ordbeta] phi_mean;
  array[num_bills] int ordbeta_id;
  array[num_ordbeta] vector[3] ordbeta_cut_alpha;
  vector[num_ordbeta] ordbeta_cut_phi;
  real gp_alpha; // prior for the GP alpha parameter
  real gp_rho; // prior for the GP rho parameter
  real gp_nugget; // residual variation in GP
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
	array[(ignore==1) ? num_legis : 0, 2] int ignore_mat;
	
	// set mean of log-normal distribution for GP length-scale prior
	
	// if(time_proc==4) {
	//   gp_length = gp_prior_mean(time_ind,num_diff,min_length);
	// } else {
	//   gp_length = [0.0]';
	// }
	
	
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

  num_legis_real = num_legis; // promote N to type real
  
  if(ignore==1) {
    
    for(n in 1:num_legis) {
      
      int start = 0;
      int end = T+1;
      int switch_num =  0;
      
      for(t in 1:T) {
        
        if(ignore_db[n + (t-1),3]==0 &&  switch_num == 0) {
          
          if(start==T) {
            
            // failsafe
            
            start = T;
          }
          
          start += 1;
          
        }
        
        if(ignore_db[n + (t-1),3]==1 && start > 0) {
          
          switch_num = 1;
          
        }
        
        if(ignore_db[n + (t-1),3]==0 && start>0 && switch_num ==  1) {
          
          if(t<end) {
            
            end = t;
            
          }
          
        } else if(ignore_db[n + (t-1),3]==0 && start==0) {
          
          
          if(t<end) {
            
            end = t;
            
          }
          
        }
        
      }
      
      // assign start/end for this legislator
      
      ignore_mat[n,1] = start;
      ignore_mat[n,2] = end;
      
    }
    
    
  }

}

parameters {
  vector<lower=-0.999,upper=0.999>[num_bills] sigma_abs_free;
  vector[num_legis] L_full; // first T=1 params to constrain
  vector<lower=0>[gp_N] m_sd_free; // marginal standard deviation of GP
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[num_ls] ls_int_abs; // extra intercepts for non-inflated latent space
  array[T] vector[(T>1 && time_proc!=5) ? num_legis : 0] L_tp1_var; // non-centered variance, don't need for splines
  vector<lower=ar1_down,upper=ar1_up>[(T>1 && time_proc==3) ? num_legis : 0] L_AR1; // AR-1 parameters for AR-1 model
  vector<lower=-.999,upper=.999>[pos_discrim == 0 ? num_bills : 0] sigma_reg_free;
  //vector<lower=0>[pos_discrim == 1 ? num_bills : 0] sigma_reg_pos;
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
  array[num_bills] ordered[n_cats_grm[1]-1] steps_votes_grm3;
  array[num_bills] ordered[n_cats_grm[2]-1] steps_votes_grm4;
  array[num_bills] ordered[n_cats_grm[3]-1] steps_votes_grm5;
  array[num_bills] ordered[n_cats_grm[4]-1] steps_votes_grm6;
  array[num_bills] ordered[n_cats_grm[5]-1] steps_votes_grm7;
  array[num_bills] ordered[n_cats_grm[6]-1] steps_votes_grm8;
  array[num_bills] ordered[n_cats_grm[7]-1] steps_votes_grm9;
  array[num_bills] ordered[n_cats_grm[8]-1] steps_votes_grm10;
  array[num_ordbeta > 0 ? num_ordbeta : 1] ordered[num_ordbeta > 0 ? 2 : 0] ordbeta_cut; // need this for ordbetareg
  vector<lower=0>[num_ordbeta] phi;
  vector<lower=0>[num_var] extra_sd;
  vector<lower=0>[gp_N] time_var_gp_free;
  vector<lower=0>[(T>1 && time_proc!=4 && restrict_var==1) ? num_legis-1 : (T>1 && time_proc!=4 ? num_legis : 0)] time_var_free;
  array[num_legis] row_vector[num_basis > 1 ? num_basis : 0] a_raw;
}

transformed parameters {

  array[T] vector[(T>1 && S_type==0) ? num_legis : 0] L_tp1;
  array[num_basis > 1 && S_type==0 ? num_legis : 0] row_vector[num_basis] a;
  vector[num_bills] sigma_reg_full;
  vector[(T>1 && S_type==0 && time_proc!=4) ? num_legis : 0] time_var_full;
  //vector[S_type==0 ? gp_N : 0] time_var_gp_full;
  vector[S_type==0 ? gp_N : 0] m_sd_full;
  //vector[S_type==0 ? gp_N : 0] gp_sd_full;
  
  if(T>1 && time_proc!=4 && restrict_var==1) {
    time_var_full = append_row([time_sd]',time_var_free);
  }
  
  if(T>1 && num_basis>1 && S_type==0 && restrict_var==1) {
    
    for(n in 1:num_legis)
              a[n] = a_raw[n] * time_var_full[n];
    
  } else if(T>1 && num_basis>1 && S_type==0) {
    
    for(n in 1:num_legis)
              a[n] = a_raw[n] * time_var_free[n];
    
  }
  
    sigma_reg_full = sigma_reg_free;

  if(T>1 && S_type==0) {
    if(time_proc==3) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan
    } else if(time_proc==2){
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    } else if(time_proc==4) {
      // m_sd_full = append_row([m_sd_par]',
      //                         m_sd_free);
      
      L_tp1 = L_tp1_var;
      
    }  else if(time_proc==5) {
    
#include /chunks/l_splines.stan
    
  }


}

model {
  
  // If in debug mode, spit out values of all parameters to see where the problem is
  
  if(debug_mode==2) {
    
    print("Showing values for all parameters: ");
    
#include chunks/debug_mode.stan

  }
  
  legis_x ~ normal(0,5);
  sigma_abs_x ~ normal(0,5);
  sigma_reg_x ~ normal(0,5);
  extra_sd ~ exponential(1);
  
  if(time_proc==3 && S_type==0 && T>1) {
    L_AR1 ~ normal(1,ar_sd);
  }
  

#include /chunks/ord_steps_calc.stan  
  
  if(time_proc==4 && S_type==0 && T>1)  {
    {
    array[gp_N] matrix[T, T] cov; // zero-length if not a GP model
    array[gp_N] matrix[T, T] L_cov;// zero-length if not a GP model
// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   gp_exp_quad_cov(time_ind, m_sd_free[n], time_var_free[n])
      + diag_matrix(rep_vector(gp_nugget,T));
  L_cov[n] = cholesky_decompose(cov[n]);

  to_vector(L_tp1_var[1:T,n]) ~ multi_normal_cholesky(rep_vector(0,T) + L_full[n], L_cov[n]); 
  
    
}
    }
  }
  
  
  if(T>1 && time_proc!=4 && S_type==0) {
    
    if(time_proc==3 || (time_proc==2 && const_type==2)) {
      L_tp1_var[1] ~ normal(0,5);
    }
    
    if(T<center_cutoff) {
    
      for(t in 2:T) {
        L_tp1_var[t] ~ normal(0,1);
      }
      
      
    } else {
      
      int start = 1;
      int end = T;
      
      if(time_proc==2) {
        
        if(restrict_var==1) {
          
          for(n in 1:num_legis) {
            if(ignore==1) {
    
              start = ignore_mat[n,1];
               end = ignore_mat[n,2];
               
                   if(end>T) {
                      end = T;
                    }
                    if(start < 1) {
                      
                      start = 1;
                      
                  }
    
              }
              
              L_tp1_var[(start+1):end,n] ~ normal(L_tp1_var[start:(end-1),n],time_var_full[n]);
              
          }

            
          } else {
          
          for(n in 1:num_legis) {
            
            if(ignore==1) {
    
              start = ignore_mat[n,1];
               end = ignore_mat[n,2];
               
                  if(end>T) {
                      end = T;
                    }
                    if(start < 1) {
                      
                      start = 1;
                      
                  }
    
              }
            
            L_tp1_var[(start+1):end,n] ~ normal(L_tp1_var[start:(end-1),n],time_var_free[n]);
          
          }
                
        }
        
      } else if(time_proc==3) {
        
        if(restrict_var==1) {
          
          for(n in 1:num_legis) {
            
            if(ignore==1) {
    
              start = ignore_mat[n,1];
               end = ignore_mat[n,2];
               
               if(end>T) {
                      end = T;
                    }
                    if(start < 1) {
                      
                      start = 1;
                      
                  }
    
              }
              
              L_tp1_var[(start+1):end,n] ~ normal(L_full[n] + L_AR1[n]*to_vector(L_tp1_var[start:(end-1),n]),time_var_full[n]);
            
          }
              
                
          
        } else {
          
          for(n in 1:num_legis) {
            if(ignore==1) {
    
              start = ignore_mat[n,1];
               end = ignore_mat[n,2];
               
               if(end>T) {
                      end = T;
                    }
                    if(start < 1) {
                      
                      start = 1;
                      
                  }
    
              }
              
              L_tp1_var[(start+1):end,n] ~ normal(L_full[n] + L_AR1[n]*to_vector(L_tp1_var[start:(end-1),n]),time_var_free[n]);
          }
                
          
        }
        
        
      }
      
    } 
    
    
  } 
  
  if(S_type==0 && time_proc==5 && T>1) {
    
    // splines define priors for time series values
    for(n in 1:num_legis)
            a_raw[n] ~ normal(0,legis_sd);
    
  } 
  
  if(S_type==0)  {
    ls_int ~ normal(0,legis_sd);
    ls_int_abs ~ normal(0,legis_sd);
  }
  
  if(T>1 && S_type==0) {
    
    if(inv_gamma_beta>0) {
      
      time_var_free ~ inv_gamma(2,inv_gamma_beta);
        
    } else {
      
      time_var_free ~ exponential(time_var_sd);    
        
    }
    //time_var_gp_free ~ inv_gamma(5,5); // tight-ish prior on additional variances
    time_var_gp_free ~ exponential(gp_rho);
    m_sd_free ~ exponential(gp_alpha);
  }

  //priors for parameters not included in reduce_sum
  // case where restrictions are on persons, not items (S_type==1)
  
if(S_type==1 && const_type==1) {
  // both ID and map for persons
  //sigma_reg_free ~ normal(0, discrim_reg_sd);
  if(debug_mode==2) print("Adding genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb) to target(): ",genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb));
  target += genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb);
  //sigma_reg_pos ~ exponential(1/discrim_reg_scale);
  if(debug_mode==2) print("Adding genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb) to target(): ",genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb));
  target += genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb);
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
} else if(S_type==1 && const_type==2) {
  // map persons, ID items
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
  if(debug_mode==2) print("Adding genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb) to target(): ",genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb));
  target += genbeta_vec_lpdf(sigma_abs_free|discrim_abs_scale,discrim_abs_shape,discrim_miss_lb,discrim_miss_upb);
  
    target += id_params2(sigma_reg_free,
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        restrict_sd_high,
                        restrict_sd_low,
                        0,
                        discrim_reg_scale,
                        discrim_reg_shape,
                        discrim_reg_upb,
                        discrim_reg_lb,
                        restrict_N_high,
                        restrict_N_low,
                        debug_mode);
  
  
  
} else if(S_type==0 && const_type==1) {
  
  // map items, ID persons
  if(pos_discrim==0) {
    
    if(time_proc==2) {
      
      target += id_params(to_vector(L_tp1_var[1,1:num_legis]),
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        restrict_sd_high,
                        restrict_sd_low,
                        0,
                        legis_sd); 
      
      L_full ~ normal(0,legis_sd);
      
    } else {
      
      target += id_params(L_full,
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        restrict_sd_high,
                        restrict_sd_low,
                        0,
                        legis_sd); 
      
      
    }
    
  } else {
    
    L_full ~ normal(0,legis_sd);
    
    if(time_proc==2) {
      to_vector(L_tp1_var[1,1:num_legis]) ~ normal(0,legis_sd);
    }
  }
   
  
} else if(S_type==0 && const_type==2) {
  
  // map items, ID items
  
  L_full ~ normal(0,legis_sd);
  
  if(time_proc==2) {
    
      to_vector(L_tp1_var[1,1:num_legis]) ~ normal(0,legis_sd);
      
  }
  
}
  // temporary
  
  //L_full ~ normal(0,legis_sd);

  //all model types
  if(debug_mode==1) print("target before reduce sum = ", target());
  target += reduce_sum(partial_sum,
                      sum_vals,
                     grainsize,
                     T,
                     pos_discrim,
                     gp_N,
                     num_legis,
                     Y_int,
        Y_cont,
        y_int_miss,
        y_cont_miss,
        S_type,
         srx_pred,
         sax_pred,
         legis_pred,
        bb,
        ll,
        time,
        mm,
        time_ind,
        n_cats_rat,
        mod_count, // total number of models
        tot_cats, // total number of possible ordinal outcomes
        n_cats_grm,
        order_cats_rat,
        order_cats_grm,
        const_type,
        num_restrict_high,
        num_restrict_low,
        restrict_high,
        restrict_low,
        center_cutoff,
        fix_high,
        fix_low,
        restrict_sd_high,
        restrict_sd_low,
        discrim_reg_upb,
        discrim_reg_lb,
        discrim_miss_upb,
        discrim_miss_lb,
        discrim_reg_scale,
        discrim_reg_shape,
        discrim_abs_scale,
        discrim_abs_shape,
        legis_sd,
        diff_abs_sd,
        diff_reg_sd,
        ar_sd,
        time_sd,
        time_var_sd,
        time_proc,
        zeroes, // whether to use traditional zero-inflation for bernoulli and poisson models
        sigma_abs_free,
        L_full, // first T=1 params to constrain
        m_sd_free, // marginal standard deviation of GP
        gp_nugget, // residual GP variation in Y
        ls_int, // extra intercepts for non-inflated latent space
        ls_int_abs, // extra intercepts for non-inflated latent space
        L_tp1_var, // non-centered variance
        L_AR1, // AR-1 parameters for AR-1 model
        sigma_reg_full,
        legis_x,
        sigma_reg_x,
        sigma_abs_x,
        B_int_free,
        A_int_free,
        steps_votes3,
        steps_votes4,
        steps_votes5,
        steps_votes6,
        steps_votes7,
        steps_votes8,
        steps_votes9,
        steps_votes10,
        steps_votes_grm3,
        steps_votes_grm4,
        steps_votes_grm5,
        steps_votes_grm6,
        steps_votes_grm7,
        steps_votes_grm8,
        steps_votes_grm9,
        steps_votes_grm10,
        extra_sd,
        time_var_gp_free,
        L_tp1,
        time_var_free,
        inv_gamma_beta,
        num_var,
        type_het_var,
        restrict_var,
        ignore,
        ignore_mat,
        num_basis,
        a_raw,
        B,
        prior_only,
        restrict_N_high,
        restrict_N_low,
        debug_mode,
        ordbeta_id,
        phi,
        ordbeta_cut,
        gp_rho,
        gp_alpha);
        
      if(debug_mode==1) print("target after reduce sum = ", target());

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

