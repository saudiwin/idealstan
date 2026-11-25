
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

  // Normal scores Phi^-1(F(y)) of discriminations under their prior CDF F, used by the
  // Gaussian copula that correlates observed and missingness discriminations.
  // Each is computed from whichever tail is smaller, to keep precision near the bounds.
  real genbeta_normal_score(real y, real alpha, real beta, real lb, real lb_offset) {
    real x = (y - lb) / lb_offset;
    if (x < 0.5) {
      return std_normal_log_qf(beta_lcdf(x | alpha, beta));
    } else {
      return -std_normal_log_qf(beta_lcdf(1 - x | beta, alpha));
    }
  }

  real exponential_normal_score(real y, real rate) {
    if (rate * y < log2()) {
      return std_normal_log_qf(log1m_exp(-rate * y));
    } else {
      return -std_normal_log_qf(-rate * y);
    }
  }

  // log density of a bivariate Gaussian copula with correlation rho, evaluated at
  // the normal scores z1 and z2 of each pair
  real gaussian_copula_lpdf(vector z1, vector z2, real rho) {
    real r2 = square(rho);
    return -0.5 * num_elements(z1) * log1m(r2) -
      (r2 * (dot_self(z1) + dot_self(z2)) - 2 * rho * dot_product(z1, z2)) / (2 * (1 - r2));
  }

  // Time paths of persons' ideal points on dimensions 2 to D (one column per
  // dimension), built the same way as dimension 1 when persons are mapped over:
  // random walk (time_proc 2) and AR(1) (3) from non-centered innovations, or
  // the raw parameters themselves when the series is centered (T >= center_cutoff)
  // or a Gaussian process (4); splines (5) from the basis coefficients.
  array[] matrix extra_dim_paths(int T, int time_proc, int center_cutoff,
                                 array[] matrix raw, matrix ar1, matrix time_sd_d,
                                 array[,] row_vector a_raw_d, matrix B,
                                 int ignore, array[,] int ignore_mat,
                                 int N, int Dm) {
    array[T] matrix[N, Dm] path;

    if (time_proc == 5) {
      for (n in 1:N) {
        for (d in 1:Dm) {
          row_vector[T] x = a_raw_d[n, d] * B;
          for (t in 1:T) path[t][n, d] = x[t];
        }
      }
    } else if (time_proc == 4 || T >= center_cutoff) {
      path = raw;
    } else {
      for (n in 1:N) {
        int start_time = 1;
        int end_time = T + 1;
        if (ignore == 1) {
          start_time = ignore_mat[n, 1];
          end_time = ignore_mat[n, 2];
        }
        for (t in 1:T) {
          if (t == 1 || t <= start_time || t >= end_time) {
            path[t][n] = raw[t][n];
          } else if (time_proc == 2) {
            path[t][n] = path[t - 1][n] + time_sd_d[n] .* raw[t][n];
          } else {
            path[t][n] = raw[1][n] + ar1[n] .* path[t - 1][n] + time_sd_d[n] .* raw[t][n];
          }
        }
      }
    }

    return path;
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
  array[N_int] int<lower=0> trials; // number of trials for binomial outcomes (models 17 and 18)
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
  int<lower=1> D; // number of dimensions of the ideal points
  array[D - 1] int<lower=1> restrict_dim; // person or item that identifies each of dimensions 2 to D
  array[D - 1] real fix_dim; // value the identifying person is pinned to on its dimension (const_type 1)
  real discrim_reg_upb;
  real discrim_reg_lb;
  real discrim_miss_upb;
  real discrim_miss_lb;
  real discrim_reg_scale;
  real discrim_reg_shape;
  real discrim_abs_scale;
  real discrim_abs_shape;
  int<lower=0> num_corr; // number of items whose observed and missingness discriminations are correlated
  array[num_corr] int<lower=1, upper=num_bills> corr_items; // those items
  real<lower=0> discrim_corr_eta; // prior on their correlation: (rho + 1)/2 ~ beta(eta, eta)
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  array[num_restrict_high] real<lower=0> restrict_sd_high;
  array[num_restrict_low] real<lower=0> restrict_sd_low;
  array[num_restrict_high] real<lower=0> restrict_N_high;
  array[num_restrict_low] real<lower=0> restrict_N_low;
  vector[2] ar_prior;
  real time_sd;
  real time_var_sd; // over-time variance of persons
  real ar1_up;  // upper ar1 limit
  real ar1_down; // lower ar1 limit
  real inv_gamma_beta;
  int<lower=2> center_cutoff;
  int restrict_var; // whether to fix the over-time variance of the first person to a value
  array[S,3] int sum_vals; // what to loop over for reduce sum
  int time_proc;
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
	array[T] real time_ind; // the actual indices/values of time points, used for Gaussian processes
	array[(ignore==1) ? num_legis : 0, 2] int ignore_mat;

	// bounds of the discrimination parameters: the support of their priors, pulled in
	// slightly from the edges of the generalized beta (the defaults give -0.999 and 0.999)
	real reg_lower = pos_discrim == 1 ? 0 : discrim_reg_lb + 0.0005 * discrim_reg_upb;
	real reg_upper = pos_discrim == 1 ? positive_infinity() : discrim_reg_lb + 0.9995 * discrim_reg_upb;
	real abs_lower = discrim_miss_lb + 0.0005 * discrim_miss_upb;
	real abs_upper = discrim_miss_lb + 0.9995 * discrim_miss_upb;

	// Dimensions 2 to D. When items are pinned (const_type 2), the rotation is
	// identified with a lower-triangular discrimination matrix: the item pinned
	// high on dimension 1 has discriminations of 0 on dimensions 2 to D, and the
	// item identifying dimension d has 0 on the dimensions after d and the pinned
	// prior on dimension d (unless discriminations are positive).
	int Dm = D - 1;
	array[num_bills, Dm] int zero_d = rep_array(0, num_bills, Dm);
	array[num_bills, Dm] int anchor_d = rep_array(0, num_bills, Dm);
	int num_reg_d = 0; // free discriminations on dimensions 2 to D
	int num_reg_d_anchor = 0;

	if(Dm > 0 && const_type == 2) {
	  for(d in 1:Dm) zero_d[restrict_high[1], d] = 1;
	  for(k in 1:Dm) {
	    if(pos_discrim == 0) anchor_d[restrict_dim[k], k] = 1;
	    for(d in (k + 1):Dm) zero_d[restrict_dim[k], d] = 1;
	  }
	}

	for(i in 1:num_bills) {
	  for(d in 1:Dm) {
	    num_reg_d += 1 - zero_d[i, d];
	    num_reg_d_anchor += anchor_d[i, d];
	  }
	}

	array[num_reg_d] int reg_d_item;
	array[num_reg_d] int reg_d_dim;
	array[num_reg_d - num_reg_d_anchor] int reg_d_std; // free discriminations with the standard prior
	array[num_reg_d_anchor] int reg_d_anchor; // free discriminations with the pinned prior

	{
	  int k = 0;
	  int k_std = 0;
	  int k_anchor = 0;
	  for(i in 1:num_bills) {
	    for(d in 1:Dm) {
	      if(zero_d[i, d] == 0) {
	        k += 1;
	        reg_d_item[k] = i;
	        reg_d_dim[k] = d;
	        if(anchor_d[i, d] == 1) {
	          k_anchor += 1;
	          reg_d_anchor[k_anchor] = k;
	        } else {
	          k_std += 1;
	          reg_d_std[k_std] = k;
	        }
	      }
	    }
	  }
	}

	// copula pairs on dimensions 2 to D: the free, unpinned discriminations of the
	// items in corr_items
	int num_corr_d = 0;
	for(k in 1:num_corr) {
	  for(d in 1:Dm) {
	    num_corr_d += (zero_d[corr_items[k], d] == 0 && anchor_d[corr_items[k], d] == 0);
	  }
	}
	array[num_corr_d] int corr_d_item;
	array[num_corr_d] int corr_d_dim;
	{
	  int j = 0;
	  for(k in 1:num_corr) {
	    for(d in 1:Dm) {
	      if(zero_d[corr_items[k], d] == 0 && anchor_d[corr_items[k], d] == 0) {
	        j += 1;
	        corr_d_item[j] = corr_items[k];
	        corr_d_dim[j] = d;
	      }
	    }
	  }
	}

	for (t in 1:T) {
    time_ind[t] = t;
  }
	
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
  vector<lower=abs_lower,upper=abs_upper>[num_bills] sigma_abs_free;
  vector<lower=-1,upper=1>[num_corr > 0 ? 1 : 0] discrim_rho; // correlation of observed and missingness discriminations (Gaussian copula)
  vector[num_legis] L_full; // first T=1 params to constrain
  vector<lower=0>[gp_N] m_sd_free; // marginal standard deviation of GP
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[num_ls] ls_int_abs; // extra intercepts for non-inflated latent space
  array[T] vector[(T>1 && time_proc!=5) ? num_legis : 0] L_tp1_var; // non-centered variance, don't need for splines
  vector<lower=ar1_down,upper=ar1_up>[(T>1 && time_proc==3) ? num_legis : 0] L_AR1; // AR-1 parameters for AR-1 model
  vector<lower=reg_lower,upper=reg_upper>[num_bills] sigma_reg_free;
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
  // dimensions 2 to D (all zero-length when D = 1)
  matrix[num_legis, Dm] L_full_d; // ideal points
  vector<lower=reg_lower,upper=reg_upper>[num_reg_d] sigma_reg_d_free; // free discriminations
  matrix<lower=abs_lower,upper=abs_upper>[num_bills, Dm] sigma_abs_d; // missingness discriminations
  matrix[LX, Dm] legis_x_d; // person covariates
  matrix[SRX, Dm] sigma_reg_x_d; // item covariates of discriminations
  matrix[SAX, Dm] sigma_abs_x_d; // item covariates of missingness discriminations
  array[T] matrix[(T>1 && time_proc!=5) ? num_legis : 0, Dm] L_tp1_var_d; // time-series parameters
  matrix<lower=ar1_down,upper=ar1_up>[(T>1 && time_proc==3) ? num_legis : 0, Dm] L_AR1_d;
  matrix<lower=0>[(T>1 && (time_proc==2 || time_proc==3)) ? (restrict_var==1 ? num_legis - 1 : num_legis) : 0, Dm] time_var_free_d;
  matrix<lower=0>[gp_N, Dm] m_sd_free_d;
  matrix<lower=0>[gp_N, Dm] time_var_gp_free_d;
  array[num_legis, Dm] row_vector[num_basis > 1 ? num_basis : 0] a_raw_d;
}

transformed parameters {

  array[T] vector[(T>1 && S_type==0) ? num_legis : 0] L_tp1;
  array[num_basis > 1 && S_type==0 ? num_legis : 0] row_vector[num_basis] a;
  vector[num_bills] sigma_reg_full;
  vector[(T>1 && S_type==0 && time_proc!=4) ? num_legis : 0] time_var_full;
  //vector[S_type==0 ? gp_N : 0] time_var_gp_full;
  vector[S_type==0 ? gp_N : 0] m_sd_full;
  //vector[S_type==0 ? gp_N : 0] gp_sd_full;
  matrix[num_bills, Dm] sigma_reg_d; // discriminations on dimensions 2 to D (0 where fixed for identification)
  array[T] matrix[T>1 ? num_legis : 0, Dm] L_tp1_d; // time-varying ideal points on dimensions 2 to D

  if(Dm > 0) {

    sigma_reg_d = rep_matrix(0, num_bills, Dm);
    for(k in 1:num_reg_d) sigma_reg_d[reg_d_item[k], reg_d_dim[k]] = sigma_reg_d_free[k];

    if(T > 1) {
      matrix[num_legis, Dm] time_sd_d = rep_matrix(0, num_legis, Dm);
      if(time_proc==2 || time_proc==3) {
        if(restrict_var==1) {
          time_sd_d = append_row(rep_row_vector(time_sd, Dm), time_var_free_d);
        } else {
          time_sd_d = time_var_free_d;
        }
      }
      L_tp1_d = extra_dim_paths(T, time_proc, center_cutoff, L_tp1_var_d, L_AR1_d, time_sd_d,
                                a_raw_d, B, ignore, ignore_mat, num_legis, Dm);
    }

  }

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
    target += genbeta_vec_lpdf(L_AR1 | ar_prior[1],ar_prior[2],ar1_down, ar1_up - ar1_down);
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
              
              L_tp1_var[(start+1):end,n] ~ normal(L_tp1_var[start,n] + L_AR1[n]*to_vector(L_tp1_var[start:(end-1),n]),time_var_full[n]);
            
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
              
              L_tp1_var[(start+1):end,n] ~ normal(L_tp1_var[start,n] + L_AR1[n]*to_vector(L_tp1_var[start:(end-1),n]),time_var_free[n]);
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
  if(pos_discrim==0) {
    if(debug_mode==2) print("Adding genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb) to target(): ",genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb));
    target += genbeta_vec_lpdf(sigma_reg_free|discrim_reg_scale,discrim_reg_shape,discrim_reg_lb,discrim_reg_upb);
  } else {
    sigma_reg_free ~ exponential(1/discrim_reg_scale);
  }
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

  if(pos_discrim==1) {
    sigma_reg_free ~ exponential(1/discrim_reg_scale);
  } else {
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
  }

} else if(S_type==0 && const_type==1) {
  
  // map items, ID persons
  // (positive discriminations don't fix the rotation, so with D > 1 persons are still pinned)
  if(pos_discrim==0 || D > 1) {

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

    // the persons identifying dimensions 2 to D are pinned at 0 on dimension 1,
    // replacing the standard prior id_params gave them
    for(k in 1:Dm) {
      real v = time_proc==2 ? L_tp1_var[1, restrict_dim[k]] : L_full[restrict_dim[k]];
      target += normal_lpdf(v | 0, restrict_sd_high[1]) - normal_lpdf(v | 0, legis_sd);
    }

  } else {
    
    L_full ~ normal(0,legis_sd);
    
    if(time_proc==2 || time_proc==3) {
      to_vector(L_tp1_var[1,1:num_legis]) ~ normal(0,legis_sd);
    }
    
  }
   
  
} else if(S_type==0 && const_type==2) {
  
  // map items, ID items
  
  L_full ~ normal(0,legis_sd);
  
  if(time_proc==2 || time_proc==3) {
    
      to_vector(L_tp1_var[1,1:num_legis]) ~ normal(0,legis_sd);
      
  }
  
}
  // Gaussian copula correlating the observed and missingness discriminations of
  // inflated items. The priors above are each discrimination's marginal
  // distribution, so this term only adds their dependence.
  if(num_corr > 0) {
    vector[num_corr + num_corr_d] z_reg;
    vector[num_corr + num_corr_d] z_abs;

    for(k in 1:(num_corr + num_corr_d)) {
      // dimension 1 first, then the free pairs on dimensions 2 to D
      real reg;
      real abs_k;
      if(k <= num_corr) {
        reg = sigma_reg_free[corr_items[k]];
        abs_k = sigma_abs_free[corr_items[k]];
      } else {
        reg = sigma_reg_d[corr_d_item[k - num_corr], corr_d_dim[k - num_corr]];
        abs_k = sigma_abs_d[corr_d_item[k - num_corr], corr_d_dim[k - num_corr]];
      }
      if(pos_discrim == 1) {
        z_reg[k] = exponential_normal_score(reg, 1 / discrim_reg_scale);
      } else {
        z_reg[k] = genbeta_normal_score(reg, discrim_reg_scale, discrim_reg_shape,
                                        discrim_reg_lb, discrim_reg_upb);
      }
      z_abs[k] = genbeta_normal_score(abs_k, discrim_abs_scale, discrim_abs_shape,
                                      discrim_miss_lb, discrim_miss_upb);
    }

    target += beta_lpdf((discrim_rho[1] + 1) / 2 | discrim_corr_eta, discrim_corr_eta);
    target += gaussian_copula_lpdf(z_reg | z_abs, discrim_rho[1]);
  }

  // priors for dimensions 2 to D
  if(Dm > 0) {

    to_vector(legis_x_d) ~ normal(0,5);
    to_vector(sigma_reg_x_d) ~ normal(0,5);
    to_vector(sigma_abs_x_d) ~ normal(0,5);

    // discriminations
    target += genbeta_vec_lpdf(to_vector(sigma_abs_d) | discrim_abs_scale, discrim_abs_shape,
                               discrim_miss_lb, discrim_miss_upb);
    if(pos_discrim == 1) {
      sigma_reg_d_free ~ exponential(1/discrim_reg_scale);
    } else {
      target += genbeta_vec_lpdf(sigma_reg_d_free[reg_d_std] | discrim_reg_scale, discrim_reg_shape,
                                 discrim_reg_lb, discrim_reg_upb);
      for(k in 1:num_reg_d_anchor) {
        target += genbeta_lpdf(sigma_reg_d_free[reg_d_anchor[k]] | restrict_N_high[1], restrict_sd_high[1],
                               discrim_reg_lb, discrim_reg_upb);
      }
    }

    // ideal points. When persons are pinned (const_type 1), the person identifying
    // dimension d is pinned at fix_dim[d] on it, and the other pinned persons at 0.
    // The pins are on the first time point of random-walk and AR(1) models and on
    // the static ideal points otherwise, as for dimension 1.
    {
      int pin_first = T > 1 && (time_proc == 2 || time_proc == 3);

      for(d in 1:Dm) {
        vector[num_legis] pin_mu = rep_vector(0, num_legis);
        vector[num_legis] pin_sd = rep_vector(legis_sd, num_legis);

        if(const_type == 1) {
          pin_sd[restrict_high[1]] = restrict_sd_high[1];
          pin_sd[restrict_low[1]] = restrict_sd_high[1];
          for(k in 1:Dm) pin_sd[restrict_dim[k]] = restrict_sd_high[1];
          pin_mu[restrict_dim[d]] = fix_dim[d];
        }

        if(pin_first) {
          L_tp1_var_d[1][, d] ~ normal(pin_mu, pin_sd);
          L_full_d[, d] ~ normal(0, legis_sd);
        } else {
          L_full_d[, d] ~ normal(pin_mu, pin_sd);
        }
      }
    }

    if(T > 1) {

      if(time_proc == 2 || time_proc == 3) {

        if(T < center_cutoff) {

          for(t in 2:T) to_vector(L_tp1_var_d[t]) ~ std_normal();

        } else {

          // centered: the parameters are the path
          for(n in 1:num_legis) {

            int start_time = 1;
            int end_time = T;

            if(ignore == 1) {
              start_time = ignore_mat[n, 1];
              end_time = ignore_mat[n, 2];
              if(end_time > T) end_time = T;
              if(start_time < 1) start_time = 1;
              if(start_time == T) start_time = T - 1;
            }

            for(d in 1:Dm) {
              vector[T] x;
              real sd_nd;
              for(t in 1:T) x[t] = L_tp1_var_d[t][n, d];
              if(restrict_var == 1) {
                sd_nd = n == 1 ? time_sd : time_var_free_d[n - 1, d];
              } else {
                sd_nd = time_var_free_d[n, d];
              }
              if(time_proc == 2) {
                x[(start_time + 1):end_time] ~ normal(x[start_time:(end_time - 1)], sd_nd);
              } else {
                x[(start_time + 1):end_time] ~ normal(x[start_time] + L_AR1_d[n, d] * x[start_time:(end_time - 1)], sd_nd);
              }
              if(start_time > 1) x[2:start_time] ~ normal(0, legis_sd);
              if(end_time < T) x[(end_time + 1):T] ~ normal(0, legis_sd);
            }
          }

        }

        if(inv_gamma_beta > 0) {
          to_vector(time_var_free_d) ~ inv_gamma(2, inv_gamma_beta);
        } else {
          to_vector(time_var_free_d) ~ exponential(time_var_sd);
        }

        if(time_proc == 3) {
          target += genbeta_vec_lpdf(to_vector(L_AR1_d) | ar_prior[1], ar_prior[2], ar1_down, ar1_up - ar1_down);
        }

      } else if(time_proc == 4) {

        to_vector(time_var_gp_free_d) ~ exponential(gp_rho);
        to_vector(m_sd_free_d) ~ exponential(gp_alpha);

        for(n in 1:num_legis) {
          for(d in 1:Dm) {
            vector[T] x;
            matrix[T, T] L_cov = cholesky_decompose(gp_exp_quad_cov(time_ind, m_sd_free_d[n, d], time_var_gp_free_d[n, d]) +
                                                    diag_matrix(rep_vector(gp_nugget, T)));
            for(t in 1:T) x[t] = L_tp1_var_d[t][n, d];
            x ~ multi_normal_cholesky(rep_vector(L_full_d[n, d], T), L_cov);
          }
        }

      } else if(time_proc == 5) {

        for(n in 1:num_legis) {
          for(d in 1:Dm) a_raw_d[n, d] ~ normal(0, legis_sd);
        }

      }

    }

  }

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
                     trials,
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
        ar_prior,
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
        gp_alpha,
        ar1_down,
        ar1_up,
        D,
        restrict_dim,
        L_full_d,
        L_tp1_d,
        sigma_reg_d,
        sigma_abs_d,
        legis_x_d,
        sigma_reg_x_d,
        sigma_abs_x_d);
        
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

