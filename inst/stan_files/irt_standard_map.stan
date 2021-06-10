
#include /chunks/license.stan

functions {
#include /chunks/stationary_functions.stan
#include /chunks/jacobians.stan
#include /chunks/calc_rlnorm_gp.stan
#include /chunks/id_params.stan
// #include /chunks/r_in.stan
int r_in(int pos,int[] pos_var) {
  
  for (p in 1:(size(pos_var))) {
    if (pos_var[p]==pos) {
      // can return immediately, as soon as find a match
      return 1;
    }
  }
  return 0;
}


real partial_sum(int[,] y_slice,
        int start, int end,
        int T,
        int pos_discrim,
        int gp_N,
        int num_legis,
        int[] Y_int,
        real[] Y_cont,
        int y_int_miss,
        real y_cont_miss,
        int S_type,
        matrix srx_pred,
        matrix sax_pred,
        matrix legis_pred,
        int[] bb,
        int[] ll,
        int[] time,
        int[] mm,
        real[] time_ind,
        int[] n_cats_rat,
        int mod_count, // total number of models
        int tot_cats, // total number of possible ordinal outcomes
        int[] n_cats_grm,
        int[] order_cats_rat,
        int[] order_cats_grm,
        int const_type,
        int restrict_high,
        int restrict_low,
        int center_cutoff,
        real fix_high,
        real fix_low,
        real restrict_sd_high,
        real restrict_sd_low,
        real discrim_reg_sd,
        real discrim_abs_sd,
        real legis_sd,
        real diff_abs_sd,
        real diff_reg_sd,
        real ar_sd,
        real time_sd,
        int time_proc,
        int zeroes, // whether to use traditional zero-inflation for bernoulli and poisson models
        real gp_sd_par, // residual variation in GP
        real num_diff, // number of time points used to calculate GP length-scale prior
        real m_sd_par, // the marginal standard deviation of the GP
        int min_length, // the minimum threshold for GP length-scale prior,
        vector sigma_abs_free,
        vector L_full, // first T=1 params to constrain
        vector m_sd_free, // marginal standard deviation of GP
        vector gp_sd_free, // residual GP variation in Y
        vector ls_int, // extra intercepts for non-inflated latent space
        vector ls_int_abs, // extra intercepts for non-inflated latent space
        vector[] L_tp1_var, // non-centered variance
        vector L_AR1, // AR-1 parameters for AR-1 model
        vector sigma_reg_full,
        vector legis_x,
        vector sigma_reg_x,
        vector sigma_abs_x,
        vector B_int_free,
        vector A_int_free,
        vector steps_votes3,
        vector steps_votes4,
        vector steps_votes5,
        vector steps_votes6,
        vector steps_votes7,
        vector steps_votes8,
        vector steps_votes9,
        vector steps_votes10,
        vector[] steps_votes_grm3,
        vector[] steps_votes_grm4,
        vector[] steps_votes_grm5,
        vector[] steps_votes_grm6,
        vector[] steps_votes_grm7,
        vector[] steps_votes_grm8,
        vector[] steps_votes_grm9,
        vector[] steps_votes_grm10,
        vector extra_sd,
        vector time_var_gp_free,
        vector[] L_tp1,
        vector time_var_free,
        real inv_gamma_beta,
        vector gp_length,
        int het_var,
        int[] type_het_var,
        int restrict_var) {
  
  // big loop over states
  real log_prob = 0;
  vector[T>1 ? T : 0] lt; // time-varying person parameter if mapped over persons
  
  for(r in 1:size(y_slice)) {
    
    int s;
    int start2;
    int end2;
    int this_var = 1;
    
    vector[y_slice[r,3] - y_slice[r,2] + 1] legis_calc; // store calculations for hierarchical covariates
    vector[y_slice[r,3] - y_slice[r,2] + 1] sigma_reg_calc;
    vector[y_slice[r,3] - y_slice[r,2] + 1] sigma_abs_calc;

    s = y_slice[r,1];
    start2 = y_slice[r,2];
    end2 = y_slice[r,3];
    
    

    // create covariates
    // depends on whether persons or items are mapped over
    // use conditional operator to determine size of vectors

    // ID parameters

    if(const_type==1 && S_type==1) {
      // determine whether mapped parameter is restricted
        if(pos_discrim==0) {
          if(s==restrict_high) {
            if(time_proc==2) {
              //centered
              log_prob += normal_lpdf(L_tp1_var[1,s]|fix_high,restrict_sd_high);
              log_prob += normal_lpdf(L_full[s]|0,legis_sd);
            } else {
              log_prob += normal_lpdf(L_full[s]|fix_high,restrict_sd_high);
            }
          } else if(s==restrict_low) {
            if(time_proc==2) {
              //centered
              log_prob += normal_lpdf(L_tp1_var[1,s]|fix_low,restrict_sd_low);
              log_prob += normal_lpdf(L_full[s]|0,legis_sd);
            } else {
              log_prob += normal_lpdf(L_full[s]|fix_low,restrict_sd_low);
            }

          } else {
            
            if(time_proc==2) {
              log_prob += normal_lpdf(L_tp1_var[1,s]|0,legis_sd);
            } 
            
              log_prob += normal_lpdf(L_full[s]|0,legis_sd);

            
          }
        } else {
          log_prob += normal_lpdf(L_full[s]|0,legis_sd);
        }
        
        if(rows(ls_int)>0) {
          
          log_prob += normal_lpdf(ls_int[s]|0,legis_sd);
          log_prob += normal_lpdf(ls_int_abs[s]|0,legis_sd);
          
        }
        
    } else if(S_type==0 && const_type==2) {
      
      if(pos_discrim==0) {
        if(s==restrict_high) {
          log_prob += normal_lpdf(sigma_reg_full[s]|fix_high,restrict_sd_high);
        } else if(s==restrict_low) {
          log_prob += normal_lpdf(sigma_reg_full[s]|fix_low,restrict_sd_low);
        } else {
          log_prob += normal_lpdf(sigma_reg_full[s]|0,discrim_reg_sd);
        }
      } else {
        log_prob += exponential_lpdf(sigma_reg_full[s]|1/discrim_reg_sd);
      }
      
      log_prob += normal_lpdf(B_int_free[s]|0,diff_reg_sd);
      log_prob += normal_lpdf(sigma_abs_free[s]|0,discrim_abs_sd);
      log_prob += normal_lpdf(A_int_free[s]|0,diff_abs_sd);

    } else if(S_type==0 && const_type==1) {

      // priors if items
      if(pos_discrim==0) { 
        
        log_prob += normal_lpdf(sigma_reg_full[s]|0,discrim_reg_sd);
        
      } else {
        
        log_prob += exponential_lpdf(sigma_reg_full[s]|1/discrim_reg_sd);
        
      }
      
      log_prob += normal_lpdf(sigma_abs_free[s]|0,discrim_abs_sd);
      log_prob += normal_lpdf(B_int_free[s]|0,diff_reg_sd);
      log_prob += normal_lpdf(A_int_free[s]|0,diff_abs_sd);

    } else if(const_type==2 && S_type==1) {
      
      log_prob += normal_lpdf(L_full[s]|0,legis_sd);
      
      if(rows(ls_int)>0) {
          
          log_prob += normal_lpdf(ls_int[s]|0,legis_sd);
          log_prob += normal_lpdf(ls_int_abs[s]|0,legis_sd);
          
      }
      
    }

    if(S_type==1 && T>1) {

      // priors for time-varying persons

      if(time_proc!=4) {

        if(time_proc==3) {
          log_prob += normal_lpdf(L_tp1_var[1,s]|0,legis_sd);
        }

        if(T<center_cutoff) {

          log_prob += normal_lpdf(to_vector(L_tp1_var[2:T,s])|0,1);

        }

        if(restrict_var==1) {

          if(s>1) {

            if(inv_gamma_beta>0) {

              log_prob += inv_gamma_lpdf(time_var_free[s-1]|2,inv_gamma_beta); // boundary-avoiding prior

            } else {

              log_prob += exponential_lpdf(time_var_free[s-1]|.1); // tight-ish prior on additional variances

            }

          }

        } else {

          if(inv_gamma_beta>0) {

              log_prob += inv_gamma_lpdf(time_var_free[s]|2,inv_gamma_beta); // boundary-avoiding prior

            } else {

              log_prob += exponential_lpdf(time_var_free[s]|.1); // tight-ish prior on additional variances

            }

        }

      }


      if(time_proc==3) {
        log_prob += normal_lpdf(L_AR1[s]|0,ar_sd);
#include /chunks/l_hier_ar1_prior_map.stan
      } else if(time_proc==2) {
#include /chunks/l_hier_prior_map.stan
      } else if(time_proc==4) {

            log_prob += inv_gamma_lpdf(time_var_gp_free[s]|5,5);

        if(s>1) {
            log_prob += exponential_lpdf(m_sd_free[s-1]|1);
        }


        {
          matrix[T, T] cov; // zero-length if not a GP model
          matrix[T, T] L_cov;// zero-length if not a GP model
          real time_ind_center[T];

          for(t in 1:T)
            time_ind_center[t] = time_ind[t] - mean(time_ind);

          // chunk giving a GP prior to legislators/persons
            //create covariance matrices given current values of hiearchical parameters
            if(s==1) {
              //cov =   cov_exp_quad(time_ind, m_sd_par, gp_length[1]) + diag_matrix(rep_vector(square(gp_sd_par),T));
              cov =   cov_exp_quad(time_ind_center, m_sd_par, time_var_gp_free[s]) + diag_matrix(rep_vector(gp_sd_free[1],T));
            } else {
              cov =   cov_exp_quad(time_ind_center, m_sd_free[s-1], time_var_gp_free[s]) + diag_matrix(rep_vector(gp_sd_free[1],T));
            }

            L_cov = cholesky_decompose(cov);

            log_prob += multi_normal_cholesky_lpdf(to_vector(L_tp1_var[,s])|rep_vector(0,T) + L_full[s], L_cov);


          }

          lt = to_vector(L_tp1_var[,s]);
        }


    }

    // do calculations
    if(cols(legis_pred)>0) {
      legis_calc = legis_pred[start2:end2,]*legis_x;
    } else {
      legis_calc = rep_vector(0.0,end2 - start2 + 1);
    }

    if(cols(srx_pred)>0) {
      sigma_reg_calc = srx_pred[start2:end2,]*sigma_reg_x;
    } else {
      sigma_reg_calc = rep_vector(0.0,end2 - start2 + 1);
    }

    if(cols(sax_pred)>0) {
      sigma_abs_calc = sax_pred[start2:end2,]*sigma_abs_x;
    } else {
      sigma_abs_calc = rep_vector(0.0,end2 - start2 + 1);
    }

    if(S_type==1) {
      
#include /chunks/model_types_mm_map_persons.stan

    } else {
  
#include /chunks/model_types_mm_map_items.stan

    }
  }
  
  return log_prob;

}

}

data {
  int N; // total number of observations
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T; // number of time points
  int grainsize;
  int Y_int[N_int]; // integer outcome
  real Y_cont[N_cont]; // continuous outcome
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
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real<lower=0> restrict_sd_high;
  real<lower=0> restrict_sd_low;
  real ar_sd;
  real time_sd;
  real inv_gamma_beta;
  int<lower=2> center_cutoff;
  int restrict_var; // whether to fix the over-time variance of the first person to a value
  int sum_vals[S,3]; // what to loop over for reduce sum
  int time_proc;
  real time_ind[T]; // the actual indices/values of time points, used for Gaussian processes
  int zeroes; // whether to use traditional zero-inflation for bernoulli and poisson models
  real gp_sd_par; // residual variation in GP
  real num_diff; // number of time points used to calculate GP length-scale prior
  real m_sd_par; // the marginal standard deviation of the GP
  int min_length; // the minimum threshold for GP length-scale prior
  int num_var;
  int type_het_var[num_bills];
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

  num_legis_real = num_legis; // promote N to type real
  
  
}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis] L_full; // first T=1 params to constrain
  vector<lower=0>[gp_N_fix] m_sd_free; // marginal standard deviation of GP
  vector<lower=0>[time_proc==4 ? 1 : 0] gp_sd_free; // residual GP variation in Y
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[num_ls] ls_int_abs; // extra intercepts for non-inflated latent space
  vector[T>1 ? num_legis : 0] L_tp1_var[T]; // non-centered variance
  vector<lower=-.99,upper=.99>[(T>1 && time_proc==3) ? num_legis : 0] L_AR1; // AR-1 parameters for AR-1 model
  vector[pos_discrim == 0 ? num_bills : 0] sigma_reg_free;
  vector<lower=0>[pos_discrim == 1 ? num_bills : 0] sigma_reg_pos;
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
  vector<lower=0>[num_var] extra_sd;
  vector<lower=0>[gp_N] time_var_gp_free;
  vector<lower=0>[(T>1 && time_proc!=4 && restrict_var==1) ? num_legis-1 : (T>1 && time_proc!=4 && restrict_var==0 ? num_legis : 0)] time_var_free;
  
}

transformed parameters {

  vector[(T>1 && S_type==0) ? num_legis : 0] L_tp1[T];
  vector[num_bills] sigma_reg_full;
  vector[(T>1 && S_type==0 && time_proc!=4) ? num_legis : 0] time_var_full;
  //vector[S_type==0 ? gp_N : 0] time_var_gp_full;
  vector[S_type==0 ? gp_N : 0] m_sd_full;
  //vector[S_type==0 ? gp_N : 0] gp_sd_full;
  
  if(T>1 && time_proc!=4 && S_type==0) {
    time_var_full = append_row([time_sd]',time_var_free);
  }
  
  if(pos_discrim==0) {
    sigma_reg_full = sigma_reg_free;
  } else {
    sigma_reg_full = sigma_reg_pos;
  }
  
    //combine constrained and unconstrained parameters
    //#include /chunks/build_params_v2.stan

  if(T>1 && S_type==0) {
    if(time_proc==3) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan
    } else if(time_proc==2){
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    } else if(time_proc==4) {
      m_sd_full = append_row([m_sd_par]',
                              m_sd_free);
      // gp_sd_full = append_row([gp_sd_par]',
      //                         gp_sd_free);
      // time_var_gp_full = append_row(gp_length,
      //                                 time_var_gp_free);
      
      L_tp1 = L_tp1_var;
      
    }  
  } 


}

model {
  
  legis_x ~ normal(0,5);
  sigma_abs_x ~ normal(0,5);
  sigma_reg_x ~ normal(0,5);
  extra_sd ~ exponential(1);
  
  if(time_proc==3 && S_type==0 && T>1) {
    L_AR1 ~ normal(0,ar_sd);
  }
  

#include /chunks/ord_steps_calc.stan  
  
  if(time_proc==4 && S_type==0 && T>1)  {
    {
    matrix[T, T] cov[gp_N]; // zero-length if not a GP model
    matrix[T, T] L_cov[gp_N];// zero-length if not a GP model
// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   cov_exp_quad(time_ind, m_sd_full[n], time_var_free[n])
      + diag_matrix(rep_vector(gp_sd_free[1],T));
  L_cov[n] = cholesky_decompose(cov[n]);

  to_vector(L_tp1_var[1:T,n]) ~ multi_normal_cholesky(rep_vector(0,T) + L_full[n], L_cov[n]); 
  
    
}
    }
  }
  
  
  if(T>1 && time_proc!=4 && S_type==0) {
    
    if(time_proc!=2) {
      L_tp1_var[1] ~ normal(0,5);
    }
    
    if(T<center_cutoff) {
    
      for(t in 2:T) {
        L_tp1_var[t] ~ normal(0,1);
      }
      
      
    } else {
      
      if(time_proc==2) {
        
        if(restrict_var==1) {
          
          for(n in 1:num_legis) 
              L_tp1_var[2:T,n] ~ normal(L_tp1_var[1:(T-1),n],time_var_full[n]);

            
          } else {
          
          for(n in 1:num_legis)
                L_tp1_var[2:T,n] ~ normal(L_tp1_var[1:(T-1),n],time_var_free[n]);
          
        }
        
      } else if(time_proc==3) {
        
        if(restrict_var==1) {
          
          for(n in 1:num_legis) 
              L_tp1_var[2:T,n] ~ normal(L_full[n] + L_AR1[n]*to_vector(L_tp1_var[1:(T-1),n]),time_var_full[n]);
                
          
        } else {
          
          for(n in 1:num_legis)
                L_tp1_var[2:T,n] ~ normal(L_full[n] + L_AR1[n]*to_vector(L_tp1_var[1:(T-1),n]),time_var_free[n]);
          
        }
        
        
      }
      
    }
    
    
  }
  
  if(S_type==0)  {
    ls_int ~ normal(0,legis_sd);
    ls_int_abs ~ normal(0,legis_sd);
  }
  
  gp_sd_free ~ exponential(1); // length 1
  
  if(T>1 && S_type==0) {
    
    if(inv_gamma_beta>0) {
      
      time_var_free ~ inv_gamma(2,inv_gamma_beta);
        
    } else {
      
      time_var_free ~ exponential(.1);    
        
    }
    time_var_gp_free ~ inv_gamma(5,5); // tight-ish prior on additional variances
    m_sd_free ~ exponential(1);
  }

  //priors for parameters not included in reduce_sum
  
if(S_type==1 && const_type==1) {
  // both ID and map for persons
  sigma_reg_free ~ normal(0, discrim_reg_sd);
  sigma_reg_pos ~ exponential(1/discrim_reg_sd);
  sigma_abs_free ~ normal(0,discrim_abs_sd);
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
} else if(S_type==1 && const_type==2) {
  // map persons, ID items
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
  sigma_abs_free ~ normal(0,discrim_abs_sd);
  
  if(pos_discrim==0) {
    target += id_params(sigma_reg_free,
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        restrict_sd_high,
                        restrict_sd_low,
                        0,
                        discrim_reg_sd);
  } else {
    sigma_reg_pos ~ exponential(1/discrim_reg_sd);
  }
  
  
  
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
        restrict_high,
        restrict_low,
        center_cutoff,
        fix_high,
        fix_low,
        restrict_sd_high,
        restrict_sd_low,
        discrim_reg_sd,
        discrim_abs_sd,
        legis_sd,
        diff_abs_sd,
        diff_reg_sd,
        ar_sd,
        time_sd,
        time_proc,
        zeroes, // whether to use traditional zero-inflation for bernoulli and poisson models
        gp_sd_par, // residual variation in GP
        num_diff, // number of time points used to calculate GP length-scale prior
        m_sd_par, // the marginal standard deviation of the GP
        min_length, // the minimum threshold for GP length-scale prior,
        sigma_abs_free,
        L_full, // first T=1 params to constrain
        m_sd_free, // marginal standard deviation of GP
        gp_sd_free, // residual GP variation in Y
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
        gp_length,
        num_var,
        type_het_var,
        restrict_var);

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

