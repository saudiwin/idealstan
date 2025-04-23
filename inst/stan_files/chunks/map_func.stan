// mapped likelihood function

real partial_sum(array[,] int y_slice,
                 int start, int end,
                 int T,
                 int pos_discrim,
                 int gp_N,
                 int num_legis,
                 array[] int Y_int,
                 array[] real Y_cont,
                 int y_int_miss,
                 real y_cont_miss,
                 int S_type,
                 matrix srx_pred,
                 matrix sax_pred,
                 matrix legis_pred,
                 array[] int bb,
                 array[] int ll,
                 array[] int time,
                 array[] int mm,
                 array[] real time_ind,
                 array[] int n_cats_rat,
                 int mod_count, // total number of models
                 int tot_cats, // total number of possible ordinal outcomes
                 array[] int n_cats_grm,
                 array[] int order_cats_rat,
                 array[] int order_cats_grm,
                 int const_type,
                 int num_restrict_high,
                 int num_restrict_low,
                 array[] int restrict_high,
                 array[] int restrict_low,
                 int center_cutoff,
                 array[] real fix_high,
                 array[] real fix_low,
                 array[] real restrict_sd_high,
                 array[] real restrict_sd_low,
                 real discrim_reg_upb,
                 real discrim_reg_lb,
                 real discrim_miss_upb,
                 real discrim_miss_lb,
                 real discrim_reg_scale,
                 real discrim_reg_shape,
                 real discrim_abs_scale,
                 real discrim_abs_shape,
                 real legis_sd,
                 real diff_abs_sd,
                 real diff_reg_sd,
                 vector ar_prior,
                 real time_sd,
                 real time_var_sd,
                 int time_proc,
                 int zeroes, // whether to use traditional zero-inflation for bernoulli and poisson models
                 vector sigma_abs_free,
                 vector L_full, // first T=1 params to constrain
                 vector m_sd_free, // marginal standard deviation of GP
                 real gp_nugget, // residual variation in GP
                 vector ls_int, // extra intercepts for non-inflated latent space
                 vector ls_int_abs, // extra intercepts for non-inflated latent space
                 array[] vector L_tp1_var, // non-centered variance
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
                 array[] vector steps_votes_grm3,
                 array[] vector steps_votes_grm4,
                 array[] vector steps_votes_grm5,
                 array[] vector steps_votes_grm6,
                 array[] vector steps_votes_grm7,
                 array[] vector steps_votes_grm8,
                 array[] vector steps_votes_grm9,
                 array[] vector steps_votes_grm10,
                 vector extra_sd,
                 vector time_var_gp_free,
                 array[] vector L_tp1,
                 vector time_var_free,
                 real inv_gamma_beta,
                 int het_var,
                 array[] int type_het_var,
                 int restrict_var,
                 int ignore,
                 array[,] int ignore_mat,
                 int num_basis,
                 array[] row_vector a_raw,
                 matrix B,
                 int prior_only,
                 array[] real restrict_N_high,
                 array[] real restrict_N_low,
                 int debug_mode,
                 array[] int ordbeta_id,
                 vector phi,
                 array[] vector ordbeta_cut,
                 real gp_rho,
                 real gp_alpha,
                 real ar1_down,
                 real ar1_up) {
  
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
    row_vector[num_basis] a;
    
    s = y_slice[r,1];
    start2 = y_slice[r,2];
    end2 = y_slice[r,3];
    
    // create covariates
    // depends on whether persons or items are mapped over
    // use conditional operator to determine size of vectors
    
    // ID parameters
    
    int to_fix_high = r_in(s, restrict_high);
    int to_fix_low = r_in(s, restrict_low);
    

    if(const_type == 1 && S_type == 1) {
        if(pos_discrim == 0) {
          
            if(to_fix_high>0) {
                if(time_proc == 2) {
                    real term = normal_lpdf(L_tp1_var[1, s] | fix_high[to_fix_high], restrict_sd_high);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_tp1_var[1,s] | fix_high, restrict_sd_high) to log_prob: ", term);
                    
                    term = normal_lpdf(L_full[s] | 0, legis_sd);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_full[s] | 0, legis_sd) to log_prob: ", term);
                } else {
                    real term = normal_lpdf(L_full[s] | fix_high[to_fix_high], restrict_sd_high);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_full[s] | fix_high, restrict_sd_high) to log_prob: ", term);
                }
            } else if(to_fix_low>0) {
                if(time_proc == 2) {
                    real term = normal_lpdf(L_tp1_var[1, s] | fix_low[to_fix_low], restrict_sd_low);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_tp1_var[1, s] | fix_low, restrict_sd_low) to log_prob: ", term);
                    
                    term = normal_lpdf(L_full[s] | 0, legis_sd);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_full[s] | 0, legis_sd) to log_prob: ", term);
                } else {
                    real term = normal_lpdf(L_full[s] | fix_low[to_fix_low], restrict_sd_low);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_full[s] | fix_low, restrict_sd_low) to log_prob: ", term);
                }
            } else {
                if(time_proc == 2) {
                    real term = normal_lpdf(L_tp1_var[1, s] | 0, legis_sd);
                    log_prob += term;
                    if(debug_mode==2) print("Added normal_lpdf(L_tp1_var[1, s] | 0, legis_sd) to log_prob: ", term);
                }
                real term = normal_lpdf(L_full[s] | 0, legis_sd);
                log_prob += term;
                if(debug_mode==2) print("Added normal_lpdf(L_full[s] | 0, legis_sd) to log_prob: ", term);
            }
        } else {
            real term = normal_lpdf(L_full[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(L_full[s] | 0, legis_sd) to log_prob: ", term);
        }
        if(rows(ls_int) > 0) {
            real term = normal_lpdf(ls_int[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(ls_int[s] | 0, legis_sd) to log_prob: ", term);
            
            term = normal_lpdf(ls_int_abs[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(ls_int_abs[s] | 0, legis_sd) to log_prob: ", term);
        }
    } else if(S_type == 0 && const_type == 2) {
        if(pos_discrim == 0) {
          
          int num_high = num_elements(restrict_high);
          int num_low = num_elements(restrict_low);
          int count_high = 1;
          int count_low = 1;
          
            if(r_in(s, restrict_high)) {
                real term = genbeta_lpdf(sigma_reg_full[s] | restrict_N_high[count_high], restrict_sd_high[count_high], discrim_reg_lb, discrim_reg_upb);
                count_high = count_high + 1;
                log_prob += term;
                if(debug_mode==2) print("Added genbeta_lpdf(sigma_reg_full[s] | restrict_N_high, restrict_sd_high, discrim_reg_lb, discrim_reg_upb) to log_prob: ", term);
            } else if(r_in(s, restrict_low)) {
                real term = genbeta_lpdf(sigma_reg_full[s] | restrict_sd_low[count_low], restrict_N_low[count_low], discrim_reg_lb, discrim_reg_upb);
                count_low = count_low + 1;
                log_prob += term;
                if(debug_mode==2) print("Added genbeta_lpdf(sigma_reg_full[s] | restrict_sd_low, restrict_N_low, discrim_reg_lb, discrim_reg_upb) to log_prob: ", term);
            } else {
                real term = genbeta_lpdf(sigma_reg_full[s] | discrim_reg_scale, discrim_reg_shape, discrim_reg_lb, discrim_reg_upb);
                log_prob += term;
                if(debug_mode==2) print("Added genbeta_lpdf(sigma_reg_full[s] | discrim_reg_scale, discrim_reg_shape, discrim_reg_lb, discrim_reg_upb) to log_prob: ", term);
            }
        } else {
            real term = exponential_lpdf(sigma_reg_full[s] | 1 / discrim_reg_scale);
            log_prob += term;
            if(debug_mode==2) print("Added exponential_lpdf(sigma_reg_full[s] | 1 / discrim_reg_scale) to log_prob: ", term);
        }
        real term = normal_lpdf(B_int_free[s] | 0, diff_reg_sd);
        log_prob += term;
        if(debug_mode==2) print("Added normal_lpdf(B_int_free[s] | 0, diff_reg_sd) to log_prob: ", term);
        
        term = genbeta_lpdf(sigma_abs_free[s] | discrim_abs_scale, discrim_abs_shape, discrim_miss_lb, discrim_miss_upb);
        log_prob += term;
        if(debug_mode==2) print("Added genbeta_lpdf(sigma_abs_free[s] | discrim_abs_scale, discrim_abs_shape, discrim_miss_lb, discrim_miss_upb) to log_prob: ", term);
        
        term = normal_lpdf(A_int_free[s] | 0, diff_abs_sd);
        log_prob += term;
        if(debug_mode==2) print("Added normal_lpdf(A_int_free[s] | 0, diff_abs_sd) to log_prob: ", term);
    } else if(S_type == 0 && const_type == 1) {
        if(pos_discrim == 0) {
            real term = genbeta_lpdf(sigma_reg_full[s] | discrim_reg_scale, discrim_reg_shape, discrim_reg_lb, discrim_reg_upb);
            log_prob += term;
            if(debug_mode==2) print("Added genbeta_lpdf(sigma_reg_full[s] | discrim_reg_scale, discrim_reg_shape, discrim_reg_lb, discrim_reg_upb) to log_prob: ", term);
        } else {
            real term = exponential_lpdf(sigma_reg_full[s] | 1 / discrim_reg_scale);
            log_prob += term;
            if(debug_mode==2) print("Added exponential_lpdf(sigma_reg_full[s] | 1 / discrim_reg_scale) to log_prob: ", term);
        }
        real term = genbeta_lpdf(sigma_abs_free[s] | discrim_abs_scale, discrim_abs_shape, discrim_miss_lb, discrim_miss_upb);
        log_prob += term;
        if(debug_mode==2) print("Added genbeta_lpdf(sigma_abs_free[s] | discrim_abs_scale, discrim_abs_shape, discrim_miss_lb, discrim_miss_upb) to log_prob: ", term);
        
        term = normal_lpdf(B_int_free[s] | 0, diff_reg_sd);
        log_prob += term;
        if(debug_mode==2) print("Added normal_lpdf(B_int_free[s] | 0, diff_reg_sd) to log_prob: ", term);
        
        term = normal_lpdf(A_int_free[s] | 0, diff_abs_sd);
        log_prob += term;
        if(debug_mode==2) print("Added normal_lpdf(A_int_free[s] | 0, diff_abs_sd) to log_prob: ", term);
    } else if(const_type == 2 && S_type == 1) {
        real term = normal_lpdf(L_full[s] | 0, legis_sd);
        log_prob += term;
        if(debug_mode==2) print("Added normal_lpdf(L_full[s] | 0, legis_sd) to log_prob: ", term);
        
        if(rows(ls_int) > 0) {
            term = normal_lpdf(ls_int[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(ls_int[s] | 0, legis_sd) to log_prob: ", term);
            
            term = normal_lpdf(ls_int_abs[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(ls_int_abs[s] | 0, legis_sd) to log_prob: ", term);
        }
    }

    if(S_type == 1 && T > 1) {
        if(time_proc != 4) {
            if(time_proc == 3 || (time_proc == 2 && const_type == 2)) {
                real term = normal_lpdf(L_tp1_var[1, s] | 0, legis_sd);
                log_prob += term;
                if(debug_mode==2) print("Added normal_lpdf(L_tp1_var[1, s] | 0, legis_sd) to log_prob: ", term);
            }
        }
        if(T < center_cutoff && (time_proc < 4 && time_proc>1)) {
            real term = std_normal_lpdf(to_vector(L_tp1_var[2:T, s]));
            log_prob += term;
            if(debug_mode==2) print("Added std_normal_lpdf(to_vector(L_tp1_var[2:T, s])) to log_prob: ", term);
        }

        if(restrict_var == 1 && time_proc != 4) {
            if(s > 1) {
                if(inv_gamma_beta > 0) {
                    real term = inv_gamma_lpdf(time_var_free[s - 1] | 2, inv_gamma_beta);
                    log_prob += term;
                    if(debug_mode==2) print("Added inv_gamma_lpdf(time_var_free[s - 1] | 2, inv_gamma_beta) to log_prob: ", term);
                } else {
                    real term = exponential_lpdf(time_var_free[s - 1] | time_var_sd);
                    log_prob += term;
                    if(debug_mode==2) print("Added exponential_lpdf(time_var_free[s - 1] | time_var_sd) to log_prob: ", term);
                }
            }
        } else if(time_proc != 4) {
            if(inv_gamma_beta > 0) {
                real term = inv_gamma_lpdf(time_var_free[s] | 2, inv_gamma_beta);
                log_prob += term;
                if(debug_mode==2) print("Added inv_gamma_lpdf(time_var_free[s] | 2, inv_gamma_beta) to log_prob: ", term);
            } else {
                real term = exponential_lpdf(time_var_free[s] | time_var_sd);
                log_prob += term;
                if(debug_mode==2) print("Added exponential_lpdf(time_var_free[s] | time_var_sd) to log_prob: ", term);
            }
        }
        
        if(time_proc == 5) {
            real term = normal_lpdf(a_raw[s] | 0, legis_sd);
            log_prob += term;
            if(debug_mode==2) print("Added normal_lpdf(a_raw[s] | 0, legis_sd) to log_prob: ", term);
            if(restrict_var == 1) {
                if(s == 1) {
                    a = a_raw[s] * time_sd;
                } else {
                    a = a_raw[s] * time_var_free[s - 1];
                }
            } else {
                a = a_raw[s] * time_var_free[s];
            }
        }

        if(time_proc == 3) {
            real term = genbeta_lpdf(L_AR1[s] | ar_prior[1], ar_prior[2], ar1_down, ar1_up - ar1_down);
            log_prob += term;
            if(debug_mode==2) print("Added genbeta_lpdf(L_AR1[s] | ar_prior[1], ar_prior[2]) to log_prob: ", term);
#include /chunks/l_hier_ar1_prior_map.stan
        } else if(time_proc == 2) {
            
#include /chunks/l_hier_prior_map.stan
        } else if(time_proc == 4) {
              //calculated values for GPs
  
            matrix[T, T] cov; // zero-length if not a GP model
            matrix[T, T] L_cov;// zero-length if not a GP model
            
            //real term = inv_gamma_lpdf(time_var_gp_free[s] | 5, 5);
            real term = exponential_lpdf(time_var_gp_free[s]|gp_rho);
            
            log_prob += term;
            
            if(debug_mode==2) print("Added inv_gamma_lpdf(time_var_gp_free[s] | 5, 5) to log_prob: ", term);
            
            term = exponential_lpdf(m_sd_free[s] | gp_alpha);
            log_prob += term;
            
            if(debug_mode==2) print("Added exponential_lpdf(m_sd_free[s - 1] | 1) to log_prob: ", term);

          // chunk giving a GP prior to legislators/persons
            //create covariance matrices given current values of hiearchical parameters
            // if(s==1) {
            //   //cov =   gp_exp_quad_cov(time_ind, m_sd_par, gp_length[1]) + diag_matrix(rep_vector(square(gp_sd_par),T));
            //   cov =   gp_exp_quad_cov(time_ind, m_sd_par, time_var_gp_free[s]) + diag_matrix(rep_vector(gp_sd_free[1],T));
            //   if(debug_mode==2) {
            //     term = sum(gp_exp_quad_cov(time_ind, m_sd_par, time_var_gp_free[s]) + diag_matrix(rep_vector(gp_sd_free[1],T)));
            //     print("Added exponential_lpdf(m_sd_free[s - 1] | 1) to log_prob: ", term);
            //   }
            // } else {
              
              if(debug_mode==2) {
                term = sum(gp_exp_quad_cov(time_ind, m_sd_free[s], time_var_gp_free[s]) + diag_matrix(rep_vector(gp_nugget,T)));
                print("Calculated gp_exp_quad_cov(time_ind, m_sd_par, time_var_gp_free[s]) + diag_matrix(rep_vector(gp_sd_free[1],T)): ", term);
              }
              
              cov =   gp_exp_quad_cov(time_ind, m_sd_free[s], time_var_gp_free[s]) + diag_matrix(rep_vector(gp_nugget,T));

            L_cov = cholesky_decompose(cov);
            
            if(debug_mode==2) {
                term = multi_normal_cholesky_lpdf(to_vector(L_tp1_var[,s])|rep_vector(0,T) + L_full[s], L_cov);
                print("Added multi_normal_cholesky_lpdf(to_vector(L_tp1_var[,s])|rep_vector(0,T) + L_full[s], L_cov) to log_prob: ", term);
              }
              
              // + L_full[s]

            log_prob += multi_normal_cholesky_lpdf(to_vector(L_tp1_var[,s])|rep_vector(0,T) + L_full[s], L_cov);
            //log_prob += normal_lpdf(to_vector(L_tp1_var[,s])|0, 1);

        lt = to_vector(L_tp1_var[,s]);
        
        } else if(time_proc==5) {
          
          //splines
          
#include /chunks/l_splines_map.stan         
          
          
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

if(prior_only==0) {
  
#include /chunks/model_types_mm.stan
    
  }
      
    }


  
  return log_prob;
}
