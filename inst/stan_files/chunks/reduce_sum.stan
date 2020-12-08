
real partial_sum(int[,] y_slice,
        int start, int end,
        int T,
        int gp_N,
        int num_legis,
        int[] Y_int,
        int[] Y_cont,
        int S_type,
        matrix srx_pred,
        matrix sax_pred,
        matrix legis_pred,
        int[] bb,
        int[] ll,
        int[] time,
        int[] mm,
        real[] time_ind,
        int n_cats_rat,
        int mod_count, // total number of models
        int tot_cats, // total number of possible ordinal outcomes
        int[] n_cats_grm,
        int[] order_cats_rat,
        int[] order_cats_grm,
        int const_type,
        int restrict_high,
        int restrict_low,
        real fix_high,
        real fix_low,
        real sd_fix,
        real restrict_sd,
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
        vector[] L_tp2, // additional L_tp1 for GPs only
        vector ls_int, // extra intercepts for non-inflated latent space
        vector[] L_tp1_var, // non-centered variance
        vector L_AR1, // AR-1 parameters for AR-1 model
        vector sigma_reg_free,
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
        real extra_sd,
        vector time_var_gp_full,
        vector m_sd_full,
        vector gp_sd_full,
        vector time_var_full,
        vector[] L_tp1) {
  
  // big loop over states
  real log_prob = 0;
  
  for(r in 1:size(y_slice)) {
    
    int s;
    int start2;
    int end2;

    s = y_slice[r,1];
    start2 = y_slice[r,2];
    end2 = y_slice[r,3];

    // create covariates
    // depends on whether persons or items are mapped over
    // use conditional operator to determine size of vectors

    // ID parameters

    if(const_type==1 && S_type==1) {
      // determine whether mapped parameter is restricted

      if(s==restrict_high) {
        log_prob += normal_lpdf(L_full[s]|fix_high,restrict_sd);
      } else if(s==restrict_low) {
        log_prob += normal_lpdf(L_full[s]|fix_low,restrict_sd);
      } else {
        log_prob += normal_lpdf(L_full[s]|0,legis_sd);
      }


    } else if(S_type==0 && const_type==2) {

      if(s==restrict_high) {
        log_prob += normal_lpdf(sigma_reg_free[s]|fix_high,restrict_sd);
      } else if(s==restrict_low) {
        log_prob += normal_lpdf(sigma_reg_free[s]|fix_low,restrict_sd);
      } else {
        log_prob += normal_lpdf(sigma_reg_free[s]|0,discrim_reg_sd);
      }


    }

    if(S_type==1) {

      // priors if persons

      log_prob += normal_lpdf(time_var_free[s]|0,1); // tight-ish prior on additional variances
      log_prob += normal_lpdf(time_var_gp_free|0,1); // tight-ish prior on additional variances
      log_prob += normal_lpdf(gp_sd_free|0,2);
      log_prob += normal_lpdf(m_sd_free|0,2);

      if(time_proc==3 && T>1) {
        log_prob += normal_lpdf(L_AR1|0,ar_sd);
      }

      if(T>1 && time_proc!=4) {
        for(t in 1:(T-1)) {
          log_prob += normal_lpdf(L_tp1_var[t]|0,1);
        }
      } else if(time_proc==4 && T>1) {
        {
          matrix[T, T] cov; // zero-length if not a GP model
          matrix[T, T] L_cov;// zero-length if not a GP model
          // chunk giving a GP prior to legislators/persons
            //create covariance matrices given current values of hiearchical parameters

            cov =   cov_exp_quad(time_ind, m_sd_full[s], time_var_full[s])
            + diag_matrix(rep_vector(square(gp_sd_full[s]),T));
            L_cov = cholesky_decompose(cov);

            log_prob += multi_normal_cholesky_lpdf(to_vector(L_tp2[,s])|rep_vector(0,T) + L_full[s], L_cov);


          }
        }

    } else {

      // priors if items

      log_prob += normal_lpdf(sigma_reg_free[s]|0, discrim_reg_sd);
      log_prob += normal_lpdf(sigma_abs_free[s]|0,discrim_abs_sd);
      log_prob += normal_lpdf(B_int_free[s]|0,diff_reg_sd);
      log_prob += normal_lpdf(A_int_free[s]|0,diff_abs_sd);

    }

    if(S_type==1) {
#include /chunks/model_types_mm_map_persons.stan
    } else {
#include /chunks/model_types_mm_map_items.stan
    }

    return log_prob;
  }

}