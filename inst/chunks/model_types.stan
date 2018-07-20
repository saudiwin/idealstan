/* This file sets up the various types of IRT models that can be run in idealstan */

if(model_type==1) {
  //2 PL no inflation
      
      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
        }
      }
      
      Y_new ~ bernoulli_logit(pi1);

} else if(model_type==2) {
  //2 PL inflation
  
      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
        pi2 = sigma_abs_full[bb] .* L_full[ll] - 
                  A_int_full[bb] + exog_data*exog_param;
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
            pi2[n] = sigma_abs_full[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_full[bb[n]] + exog_data[n]*exog_param; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_new[n] ~ bernoulli_logit(pi1[n]);
          }
        }

} else if(model_type==3) {
  //ratingscale no inflation
  
      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
        }
      }
      
    for(n in 1:N) {
      Y[n] ~ ordered_logistic(pi1[n],steps_votes);
    }
        
        

} else if(model_type==4) {
  //ratingscale inflation

      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
        pi2 = sigma_abs_full[bb] .* L_full[ll] - 
                  A_int_full[bb] + exog_data*exog_param;
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
            pi2[n] = sigma_abs_full[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_full[bb[n]] + exog_data[n]*exog_param; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y[n] ~ ordered_logistic(pi1[n],steps_votes);
          }
        }

} else if(model_type==5) {
  //grm no inflation
      
      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
        }
      }
      
    for(n in 1:N) 
        Y[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);

} else if(model_type==6) {
  //grm inflation

      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
        pi2 = sigma_abs_full[bb] .* L_full[ll] - 
                  A_int_full[bb] + exog_data*exog_param;
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
            pi2[n] = sigma_abs_full[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_full[bb[n]] + exog_data[n]*exog_param; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);
          }
        }

} else if(model_type==7) {
  //hurdle poisson
  
      if(T==1) {
        pi1 = sigma_reg_full[bb] .*  L_full[ll] - B_int_full[bb];
        pi2 = sigma_abs_full[bb] .* L_full[ll] - 
                  A_int_full[bb] + exog_data*exog_param;
      } else {
        for(n in 1:N) {
            pi1[n] = sigma_reg_full[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_full[bb[n]];
            pi2[n] = sigma_abs_full[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_full[bb[n]] + exog_data[n]*exog_param; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y[n] ~ poisson(pi1[n]);
          }
        }

}
