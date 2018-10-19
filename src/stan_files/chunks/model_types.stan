/* This file sets up the various types of IRT models that can be run in idealstan */

if(model_type==1) {
  //2 PL no inflation
      
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
      Y_new ~ bernoulli_logit(pi1);

} else if(model_type==2) {
  //2 PL inflation
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

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
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_int[n] ~ ordered_logistic(pi1[n],steps_votes);
    }
        
        

} else if(model_type==4) {
  //ratingscale inflation

      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_int[n] ~ ordered_logistic(pi1[n],steps_votes);
          }
        }

} else if(model_type==5) {
  //grm no inflation
      
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) 
        Y_int[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);

} else if(model_type==6) {
  //grm inflation

      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_int[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);
          }
        }

} else if(model_type==7) {
  //poisson no inflation
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_int[n] ~ poisson(exp(pi1[n]));
    }
        
        

} else if(model_type==8) {
  //hurdle poisson
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_int[n] ~ poisson(exp(pi1[n]));
          }
        }

} else if(model_type==9) {
  //normal no inflation
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_cont[n] ~ normal(pi1[n],extra_sd);
    }
        
        

} else if(model_type==10) {
  //normal hurdle
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_cont[n] ~ normal(pi1[n],extra_sd);
          }
        }

} else if(model_type==11) {
  //lognormal no inflation
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_cont[n] ~ lognormal(exp(pi1[n]),extra_sd);
    }

} else if(model_type==12) {
  //hurdle lognormal
  
      if(T==1) {
        pi1 = sigma_reg_free[bb] .*  L_full[ll] - B_int_free[bb];
        pi2 = sigma_abs_free[bb] .* L_full[ll] - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = sigma_reg_free[bb[n]] *  L_tp1[time[n],ll[n]] - B_int_free[bb[n]];
              pi2[n] = sigma_abs_free[bb[n]] * L_tp1[time[n],ll[n]] - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_cont[n] ~ lognormal(exp(pi1[n]),extra_sd);
          }
        }

} else if(model_type==13) {
  //latent space non-inflated (normal parameterization)
  if(T==1) {
        pi1 = sigma_reg_free[bb]  + sigma_abs_free[bb] -  sqrt(square( L_full[ll] - B_int_free[bb]));

  } else {
      for(n in 1:N) {

        pi1[n] = sigma_reg_free[bb[n]] + sigma_abs_free[bb[n]] -
                  sqrt(square(L_tp1[time[n],ll[n]] - B_int_free[bb[n]]));

      }
    }

  Y_new ~ bernoulli_logit(pi1);
  
} else if(model_type==14) {
  //latent space inflated (idealstan parameterization)
    if(T==1) {
        pi1 = -sqrt(square(L_full[ll] - B_int_free[bb]));
        pi2 = -sqrt(square(L_full[ll] - A_int_free[bb]));
    } else {
        for(n in 1:N) {
          
              pi1[n] = -sqrt(square(L_tp1[time[n],ll[n]] - B_int_free[bb[n]]));
              pi2[n] = -sqrt(square(L_tp1[time[n],ll[n]] - A_int_free[bb[n]])); 

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
  
}

