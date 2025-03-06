/* The main difference between this file and the other one is that it stores 
The log probability in a vector for use with map_rect as opposed to incrementing
the log posterior directly 
This file is specifically for mapping over items.*/

// Compared to the other file, we will iterate over every possible value in the data
// for both continuous and discrete outcomes
// skip all for pad_id=0

for(n in start2:end2) {
  
  // all models with missing data
  array[7] int miss_mods = {2,4,6,8,10,12,16};
  
  real lin_mod_obs;
  real lin_mod_abs;
  real lin_mod_obs_dyn;
  real lin_mod_abs_dyn;
  
 if(T==1) {
   
   if(S_type==1) {
     
     lin_mod_obs = (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]];
    
   } else {
     
      lin_mod_obs = (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]) *  (L_full[ll[n]] + legis_calc[n - start2 + 1]) - B_int_free[s];
     
   }
    
    if(debug_mode==1) {
     
     if((is_nan(inv_logit(lin_mod_obs)) || inv_logit(lin_mod_obs)==0 || inv_logit(lin_mod_obs)==1)) {
      
      print("Logit test failed for lin_mod_obs");
      
      if(is_nan(inv_logit(lin_mod_obs))) print("Value is NAN");
      if(inv_logit(lin_mod_obs)==0) print("Value is 0");
      //if(inv_logit(lin_mod_obs)==1) print("Value is 1");
      
      print("Data point ",n);
      
     }
      
    }
    
    
  } else {
    
    if(S_type==1) {
    
    lin_mod_obs_dyn = (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]];
    
    } else {
      
      lin_mod_obs_dyn = (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]) *  (L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - B_int_free[s];
      
    }
    
    if(debug_mode==1) {
     
     if((is_nan(inv_logit(lin_mod_obs_dyn)) || inv_logit(lin_mod_obs_dyn)==0 || inv_logit(lin_mod_obs_dyn)==1)) {
      
      print("Logit test failed for lin_mod_obs_dyn");
      
      if(is_nan(inv_logit(lin_mod_obs_dyn))) print("Value is NAN");
      if(inv_logit(lin_mod_obs_dyn)==0) print("Value is 0");
      //if(inv_logit(lin_mod_obs_dyn)==1) print("Value is 1");
      
      print("Data point ",n);
      
     }
      
    }
    
    
  }
  
   if(r_in(mm[n],miss_mods)==1) {
     
    if(T==1) {
      
      if(S_type==1) {
      
          lin_mod_abs = (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]];
          
      } else {
        
        lin_mod_abs = (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]) * (L_full[ll[n]] + legis_calc[n - start2 + 1]) - A_int_free[s];
        
      }
          
     if(debug_mode==1) {
     
     if((is_nan(inv_logit(lin_mod_abs)) || inv_logit(lin_mod_abs)==0 || inv_logit(lin_mod_abs)==1)) {
      
      print("Logit test failed for lin_mod_abs");
      
      if(is_nan(inv_logit(lin_mod_abs))) print("Value is NAN");
      if(inv_logit(lin_mod_abs)==0) print("Value is 0");
      //if(inv_logit(lin_mod_abs)==1) print("Value is 1");
      
      print("Data point ",n);
      
     }
      
    }
      
    } else {
      
      if(S_type==1) {
      
      lin_mod_abs_dyn = (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - A_int_free[bb[n]];
      
      } else {
        
        lin_mod_abs_dyn = (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]) * (L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - A_int_free[s];
        
      }
      
      if(debug_mode==1) {
     
     if((is_nan(inv_logit(lin_mod_abs_dyn)) || inv_logit(lin_mod_abs_dyn)==0 || inv_logit(lin_mod_abs_dyn)==1)) {
      
      print("Logit test failed for lin_mod_abs");
      
      if(is_nan(inv_logit(lin_mod_abs_dyn))) print("Value is NAN");
      if(inv_logit(lin_mod_abs_dyn)==0) print("Value is 0");
      //if(inv_logit(lin_mod_abs_dyn)==1) print("Value is 1");
      
      print("Data point ",n);
      
     }
      
    }
      
    }
     
   }
  
  
  
  
  
    // first iterate over all the integer types
    
    if(mm[n]==1) {
      //2 PL no inflation
      
      if(T==1) {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|lin_mod_obs);
      } else {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|lin_mod_obs_dyn);
      }
    } else if(mm[n]==2) {
      //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|lin_mod_obs);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);
        } else {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|lin_mod_obs_dyn);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);
        } else {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);
        }
      }
      
    } else if(mm[n]==3) {
  //ratingscale no inflation
  // use a version of steps based on the index of ordinal counts
  if(T==1) {
    // put all the static ones here to save space
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes10);
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes10);
    }
  }
} else if(mm[n]==4) {
  //ratingscale inflation
    if(T==1) {
    // put all the static ones here to save space
    if(order_cats_rat[n]==3) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
      }
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
      }
    }
  }

} else if(mm[n]==5) {
  //grm no inflation
      
if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm3[s]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm4[s]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm5[s]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm6[s]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm7[s]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm8[s]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm9[s]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm10[s]);
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm3[s]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm4[s]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm5[s]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm6[s]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm7[s]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm8[s]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm9[s]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm10[s]);
  }
}

} else if(mm[n]==6) {
  //grm inflation

if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm3[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm4[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm5[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm6[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm7[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm8[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm9[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm10[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                        
    }
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm3[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm4[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm5[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm6[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm7[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm8[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm9[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                        
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm10[s]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                        
    }
  }
}

} else if(mm[n]==7) {
  //poisson no inflation
  
      if(T==1) {
        log_prob += poisson_log_lpmf(Y_int[n]|lin_mod_obs);
      } else {
        log_prob += poisson_log_lpmf(Y_int[n]|lin_mod_obs_dyn);
      }
        
        

} else if(mm[n]==8) {
  //hurdle poisson
  
  //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          log_prob += poisson_log_lpmf(Y_int[n]|lin_mod_obs);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
                                 
        } else {
          log_prob += poisson_log_lpmf(Y_int[n]|lin_mod_obs_dyn);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
                                 
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
                                 
        } else {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
                                 
        }
      }

} else if(mm[n]==13) {
  
  //latent space non-inflated (normal parameterization)
  
  if(S_type==1)  {
    
    //person mapping
    
    //latent space non-inflated (normal parameterization)
      if(T==1) {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s]  + B_int_free[bb[n]] -  sqrt(square( (L_full[s] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
        
      } else {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s] + B_int_free[bb[n]] -
        sqrt(square((lt[time[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
        
      }
    
    
    
  } else {
    
          //item mapping
      
      if(T==1) {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]]  + B_int_free[s] -  sqrt(square( (L_full[ll[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]))));
        
      } else {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]] + B_int_free[s] -
        sqrt(square((L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]))));
        
      }
      }
  

      
    } else if(mm[n]==14) {
      
      if(S_type==1) {
        
        //version for mapping over persons
        
        //latent space inflated (idealstan parameterization)
      if(T==1) {
        if(Y_int[n]<y_int_miss) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s] + B_int_free[bb[n]] - sqrt(square((L_full[s] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
          log_prob += bernoulli_logit_lpmf(0|ls_int_abs[s] + A_int_free[bb[n]] - sqrt(square((L_full[s] + legis_calc[n - start2 + 1]) - (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]))));
        } else {
          log_prob += bernoulli_logit_lpmf(1|ls_int_abs[s] + A_int_free[bb[n]] - sqrt(square((L_full[s] + legis_calc[n - start2 + 1]) - (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]))));
        }
        
      } else {
        if(Y_int[n]<y_int_miss) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s] + B_int_free[bb[n]] - sqrt(square((lt[time[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
          log_prob += bernoulli_logit_lpmf(0|ls_int_abs[s] + A_int_free[bb[n]] - sqrt(square((lt[time[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]))));
        } else {
          log_prob += bernoulli_logit_lpmf(1|ls_int_abs[s] + A_int_free[bb[n]] - sqrt(square((lt[time[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]))));
        }
      }
        
        
        
      } else {
        
        // version for mapping over items
        
         //latent space inflated (idealstan parameterization)
      if(T==1) {
        if(Y_int[n]<y_int_miss) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]]  + B_int_free[s] - sqrt(square((L_full[ll[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]))));
          log_prob += bernoulli_logit_lpmf(0| ls_int_abs[ll[n]]  + A_int_free[s] - sqrt(square((L_full[ll[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]))));
        } else {
          log_prob += bernoulli_logit_lpmf(1|ls_int_abs[ll[n]]  + A_int_free[s] - sqrt(square((L_full[ll[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]))));
        }
        
      } else {
        if(Y_int[n]<y_int_miss) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]]  + B_int_free[s] - sqrt(square((L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]))));
          log_prob += bernoulli_logit_lpmf(0|ls_int_abs[ll[n]]  + A_int_free[s] - sqrt(square((L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]))));
        } else {
          log_prob += bernoulli_logit_lpmf(1|ls_int_abs[ll[n]]  + A_int_free[s] - sqrt(square((L_tp1[time[n],ll[n]] + legis_calc[n - start2 + 1]) - (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]))));
        }
      }
        
        
        
      }
      
     
      
    } else if(mm[n]==9) {
      
      // next iterate over the continuous types
      
      
      //normal no inflation
      
      if(T==1) {
        log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs,
        extra_sd[type_het_var[s]]);
      } else {
        log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
        extra_sd[type_het_var[s]]);
      }
      
      
      
    } else if(mm[n]==10) {
      //normal hurdle
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs,
          extra_sd[type_het_var[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
          extra_sd[type_het_var[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
          
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
          
        } else {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
          
        }
      }
      
    } else if(mm[n]==11) {
      //lognormal no inflation
      
      if(T==1) {
        log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs,
        extra_sd[type_het_var[s]]);
      } else {
        log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
        extra_sd[type_het_var[s]]);
      }
      
    } else if(mm[n]==12) {
      //hurdle lognormal
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs,
          extra_sd[type_het_var[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
          extra_sd[type_het_var[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
          
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
          
        } else {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
          
        }
      }
    } else if(mm[n]==15) {
      //ordered beta no inflation
      
      if(T==1) {
        log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs,
                    phi[ordbeta_id[s]],
                    ordbeta_cut[ordbeta_id[s]]);
      } else {
        log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs_dyn,
                    phi[ordbeta_id[s]],
                    ordbeta_cut[ordbeta_id[s]]);
      }
    } else if(mm[n]==16) {
      //ordered beta with inflation
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs,
                    phi[ordbeta_id[s]],
                    ordbeta_cut[ordbeta_id[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs_dyn,
                    phi[ordbeta_id[s]],
                    ordbeta_cut[ordbeta_id[s]]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn); 
          
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs); 
          
        } else {
          log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn); 
          
        }
      }
      
    }
    
  // if(r_in(mm[n],{9,10,11,12})==1  && het_var>1) {
  //   
  //   this_var += 1;
  //   
  // }
  
} // end of for loop



