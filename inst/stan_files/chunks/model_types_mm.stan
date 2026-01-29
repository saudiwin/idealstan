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
  
  // maximum number of reported debug issues
  
  int debug_issues = 0;
  
  real lin_mod_obs;
  real lin_mod_abs;
  real lin_mod_obs_dyn;
  real lin_mod_abs_dyn;
  int this_ordbeta;
  int this_sigma;
  
  //define other IDs
  
  if(S_type==1) {
    
    if(mm[n]>14) this_ordbeta = ordbeta_id[bb[n]];
    if(mm[n]>8 && mm[n]<13) this_sigma = type_het_var[bb[n]];
    
  } else {
    
    if(mm[n]>14) this_ordbeta = ordbeta_id[s];
    if(mm[n]>8 && mm[n]<13) this_sigma = type_het_var[s];
    
  }
  
 if(T==1) {
   
   if(S_type==1) {
     
     lin_mod_obs = (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]];
    
   } else {
     
      lin_mod_obs = (sigma_reg_full[s] + sigma_reg_calc[n - start2 + 1]) *  (L_full[ll[n]] + legis_calc[n - start2 + 1]) - B_int_free[s];
     
   }
    
    if(debug_mode==1) {
     
     if((is_nan(inv_logit(lin_mod_obs)) || inv_logit(lin_mod_obs)==0) && debug_issues<5) {
       
       debug_issues = debug_issues + 1;
      
      print("Logit test failed for lin_mod_obs");
      
      real sigma_reg_full_bb;
      real L_full_s;
      
      if(S_type==1) sigma_reg_full_bb = sigma_reg_full[bb[n]];
      if(S_type==2) sigma_reg_full_bb = sigma_reg_full[s];
      real sigma_reg_calc_val = sigma_reg_calc[n - start2 + 1];
      if(S_type==1) L_full_s = L_full[s];
      if(S_type==2) L_full_s = L_full[ll[n]];
      real legis_calc_val = legis_calc[n - start2 + 1];
      real B_int_free_bb = B_int_free[bb[n]];
      
      if(is_nan(inv_logit(lin_mod_obs))) print("Value is NAN");
      if(inv_logit(lin_mod_obs)==0) print("Value is 0");
      if(S_type==1) print("sigma_reg_full[bb[n]] = ", sigma_reg_full_bb);
      if(S_type==2) print("sigma_reg_full[s] = ", sigma_reg_full_bb);
      print("sigma_reg_calc[n - start2 + 1] = ", sigma_reg_calc_val);
      if(S_type==1) print("L_full[s] = ", L_full_s);
      if(S_type==2) print("L_full[ll[n]] = ", L_full_s);
      print("legis_calc[n - start2 + 1] = ", legis_calc_val);
      print("B_int_free[bb[n]] = ", B_int_free_bb);
      print("Final result = ", lin_mod_obs);
      
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
     
     if(is_nan(inv_logit(lin_mod_obs_dyn)) || inv_logit(lin_mod_obs_dyn)==0 && debug_issues<5) {
       
       debug_issues = debug_issues + 1;
       real sigma_reg_full_bb;
       real lt_time;
       
      if(S_type==1) sigma_reg_full_bb = sigma_reg_full[bb[n]];
      if(S_type==2) sigma_reg_full_bb = sigma_reg_full[s];
      real sigma_reg_calc_val = sigma_reg_calc[n - start2 + 1];
      if(S_type==1) lt_time = lt[time[n]];
      if(S_type==2) lt_time = L_tp1[time[n],ll[n]];
      real legis_calc_val = legis_calc[n - start2 + 1];
      real B_int_free_bb = B_int_free[bb[n]];
      
      print("Logit test failed for lin_mod_obs_dyn");
      
      if(is_nan(inv_logit(lin_mod_obs_dyn))) print("Value is NAN");
      if(inv_logit(lin_mod_obs_dyn)==0) print("Value is 0");
      
      if(S_type==1) print("sigma_reg_full[bb[n]] = ", sigma_reg_full_bb);
      if(S_type==2) print("sigma_reg_full[s] = ", sigma_reg_full_bb);
      print("sigma_reg_calc[n - start2 + 1] = ", sigma_reg_calc_val);
      print("legis_calc[n - start2 + 1] = ", legis_calc_val);
      print("B_int_free[bb[n]] = ", B_int_free_bb);
      print("lin_mod_obs_dyn = ", lin_mod_obs_dyn);
      if(S_type==1) print("lt[time[n]] = ", lt_time);
      if(S_type==2) print("L_tp1[time[n],ll[n]] = ", lt_time);
      //if(inv_logit(lin_mod_obs_dyn)==1) print("Value is 1");
      
      print("Data point ",n);
      
     }
      
    }
    
    
  }
  
   if(r_in(mm[n],miss_mods)>0) {
     
    if(T==1) {
      
      if(S_type==1) {
      
          lin_mod_abs = (sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]];
          
      } else {
        
        lin_mod_abs = (sigma_abs_free[s] + sigma_abs_calc[n - start2 + 1]) * (L_full[ll[n]] + legis_calc[n - start2 + 1]) - A_int_free[s];
        
      }
          
     if(debug_mode==1) {
     
     if(is_nan(inv_logit(lin_mod_abs)) || inv_logit(lin_mod_abs)==0 && debug_issues<5) {
       
       debug_issues = debug_issues + 1;
       real sigma_abs_free_bb;
       real L_full_s;
       
      if(S_type==1) sigma_abs_free_bb = sigma_abs_free[bb[n]];
      if(S_type==2) sigma_abs_free_bb = sigma_abs_free[s];
      real sigma_abs_calc_val = sigma_abs_calc[n - start2 + 1];
      if(S_type==1) L_full_s = L_full[s];
      if(S_type==2) L_full_s = L_full[ll[n]];
      real legis_calc_val = legis_calc[n - start2 + 1];
      real A_int_free_bb = A_int_free[bb[n]];
      
      print("Logit test failed for lin_mod_abs");
      
      if(is_nan(inv_logit(lin_mod_abs))) print("Value is NAN");
      if(inv_logit(lin_mod_abs)==0) print("Value is 0");
      //if(inv_logit(lin_mod_abs)==1) print("Value is 1");
      
      if(S_type==1) print("sigma_abs_free[bb[n]] = ", sigma_abs_free_bb);
      if(S_type==2) print("sigma_abs_free[s] = ", sigma_abs_free_bb);
      print("sigma_abs_calc[n - start2 + 1] = ", sigma_abs_calc_val);
      if(S_type==1) print("L_full[s] = ", L_full_s);
      if(S_type==2) print("L_full[ll[n]] = ", L_full_s);
      print("legis_calc[n - start2 + 1] = ", legis_calc_val);
      print("A_int_free[bb[n]] = ", A_int_free_bb);
      print("Final result = ", lin_mod_abs);
      
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
        
        real sigma_abs_free_bb;
        real lt_time;
        
      if(S_type==1) sigma_abs_free_bb = sigma_abs_free[bb[n]];
      if(S_type==2) sigma_abs_free_bb = sigma_abs_free[s];
      real sigma_abs_calc_val = sigma_abs_calc[n - start2 + 1];
      if(S_type==1) lt_time = lt[time[n]];
      if(S_type==2) lt_time = L_tp1[time[n],ll[n]];
      real legis_calc_val = legis_calc[n - start2 + 1];
      real A_int_free_bb = A_int_free[bb[n]];
     
     if((is_nan(inv_logit(lin_mod_abs_dyn)) || inv_logit(lin_mod_abs_dyn)==0 || inv_logit(lin_mod_abs_dyn)==1) && debug_issues<5) {
       
       debug_issues = debug_issues + 1;
      
      print("Logit test failed for lin_mod_abs");
      
      print("sigma_abs_free[bb[n]] = ", sigma_abs_free_bb);
      print("sigma_abs_calc[n - start2 + 1] = ", sigma_abs_calc_val);
      print("lt[time[n]] = ", lt_time);
      print("legis_calc[n - start2 + 1] = ", legis_calc_val);
      print("A_int_free[bb[n]] = ", A_int_free_bb);
      print("lin_mod_abs_dyn = ", lin_mod_abs_dyn);
      
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
  // Use item index based on S_type: bb[n] when mapping over persons (S_type==1), s when mapping over items
  int grm_item_idx = S_type==1 ? bb[n] : s;

if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm3[grm_item_idx]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm4[grm_item_idx]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm5[grm_item_idx]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm6[grm_item_idx]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm7[grm_item_idx]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm8[grm_item_idx]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm9[grm_item_idx]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                    steps_votes_grm10[grm_item_idx]);
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm3[grm_item_idx]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm4[grm_item_idx]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm5[grm_item_idx]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm6[grm_item_idx]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm7[grm_item_idx]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm8[grm_item_idx]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm9[grm_item_idx]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                    steps_votes_grm10[grm_item_idx]);
  }
}

} else if(mm[n]==6) {
  //grm inflation
  // Use item index based on S_type: bb[n] when mapping over persons (S_type==1), s when mapping over items
  int grm_item_idx_infl = S_type==1 ? bb[n] : s;

if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm3[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm4[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm5[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm6[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm7[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm8[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm9[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs);

    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs,
                                      steps_votes_grm10[grm_item_idx_infl]);
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
                                      steps_votes_grm3[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm4[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm5[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm6[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm7[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm8[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm9[grm_item_idx_infl]);
      log_prob += bernoulli_logit_lpmf(0|lin_mod_abs_dyn);

    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|lin_mod_abs_dyn);

    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|lin_mod_obs_dyn,
                                      steps_votes_grm10[grm_item_idx_infl]);
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
        extra_sd[this_sigma]);
      } else {
        log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
        extra_sd[this_sigma]);
      }
      
      
      
    } else if(mm[n]==10) {
      //normal hurdle
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs,
          extra_sd[this_sigma]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += normal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
          extra_sd[this_sigma]);
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
        extra_sd[this_sigma]);
      } else {
        log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
        extra_sd[this_sigma]);
      }
      
    } else if(mm[n]==12) {
      //hurdle lognormal
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs,
          extra_sd[this_sigma]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += lognormal_lpdf(Y_cont[n]|lin_mod_obs_dyn,
          extra_sd[this_sigma]);
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
                    phi[this_ordbeta],
                    ordbeta_cut[this_ordbeta]);
      } else {
        log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs_dyn,
                    phi[this_ordbeta],
                    ordbeta_cut[this_ordbeta]);
      }
    } else if(mm[n]==16) {
      //ordered beta with inflation
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs,
                    phi[this_ordbeta],
                    ordbeta_cut[this_ordbeta]);
          log_prob += bernoulli_logit_lpmf(0|lin_mod_abs); 
          
        } else {
          log_prob += ordbeta_lpdf(Y_cont[n]|lin_mod_obs_dyn,
                    phi[this_ordbeta],
                    ordbeta_cut[this_ordbeta]);
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



