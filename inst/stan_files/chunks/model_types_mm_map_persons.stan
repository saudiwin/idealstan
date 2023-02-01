/* The main difference between this file and the other one is that it stores 
The log probability in a vector for use with map_rect as opposed to incrementing
the log posterior directly 
This file is specifically for mapping over persons. */

// Compared to the other file, we will iterate over every possible value in the data
// for both continuous and discrete outcomes
// skip all for pad_id=0

for(n in start2:end2) {
    
    if(mm[n]==1) {
      //2 PL no inflation
      
      if(T==1) {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
      } else {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
      }
    } else if(mm[n]==2) {
      //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          //print(time[n]);
          log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
      }
      
    } else if(mm[n]==3) {
  //ratingscale no inflation
  // use a version of steps based on the index of ordinal counts
  if(T==1) {
    // put all the static ones here to save space
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes10);
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
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
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
      }
    }
  }

} else if(mm[n]==5) {
  //grm no inflation
      
if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm3[bb[n]]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm4[bb[n]]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm5[bb[n]]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm6[bb[n]]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm7[bb[n]]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm8[bb[n]]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm9[bb[n]]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm10[bb[n]]);
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm3[bb[n]]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm4[bb[n]]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm5[bb[n]]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm6[bb[n]]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm7[bb[n]]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm8[bb[n]]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm9[bb[n]]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                    steps_votes_grm10[bb[n]]);
  }
}

} else if(mm[n]==6) {
  //grm inflation

if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm3[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm4[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm5[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm6[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm7[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm8[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm9[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm10[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm3[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm4[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm5[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm6[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm7[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm8[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm9[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
                                      steps_votes_grm10[bb[n]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                        A_int_free[bb[n]]);
    }
  }
}

} else if(mm[n]==7) {
  //poisson no inflation
  
      if(T==1) {
        log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
      } else {
        log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
      }

} else if(mm[n]==8) {
  //hurdle poisson
  
  //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
        } else {
          log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
                                 A_int_free[bb[n]]);
        }
      }

} else if(mm[n]==13) {
      //latent space non-inflated (normal parameterization)
      if(T==1) {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s]  + B_int_free[bb[n]] -  sqrt(square( (L_full[s] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
        
      } else {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[s] + B_int_free[bb[n]] -
        sqrt(square((lt[time[n]] + legis_calc[n - start2 + 1]) - (sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]))));
        
      }
      
    } else if(mm[n]==14) {
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
      
    }
    

  if(mm[n]==9) {
      
      // next iterate over the continuous types
      
      int this_sd = type_het_var[bb[n]];
      
      
      //normal no inflation
      
      if(T==1) {
        log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
        extra_sd[this_sd]);
      } else {
        log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
        extra_sd[this_sd]);
      }
      
      
      
    } else if(mm[n]==10) {
      //normal hurdle
      
      int this_sd = type_het_var[bb[n]];
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
          extra_sd[this_sd]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]],
          extra_sd[this_sd]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
      }
      
    } else if(mm[n]==11) {
      //lognormal no inflation
      
      int this_sd = type_het_var[bb[n]];
      
      if(T==1) {
        log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]),
        extra_sd[this_sd]);
      } else {
        log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]),
        extra_sd[this_sd]);
      }
      
    } else if(mm[n]==12) {
      //hurdle lognormal
      
            
      int this_sd = type_het_var[bb[n]];
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (L_full[s] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]),
          extra_sd[this_sd]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_full[bb[n]] + sigma_reg_calc[n - start2 + 1]) *  (lt[time[n]] + legis_calc[n - start2 + 1]) - B_int_free[bb[n]]),
          extra_sd[this_sd]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (L_full[s] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sigma_abs_calc[n - start2 + 1]) * (lt[time[n]] + legis_calc[n - start2 + 1]) - 
          A_int_free[bb[n]]);
        }
      }
    }
    
    
  // if(r_in(mm[n],{9,10,11,12})==1 && het_var>1) {
  // 
  //   this_item = bb[n];
  // 
  // } else if(r_in(mm[n],{9,10,11,12})==1 && het_var>1 && this_item==)
  
} // end of for loop



