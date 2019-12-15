/* The main difference between this file and the other one is that it stores 
The log probability in a vector for use with map_rect as opposed to incrementing
the log posterior directly 
This file is specifically for mapping over items.*/

// Compared to the other file, we will iterate over every possible value in the data
// for both continuous and discrete outcomes
// skip all for pad_id=0

for(n in 1:N_int) {
  
  // skip all for pad_id = 0
  if(pad_id[n]==1) {
    
    log_prob += 0;
    
  } else {
    // first iterate over all the integer types
    
    if(mm[n]==1) {
      //2 PL no inflation
      
      if(T==1) {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
      } else {
        log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
      }
    } else if(mm[n]==2) {
      //2 PL inflation
      if(Y_int[n]<2) {
        // observed data 
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
      }
      
    } else if(mm[n]==3) {
  //ratingscale no inflation
  // use a version of steps based on the index of ordinal counts
  if(T==1) {
    // put all the static ones here to save space
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes10);
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
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
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes3);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes4);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes5);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes6);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes7);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes8);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes9);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      } else {
        //observed
        log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
              steps_votes10);
        log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
      }
    }
  }

} else if(mm[n]==5) {
  //grm no inflation
      
if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm3[bb[1]]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm4[bb[1]]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm5[bb[1]]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm6[bb[1]]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm7[bb[1]]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm8[bb[1]]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm9[bb[1]]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm10[bb[1]]);
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm3[bb[1]]);
  } else if(order_cats_grm[n]==4) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm4[bb[1]]);
  } else if(order_cats_grm[n]==5) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm5[bb[1]]);
  }else if(order_cats_grm[n]==6) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm6[bb[1]]);
  }else if(order_cats_grm[n]==7) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm7[bb[1]]);
  }else if(order_cats_grm[n]==8) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm8[bb[1]]);
  }else if(order_cats_grm[n]==9) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm9[bb[1]]);
  }else if(order_cats_grm[n]==10) {
    log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                    steps_votes_grm10[bb[1]]);
  }
}

} else if(mm[n]==6) {
  //grm inflation

if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm3[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm4[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm5[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm6[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm7[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm8[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm9[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm10[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm3[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm4[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm5[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm6[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm7[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm8[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm9[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>y_int_miss) {
      // missing
      log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    } else {
      //observed
      log_prob += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
                                      steps_votes_grm10[bb[1]]);
      log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[1]);
    }
  }
}

} else if(mm[n]==7) {
  //poisson no inflation
  
      if(T==1) {
        log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
      } else {
        log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
      }
        
        

} else if(mm[n]==8) {
  //hurdle poisson
  
  //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
        } else {
          log_prob += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += poisson_log_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
        } else {
          log_prob += poisson_log_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[1]);
        }
      }

} else if(mm[n]==13) {
      //latent space non-inflated (normal parameterization)
      if(T==1) {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]]  + sigma_abs_free[1] -  sqrt(square( (L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x))));
        
      } else {
        
        log_prob += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]] + sigma_abs_free[1] -
        sqrt(square((L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x))));
        
      }
      
    } else if(mm[n]==14) {
      //latent space inflated (idealstan parameterization)
      if(T==1) {
        if(Y_int[n]<y_int_miss) {
          log_prob += log(2) + bernoulli_logit_lpmf(Y_int[n]|(- sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x)))));
          log_prob += log(2) + bernoulli_logit_lpmf(0|( - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (A_int_free[1] + sax_pred[n,]*sigma_abs_x)))));
        } else {
          log_prob += log(2) + bernoulli_logit_lpmf(Y_int[n]|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x))));
        }
        
      } else {
        if(Y_int[n]<y_int_miss) {
          log_prob += log(2) + bernoulli_logit_lpmf(Y_int[n]|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x))));
          log_prob += log(2) + bernoulli_logit_lpmf(0|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (A_int_free[1] + sax_pred[n,]*sigma_abs_x))));
        } else {
          log_prob += log(2) + bernoulli_logit_lpmf(Y_int[n]|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[1] + srx_pred[n,]*sigma_reg_x))));
        }
      }
      
    } else if(mm[n]==9) {
      
      // next iterate over the continuous types
      
      
      //normal no inflation
      
      if(T==1) {
        log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
        extra_sd);
      } else {
        log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
        extra_sd);
      }
      
      
      
    } else if(mm[n]==10) {
      //normal hurdle
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
          extra_sd);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += normal_lpdf(Y_cont[n]|(sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1],
          extra_sd);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
      }
      
    } else if(mm[n]==11) {
      //lognormal no inflation
      
      if(T==1) {
        log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]),
        extra_sd);
      } else {
        log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]),
        extra_sd);
      }
      
    } else if(mm[n]==12) {
      //hurdle lognormal
      
      if(Y_cont[n]<y_cont_miss) {
        // observed data 
        if(T==1) {
          log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]),
          extra_sd);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += lognormal_lpdf(Y_cont[n]|exp((sigma_reg_free[1] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[1]),
          extra_sd);
          log_prob += bernoulli_logit_lpmf(0|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
        
      } else {
        //missing data
        if(T==1) {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        } else {
          log_prob += bernoulli_logit_lpmf(1|(sigma_abs_free[1] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
          A_int_free[1]);
        }
      }
    }
  } // end of pad_id if statement
  
  
} // end of for loop



