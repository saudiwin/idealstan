/* This file sets up the various types of IRT models that can be run in idealstan */

if(N_int>0) {
  for(n in 1:N_int) {
  // first iterate over all the integer types
    
    if(mm[n]==1) {
      //2 PL no inflation
      
      if(T==1) {
        target += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
      } else {
        target += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
      }
} else if(mm[n]==2) {
  //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          target += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += bernoulli_logit_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
      }

} else if(mm[n]==3) {
  //ratingscale no inflation
  // use a version of steps based on the index of ordinal counts
  if(T==1) {
    // put all the static ones here to save space
    if(order_cats_rat[n]==3) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes10);
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes3);
    } else if(order_cats_rat[n]==4) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes4);
    } else if(order_cats_rat[n]==5) {
     target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes5);
    }else if(order_cats_rat[n]==6) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes6);
    }else if(order_cats_rat[n]==7) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes7);
    }else if(order_cats_rat[n]==8) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes8);
    }else if(order_cats_rat[n]==9) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes9);
    }else if(order_cats_rat[n]==10) {
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
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
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes3);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes4);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes5);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes6);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes7);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes8);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes9);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes10);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }
  } else {
    // same but time-varying
    if(order_cats_rat[n]==3) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes3);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==4) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes4);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    } else if(order_cats_rat[n]==5) {
     if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes5);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==6) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes6);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==7) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes7);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==8) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes8);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==9) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes9);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }else if(order_cats_rat[n]==10) {
      if(Y_int[n]>order_cats_rat[n]) {
        // missing
        target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      } else {
        //observed
        target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
              steps_votes10);
        target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
      }
    }
  }

} else if(mm[n]==5) {
  //grm no inflation
      
if(T==1) {
  // put all the static ones here to save space
  if(order_cats_grm[n]==3) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm3[bb[n]]);
  } else if(order_cats_grm[n]==4) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm4[bb[n]]);
  } else if(order_cats_grm[n]==5) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm5[bb[n]]);
  }else if(order_cats_grm[n]==6) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm6[bb[n]]);
  }else if(order_cats_grm[n]==7) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm7[bb[n]]);
  }else if(order_cats_grm[n]==8) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm8[bb[n]]);
  }else if(order_cats_grm[n]==9) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm9[bb[n]]);
  }else if(order_cats_grm[n]==10) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm10[bb[n]]);
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm3[bb[n]]);
  } else if(order_cats_grm[n]==4) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm4[bb[n]]);
  } else if(order_cats_grm[n]==5) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm5[bb[n]]);
  }else if(order_cats_grm[n]==6) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm6[bb[n]]);
  }else if(order_cats_grm[n]==7) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm7[bb[n]]);
  }else if(order_cats_grm[n]==8) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm8[bb[n]]);
  }else if(order_cats_grm[n]==9) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                    steps_votes_grm9[bb[n]]);
  }else if(order_cats_grm[n]==10) {
    target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
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
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) -
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm3[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) -
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm4[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm5[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm6[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm7[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm8[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm9[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>order_cats_grm[n]) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm10[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }
} else {
  // same but time-varying
  if(order_cats_grm[n]==3) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm3[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==4) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm4[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  } else if(order_cats_grm[n]==5) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm5[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==6) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm6[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==7) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm7[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==8) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm8[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==9) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm9[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }else if(order_cats_grm[n]==10) {
    if(Y_int[n]>y_int_miss) {
      // missing
      target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    } else {
      //observed
      target += ordered_logistic_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                      steps_votes_grm10[bb[n]]);
      target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                        A_int_free[bb[n]]);
    }
  }
}

} else if(mm[n]==7) {
  //poisson no inflation
  
      if(T==1) {
        target += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
      } else {
        target += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
      }
        
        

} else if(mm[n]==8) {
  //hurdle poisson
  
  //2 PL inflation
      if(Y_int[n]<y_int_miss) {
        // observed data 
        if(T==1) {
          target += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += poisson_log_lpmf(Y_int[n]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
      }

} else if(mm[n]==13) {
  //latent space non-inflated (normal parameterization)
  if(T==1) {
    
        target += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]]  + sigma_abs_free[bb[n]] -  sqrt(square( (L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x))));

  } else {

        target += bernoulli_logit_lpmf(Y_int[n]|ls_int[ll[n]] + sigma_abs_free[bb[n]] -
                  sqrt(square((L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x))));

    }
    
} else if(mm[n]==14) {
  //latent space inflated (idealstan parameterization)
    if(T==1) {
      if(Y_int[n]<y_int_miss) {
        target += log(2) + bernoulli_logit_lpmf(Y_int[n]|(- sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x)))));
        target += log(2) + bernoulli_logit_lpmf(0|( - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (A_int_free[bb[n]] + sax_pred[n,]*sigma_abs_x)))));
      } else {
        target += log(2) + bernoulli_logit_lpmf(1|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x))));
      }

    } else {
      if(Y_int[n]<y_int_miss) {
        target += log(2) + bernoulli_logit_lpmf(Y_int[n]|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x))));
        target += log(2) + bernoulli_logit_lpmf(0|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (A_int_free[bb[n]] + sax_pred[n,]*sigma_abs_x))));
      } else {
        target += log(2) + bernoulli_logit_lpmf(1|0 - sqrt(square((L_full[ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x))));
      }
    }
  
  }
  }
}

if(N_cont>0) {
  // next iterate over the continuous types
  for(n in (N_int+1):N) {
    if(mm[n]==9) {
  //normal no inflation
  
      if(T==1) {
        target += normal_lpdf(Y_cont[n-N_int]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                  extra_sd);
      } else {
        target += normal_lpdf(Y_cont[n-N_int]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                  extra_sd);
      }
        
        

} else if(mm[n]==10) {
  //normal hurdle
  
      if(Y_cont[n-N_int]<y_cont_miss) {
        // observed data 
        if(T==1) {
          target += normal_lpdf(Y_cont[n-N_int]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                  extra_sd);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += normal_lpdf(Y_cont[n-N_int]|(sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]],
                                  extra_sd);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
      }

} else if(mm[n]==11) {
  //lognormal no inflation
  
  if(Y_cont[n-N_int]==0) {
    print("Warning: you have zero values for a log-normal distribution variable. You must remove them or use another distribution, like Normal or Poisson.");
  }
  
      if(T==1) {
        target += lognormal_lpdf(Y_cont[n-N_int]|exp((sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]),
                                  extra_sd);
      } else {
        target += lognormal_lpdf(Y_cont[n-N_int]|exp((sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]),
                                  extra_sd);
      }

} else if(mm[n]==12) {
  //hurdle lognormal
  
  if(Y_cont[n-N_int]==0) {
    print("Warning: you have zero values for a log-normal distribution variable. You must remove them or use another distribution, like Normal or Poisson.");
  }
  
      if(Y_cont[n-N_int]<y_cont_miss) {
        // observed data 
        if(T==1) {
          
          target += lognormal_lpdf(Y_cont[n-N_int]|exp((sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_full[ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]),
                                  extra_sd);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += lognormal_lpdf(Y_cont[n-N_int]|exp((sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]]),
                                  extra_sd);
          target += bernoulli_logit_lpmf(0|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
        
      } else {
        //missing data
        if(T==1) {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_full[ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        } else {
          target += bernoulli_logit_lpmf(1|(sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                                 A_int_free[bb[n]]);
        }
      }

    } 
  }
}

 

