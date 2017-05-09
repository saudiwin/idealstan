

data {
  int N;
  int Y[N];
  int T;
  int hier_type;
  int LX;
  int SRX;
  int SAX;
  int auto_reg;
  int ar_lag;
  int ma_lag;
  int i_lag;
  int with_absence;
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int constrain_par;
  int constraint_type
  int num_fix_high;
  int num_fix_low
  int ll[N];
  int bb[N];
  vector[num_legis] particip;
  matrix[num_legis,LX] legis_pred;
  matrix[num_bills,SRX] srx_pred;
  matrix[num_bills,SAX] sax_pred;
  vector[num_fix_high] pin_vals;
}

transformed data {
	int m;                         // # steps
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
	int num_constrain_sa;
	int num_constrain_sr;
	m = max(Y) - 1;
  for(n in 1:N) {
    if(Y[n]>m) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }
  //need to figure out correct number of rows to reserve for constrained parameters
  //if a vector of parameters has none, set to zero
  //constraint_type 1 == low, 2==high, 3==high/low,4==pin
  //constraint_par 1==legis,2==sigma abs, 3==sigma reg
  if(constraint_type==1) {
    if(contraint_par==1) {
      num_constrain_l=num_fix_low;
      num_constrain_sr=0;
      num_constrain_sa=0;
    } else if(constraint_par==2) {
      num_constrain_l=0;
      num_constrain_sr=0;
      num_constrain_sa=num_fix_low;
    } else if(constraint_par==3) {
      num_constrain_l=0;
      num_constrain_sr=num_fix_low;
      num_constrain_sa=0;
    }

  } else if(constraint_type==2 || constraint_type==4) {
    if(contraint_par==1) {
      num_constrain_l=num_fix_high;
      num_constrain_sr=0;
      num_constrain_sa=0;
    } else if(constraint_par==2) {
      num_constrain_l=0;
      num_constrain_sr=0;
      num_constrain_sa=num_fix_high;
    } else if(constraint_par==3) {
      num_constrain_l=0;
      num_constrain_sr=num_fix_high;
      num_constrain_sa=0;
    }
  } else if(constraint_type==3) {
    if(contraint_par==1) {
      num_constrain_l=num_fix_high + num_fix_low;
      num_constrain_sr=0;
      num_constrain_sa=0;
    } else if(constraint_par==2) {
      num_constrain_l=0;
      num_constrain_sr=0;
      num_constrain_sa=num_fix_high + num_fix_low;
    } else if(constraint_par==3) {
      num_constrain_l=0;
      num_constrain_sr=num_fix_high + num_fix_low;
      num_constrain_sa=0;
    }
  }

}

parameters {
  vector[num_bills-num_constrain_sa] sigma_abs_free;
  vector[num_legis-num_constrain_l] L_free[T];
  vector[num_bills-num_constrain_sr] sigma_reg_free;
  vector<upper=0>[num_constrain_sr+num_constrain_l+num_constrain_sa] restrict_low[T];
  vector<lower=0>[num_constrain_sr+num_constrain_l+num_constrain_sa] restrict_high[T];
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[LX] legis_x_cons;
  vector[SRX] sigma_reg_x_cons;
  vector[SAX] sigma_abs_x_cons;
  vector[num_bills] B_yes;
  vector[num_bills] B_abs;
  ordered[m-1] steps_votes;
  real avg_particip;
}

transformed parameters {
  
  vector[num_legis] L_full[T];
  vector[num_bills] sigma_abs_full;
  vector[num_bills] sigma_reg_full;
  
  if(constraint_par==1) {
    if(constraint_type==1) {
      //if legislators are constrained, we must take into account the potential time series 
      //dimension
      //if T=1, of course, this is just boilerplate
      for(t in 1:T) {
        L_full[t] = append_row(L_free[t],restrict_low[t]);
      }
    } else if(constraint_type==2 || constraint_type==4) {
      for(t in 1:T) {
        L_full[t] = append_row(L_free[t],restrict_high[t]);
      }
    } else if(constraint_type==3) {
      for(t in 1:T) {
        L_full[t] = append_row(L_free[t],append_row(restrict_high[t],restrict_low[t]));
      }
    }
    sigma_abs_full=sigma_abs_free;
    sigma_reg_full=sigma_reg_free;
  } else if(constraint_par==2) {
    if(constraint_type==1) {
      sigma_abs_full = append_row(sigma_abs_free,restrict_low[1]);
    } else if(constraint_type==2 || constraint_type==4) {
      sigma_abs_full = append_row(sigma_abs_free,restrict_high[1]);
    } else if(constraint_type==3) {
      sigma_abs_full = append_row(sigma_abs_free,append_row(restrict_low[1],restrict_high[1]));
    }
    L_full=L_free;
    sigma_reg_full=sigma_reg_free;
  } else if(constraint_par==3) {
    if(constraint_type==1) {
      sigma_reg_full = append_row(sigma_reg_free[1],restrict_low[1]);
    } else if(constraint_type==2 || constraint_type==4) {
      sigma_reg_full = append_row(sigma_reg_free,restrict_high[1]);
    } else if(constraint_type==3) {
      sigma_reg_full = append_row(sigma_reg_free,append_row(restrict_low[1],restrict_high[1]));
    }
    sigma_abs_full=sigma_abs_free;
    L_full = L_free;
  }
  
  
}

model {	
  vector[N] pi1;
  vector[N] pi2;
  
  /*
  hier_type: 1=L,2=SRX,3=SAX,4=L+SRX,5=L+SAX,6=SRX+SAX,7=SRX+SAX+L,8=none
  Only need to check for AR in situations using L as a constraint
  This would be better structured as a header file
  */
  
  if(hier_type==1) {
    //constrain legislators, check if we need to do an AR(0,1,0) prior
    if(T>1) {
      L_free[1] ~ normal(0,1);
      for(t in 2:T) {
        L_free[t] ~ normal(L_free[t-1] + legis_pred[2:(num_legis-num_constrain_l),]*legis_x,1);
      }
      //repeat for constrained if L is constrained
      if(constraint_par==1) {
        //need to check for each possible constraint type
        if(constraint_type==3) {
          restrict_low[1] ~ normal(legis_pred[num_legis-num_constrain_low,]*legis_x_cons,1);
          restrict_high[1] ~ normal(legis_pred[num_legis-num_constrain_l,]*legis_x_cons,1);
          for(t in 2:T) {
            restrict_low[t] ~ normal(restrict_low[t-1] + 
                            legis_pred[(num_legis-num_constrain_low+1):num_legis,]*legis_x_cons,1);
            restrict_high[t] ~ normal(restrict_high[t-1] +
                            legis_pred[(num_legis-num_constrain_l+1):(num_legis-num_constrain_low-1),]*legis_x_cons,1);
          } 
        } else if(constraint_type==4){
          //Need to pin some if someone really wants to do that for time series
          //We won't allow these values to change because that would really screw with Stan
          restrict_high[1] ~ normal(pin_vals + 
                                    legis_pred[(num_legis-num_constrain_l):num_legis,]*legis_x_cons,.1);
          for(t in 2:T) {
            restrict_high[t] ~ normal(pin_vals + 
                                    legis_pred[(num_legis-num_constrain_l):num_legis,]*legis_x_cons,.1);
          }
        } else if(constraint_type==2) {
          restrict_high[1] ~ normal(legis_pred[num_legis-num_constrain_l,]*legis_x_cons,1);
          for(t in 2:T) {
            restrict_high[t] ~ normal(restrict_high[t-1] +
                            legis_pred[(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,1);
          } 
        } else if(constraint_type==1) {
          restrict_low[1] ~ normal(legis_pred[num_legis-num_constrain_l,]*legis_x_cons,1);
          for(t in 2:T) {
            restrict_low[t] ~ normal(restrict_low[t-1] +
                            legis_pred[(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,1);
          } 
        }
      //all done with assigning legislator priors, now just add normal priors for the other coefs
      //constrain_par==1 so this is all that is required
      sigma_abs_free ~ normal(0,5);
      sigma_reg_free ~ normal(0,5);
      } else {
        //for the rest of constraints (2,3), it is simple for hier=1
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5);
        
        if(constraint_type<4) {
          restrict_high ~ normal(0,5);
          restrict_low ~ normal(0,5);
        } else {
          restrict_high ~ normal(pin_vals,.1);
        }
      }
    }
    //repetitive code, but we need to do the T=1 case separately for hier=1
    
    L_free ~ normal(legis_pred[1:(num_legis-num_constrain_l),]*legis_x,1);
    if(constrain==1) {
        if(constraint_type==3) {
          restrict_low ~ normal(legis_pred[(num_legis-num_constrain_low):num_legis,]*legis_x_cons,1);
          restrict_high ~ normal(legis_pred[(num_legis-num_constrain_l):(num_legis-num_constrain_low-1),]*legis_x_cons,1);
          } else if(constraint_type==4) {
              restrict_high ~ normal(pin_vals + 
                                    legis_pred[(num_legis-num_constrain_l):num_legis,]*legis_x_cons,.1);
          } else if(constraint_type==2) {
            restrict_high ~ normal(legis_pred[(num_legis-num_constrain_l):num_legis,]*legis_x_cons,1);
          } else if(constraint_type==1) {
            restrict_low ~ normal(legis_pred[(num_legis-num_constrain_l):num_legis,]*legis_x_cons,1);
          } 
      //don't forget about priors for sigma_abs_free and sigma_reg_free
      sigma_abs_free ~ normal(0,5);
      sigma_reg_free ~ normal(0,5);
    } else {
      //for the rest of constraints (2,3), it is simple for hier=1
        sigma_abs_free ~ normal(0,5);
        sigma_reg_free ~ normal(0,5);
        
        if(constraint_type<4) {
          restrict_high ~ normal(0,5);
          restrict_low ~ normal(0,5);
        } else {
          restrict_high ~ normal(pin_vals,.1);
        }
    }
  } else if(hier==2||hier==3||hier==6||hier==8) {
    
    L_free[1] ~ normal(0,5);
    if(T>1) {
      //only need to do this once because the rest of the code only matters if legis is constrained
      for(t in 1:T) {
        L_free[t] ~ normal(L_free[t-1],1);
      }
    }  
    //get the legis AR params out of the way if they aren't in any more hierarchical models

    if(constrain_par==1) {
        if(constraint_type==3) {
          restrict_low[1] ~ normal(0,1);
          restrict_high[1] ~ normal(0,1);
          if(T>1) {
          for(t in 2:T) {
            restrict_low[t] ~ normal(restrict_low[t-1],1);
            restrict_high[t] ~ normal(restrict_high[t-1],1);
          } 
          }
        } else if(constraint_type==4){
          //Need to pin some if someone really wants to do that for time series
          //We won't allow these values to change because that would really screw with Stan
          restrict_high[1] ~ normal(pin_vals,.1);
          if(T>1) {
          for(t in 2:T) {
            restrict_high[t] ~ normal(pin_vals,.1);
          }
          }
        } else if(constraint_type==2) {
          restrict_high[1] ~ normal(0,1);
          if(T>1) {
          for(t in 2:T) {
            restrict_high[t] ~ normal(restrict_high[t-1],1);
          } 
          }
        } else if(constraint_type==1) {
          restrict_low[1] ~ normal(0,1);
          if(T>1) {
          for(t in 2:T) {
            restrict_low[t] ~ normal(restrict_low[t-1],1);
          }
          }
        }
        //now put in the hierarchical priors
        
        if(hier==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
        } else if(hier==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
        } else if(hier==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
        } else if(hier==8) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(0,5);
        }
        
      } else if(constrain_par==2) {
        //now separately run through each possible hier version of constrain_par==2
        if(hier==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        } else if(hier==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred[1:(num_legis-num_constrain_sa),]*sigma_abs_x,5);
          if(constraint_type==1) {
            restrict_low ~ normal(sax_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_abs_x,5);
          } else if(constraint_type==2) {
            restrict_high ~ normal(sax_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_abs_x,5);
          } else if(constraint_type==3) {
            restrict_high ~ normal(sax_pred[(num_legis-num_constrain_sa):(num_legis-num_constrain_low-1),]*sigma_abs_x,5);
            restrict_low ~ normal(sax_pred[(num_legis-num_constrain_low):num_legis,]*sigma_abs_x,5);
          } else if(constraint_type==4) {
            restrict_high ~ normal(pin_vals + sax_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_abs_x,.1);
          }
        } else if(hier==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        } else if(hier==8) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(0,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        }
        
        
      } else if(constrain_par==3) {
        //now separately run through each possible hier version of constrain_par==3
        if(hier==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
          if(constraint_type==1) {
            restrict_low ~ normal(srx_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_reg_x,5);
          } else if(constraint_type==2) {
            restrict_high ~ normal(srx_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_reg_x,5);
          } else if(constraint_type==3) {
            restrict_high ~ normal(srx_pred[(num_legis-num_constrain_sa):(num_legis-num_constrain_low-1),]*sigma_reg_x,5);
            restrict_low ~ normal(srx_pred[(num_legis-num_constrain_low):num_legis,]*sigma_reg_x,5);
          } else if(constraint_type==4) {
            restrict_high ~ normal(pin_vals + srx_pred[(num_legis-num_constrain_sa):num_legis,]*sigma_reg_x,.1);
          }
        } else if(hier==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred[1:(num_legis-num_constrain_sa),]*sigma_abs_x,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        } else if(hier==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        } else if(hier==8) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(0,5);
          restrict_low ~ normal(0,5);
          restrict_high ~ normal(0,5);
        }
      }
    } else if(hier==5)
    
    //now for SRX 
    sigma_reg_free ~ normal(legis_pred[2:(num_legis-num_constrain_sr),]*sigma_reg_x_cons,5);
    
  }
  sigma_full ~ normal(0,5);
  L_free ~ normal(0,1);
  L_restrict_low ~ normal(0,5);
  L_restrict_high ~ normal(0,5);
  sigma_abs_full ~normal(0,5);
  avg_particip ~ normal(0,5);
  
  for(i in 1:(m-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5); 
  }
	
  B_yes ~ normal(0,5);
  B_abs ~ normal(0,5);

  //model
  for(n in 1:N) {
      pi1[n] = sigma_full[bb[n]] *  L_full[ll[n]] - B_yes[bb[n]];
      pi2[n] = sigma_abs_full[bb[n]] * L_full[ll[n]] - B_abs[bb[n]] + avg_particip * particip[ll[n]];
  if(absence[n]==1) {
	  1 ~ bernoulli_logit(pi2[n]);
  } else {
    0 ~ bernoulli_logit(pi2[n]);
    Y[n] ~ ordered_logistic(pi1[n],steps_votes);
  }
  }


  
}

