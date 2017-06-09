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