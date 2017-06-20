/*
  hier_type_type: 1=L,2=SRX,3=SAX,4=L+SRX,5=L+SAX,6=SRX+SAX,7=SRX+SAX+L,8=none
  Only need to check for AR in situations using L as a constraint
  This would be better structured as a header file
  */
  
  if(constrain_par==1) {
    if(constraint_type==1) {
      if(hier_type==1||hier_type==4||hier_type==5||hier_type==7) {
            restrict_low[1] ~ normal(legis_pred[1,(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
            L_free[1] ~ normal(legis_pred[1,1:(num_legis-num_constrain_l),]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                restrict_low[t] ~ normal(restrict_low[t-1] +
                legis_pred[t,(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
                
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,1:(num_legis-num_constrain_l),]*legis_x,1);
              }
            } 
            
            if(hier_type==1) {
              //all done with assigning legislator priors, now just add normal priors for the other coefs
              //constrain_par==1 so this is all that is required
              sigma_abs_free ~ normal(0,5);
              sigma_reg_free ~ normal(0,5);
            } else if(hier_type==4) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(0,5);
            } else if(hier_type==5) {
              sigma_reg_free ~ normal(0,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            } else if(hier_type==7) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            }
      } else {
        //legis the same for all other hier_type combinations
          restrict_low[1] ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              restrict_low[t] ~ normal(restrict_low[t-1],5);
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
          
          //do all the other hier_type combinations for when legis is constrained. none of these affect hier_type in legis

         if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
         } else if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5); 
         }
  
      }
    //all done with constraint_type==1 and constrain_par==1  
    
    } else if(constraint_type==2) {
      //just copy and paste for all other constraint types, change the type of constraint in the legis block
      if(hier_type==1||hier_type==4||hier_type==5||hier_type==7) {

            restrict_high[1] ~ normal(legis_pred[1,(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
            L_free[1] ~ normal(legis_pred[1,1:(num_legis-2*num_constrain_l),]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                restrict_high[t] ~ normal(restrict_high[t-1] +
                legis_pred[t,(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
                
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,1:(num_legis-num_constrain_l),]*legis_x,1);
              }
            } 
            
        if(hier_type==1) {
              //all done with assigning legislator priors, now just add normal priors for the other coefs
              //constrain_par==1 so this is all that is required
              sigma_abs_free ~ normal(0,5);
              sigma_reg_free ~ normal(0,5);
            } else if(hier_type==4) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(0,5);
            } else if(hier_type==5) {
              sigma_reg_free ~ normal(0,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            } else if(hier_type==7) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
        }
      } else {
        //legis the same for all other hier_type combinations
          restrict_high[1] ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              restrict_high[t] ~ normal(restrict_high[t-1],5);
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
          
          //do all the other hier_type combinations for when legis is constrained. none of these affect hier_type in legis

         if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
         } else if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5); 
         }
  
      }
      //all done with constraint_type==2 and constrain_par==1
    } else if(constraint_type==3) {
      //just copy and paste for all other constraint types, change the type of constraint in the legis block
      if(hier_type==1||hier_type==4||hier_type==5||hier_type==7) {
          //when restricting both, need to be very careful with how high and low restrictions are indexed
            restrict_high[1] ~ normal(legis_pred[1,(num_legis-(2*num_constrain_l)+1):(num_legis-num_constrain_l),]*legis_x_cons,5);
            restrict_low[1] ~ normal(legis_pred[1,(num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
            L_free[1] ~ normal(legis_pred[1,1:(num_legis-num_constrain_l),]*legis_x,5);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                restrict_high[t] ~ normal(restrict_high[t-1] +
                legis_pred[t,(num_legis-(2*num_constrain_l)+1):(num_legis-num_constrain_l),]*legis_x_cons,5);
                
                restrict_low[t] ~ normal(restrict_high[t-1] +
                legis_pred[t,
                (num_legis-num_constrain_l+1):num_legis,]*legis_x_cons,5);
                
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,1:(num_legis-num_constrain_l),]*legis_x,1);
              }
            }
            
            if(hier_type==1) {
              //all done with assigning legislator priors, now just add normal priors for the other coefs
              //constrain_par==1 so this is all that is required
              sigma_abs_free ~ normal(0,5);
              sigma_reg_free ~ normal(0,5);
            } else if(hier_type==4) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(0,5);
            } else if(hier_type==5) {
              sigma_reg_free ~ normal(0,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            } else if(hier_type==7) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            }
      } else {
        //legis the same for all other hier_type combinations
          restrict_high[1] ~ normal(0,5);
          restrict_low[1] ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              restrict_high[t] ~ normal(restrict_high[t-1],5);
              L_free[t] ~ normal(L_free[t-1],5);
            }
          } 
          
          //do all the other hier_type combinations for when legis is constrained. none of these affect hier_type in legis

         if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
         } else if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5); 
         }
 
      }
      
    //all done with constrain_par==1 and constraint_type==3
    } else if(constraint_type==4) {
      //now we pin
      if(hier_type==1||hier_type==4||hier_type==5||hier_type==7) {
          //when restricting both, need to be very careful with how high and low restrictions are indexed
            restrict_high[1] ~ normal(pin_vals,.1);
            L_free[1] ~ normal(legis_pred[1,1:(num_legis-num_constrain_l),]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                restrict_high[t] ~ normal(pin_vals,.1);
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,1:(num_legis-num_constrain_l),]*legis_x,1);
              }
            }
            
            if(hier_type==1) {
              //all done with assigning legislator priors, now just add normal priors for the other coefs
              //constrain_par==1 so this is all that is required
              sigma_abs_free ~ normal(0,5);
              sigma_reg_free ~ normal(0,5);
            } else if(hier_type==4) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(0,5);
            } else if(hier_type==5) {
              sigma_reg_free ~ normal(0,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            } else if(hier_type==7) {
              sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
              sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
            }
      } else {
        //legis the same for all other hier_type combinations
          restrict_high[1] ~ normal(pin_vals,.1);

          L_free[1] ~ normal(0,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              restrict_high[t] ~ normal(pin_vals,.1);
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
          
          //do all the other hier_type combinations for when legis is constrained. none of these affect hier_type in legis

         if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(0,5);
         } else if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
         } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          sigma_reg_free ~ normal(0,5); 
         }
      }
      
    }
    //finished constrain_par==1, on to constrain_par==2 (absence discrimination)
  } else if(constrain_par==2) {
    //now work through all combinations with absences constrained
    if(constraint_type==1) {
      if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_abs_free ~ normal(sax_pred[num_bills-num_constrain_sa,]*sigma_abs_x,5);
        restrict_low[1] ~ normal(sax_pred[(num_bills-num_constrain_sa+1):num_legis,]*sigma_abs_x_cons,5);
        if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==5) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_reg_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_low[1] ~ normal(0,5);
        sigma_abs_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_reg_free ~ normal(0,5);
        } else if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==4) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==2 and constraint_type==1
    } else if(constraint_type==2) {
      //copy and change constraints for all other constraint_type combinations
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_abs_free ~ normal(sax_pred[num_bills-num_constrain_sa,]*sigma_abs_x,5);
        restrict_high[1] ~ normal(sax_pred[(num_bills-num_constrain_sa+1):num_bills,]*sigma_abs_x_cons,5);
        if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==5) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_reg_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(0,5);
        sigma_abs_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_reg_free ~ normal(0,5);
        } else if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==4) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==2 and constraint_type==2
    } else if(constraint_type==3) {
      //copy and change constraints for all other constraint_type combinations
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_abs_free ~ normal(sax_pred[num_bills-num_constrain_sa,]*sigma_abs_x,5);
        restrict_high[1] ~ normal(sax_pred[(num_bills-(2*num_constrain_sa)+1):(num_bills-num_constrain_sa),]*sigma_abs_x_cons,5);
        restrict_low[1] ~ normal(sax_pred[(num_bills-num_constrain_sa+1):num_bills,]*sigma_abs_x_cons,5);
        if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==5) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_reg_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(0,5);
        restrict_low[1] ~normal(0,5);
        sigma_abs_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_reg_free ~ normal(0,5);
        } else if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==4) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==2 and constraint_type==3    
    } else if(constraint_type==4) {
      //copy and change constraints for all other constraint_type combinations
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_abs_free ~ normal(sax_pred[num_bills-num_constrain_sa,]*sigma_abs_x,5);
        restrict_high[1] ~ normal(pin_vals,.1);
        if(hier_type==3) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==5) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_reg_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(pin_vals,.1);
        sigma_abs_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_reg_free ~ normal(0,5);
        } else if(hier_type==2) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==4) {
          sigma_reg_free ~ normal(srx_pred*sigma_reg_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_reg_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==2 and constraint_type==4
    }
    //finished constrain_par==2
  } else if(constrain_par==3) {
    //for this one, we can just copy and paste over the absence discrim code and change to regular discriminations
      //copy and change constraints for all other constraint_type combinations
      if(constraint_type==1) {
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_reg_free ~ normal(srx_pred[num_bills-num_constrain_sr,]*sigma_reg_x,5);
        restrict_high[1] ~ normal(srx_pred[(num_bills-num_constrain_sr+1):num_bills,]*sigma_reg_x_cons,5);
        if(hier_type==2) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==4) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_abs_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(0,5);
        sigma_reg_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_abs_free ~ normal(0,5);
        } else if(hier_type==3) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==5) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==3 and constraint_type==2
    } else if(constraint_type==3) {
      //copy and change constraints for all other constraint_type combinations
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_reg_free ~ normal(srx_pred[num_bills-num_constrain_sr,]*sigma_reg_x,5);
        restrict_high[1] ~ normal(srx_pred[(num_bills-(2*num_constrain_sr)+1):(num_bills-num_constrain_sr),]*sigma_reg_x_cons,5);
        restrict_low[1] ~ normal(srx_pred[(num_bills-num_constrain_sr+1):num_bills,]*sigma_reg_x_cons,5);
        if(hier_type==2) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==4) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_abs_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(0,5);
        restrict_low[1] ~normal(0,5);
        sigma_reg_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 3,5,6 or 7 but rather 1,2,4,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_abs_free ~ normal(0,5);
        } else if(hier_type==3) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==5) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==3 and constraint_type==3    
    } else if(constraint_type==4) {
      //copy and change constraints for all other constraint_type combinations
        if(hier_type==3||hier_type==5||hier_type==6||hier_type==7) {
        //all combinations where hier_type parameters in absences
        sigma_reg_free ~ normal(srx_pred[num_bills-num_constrain_sr,]*sigma_reg_x,5);
        restrict_high[1] ~ normal(pin_vals,.1);
        if(hier_type==2) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==4) {
            L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
            //add basic integrated time-series prior
            if(T>1) {
              for(t in 2:T) {
                L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
              }
            }
            sigma_abs_free ~ normal(0,5);
        } else if(hier_type==6) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        } else if(hier_type==7) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
        }
      } else {
        //all other hier_type combinations where absences d/n have hier_typearchical priors
        restrict_high[1] ~ normal(pin_vals,.1);
        sigma_reg_free ~ normal(0,5);
        //run through remaining hier_type combinations--not 2,4,6 or 7 but rather 1,3,5,8
        if(hier_type==1) {
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }
          sigma_abs_free ~ normal(0,5);
        } else if(hier_type==3) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          }           
        } else if(hier_type==5) {
          sigma_abs_free ~ normal(sax_pred*sigma_abs_x,5);
          L_free[1] ~ normal(legis_pred[1,,]*legis_x,1);
          //add basic integrated time-series prior
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1] + legis_pred[t,,]*legis_x,1);
            }
          }        
        } else if(hier_type==8) {
          sigma_abs_free ~ normal(0,5);
          L_free[1] ~ normal(0,1);
          if(T>1) {
            for(t in 2:T) {
              L_free[t] ~ normal(L_free[t-1],1);
            }
          } 
        }
      }
      //all done with constrain_par==3 and constraint_type==4
    }
    //finished constrain_par==3
  }
