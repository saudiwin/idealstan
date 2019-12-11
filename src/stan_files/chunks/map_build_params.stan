if(S_type==1) {
      // map over persons
      
      for(s in 1:S) {
        if(T==1) {
          varparams[s] = [L_full[s]]';
        } else {
          varparams[s] = to_vector(L_tp1[1:T,s]);
        }
        
      } 
      
      dparams[1:(dP-(sum(n_cats_grm)-8)*num_bills_grm)] = append_row(sigma_reg_free,append_row(B_int_free,
                                           append_row(sigma_abs_free,
                                           append_row(A_int_free,
                                           append_row(ls_int,
                                           append_row(steps_votes3,
                                           append_row(steps_votes4,
                                           append_row(steps_votes5,
                                           append_row(steps_votes6,
                                           append_row(steps_votes7,
                                           append_row(steps_votes8,
                                           append_row(steps_votes9,
                                           append_row(steps_votes10,
                                           append_row(legis_x,
                                           append_row(sigma_reg_x,
                                           append_row(sigma_abs_x,extra_sd))))))))))))))));
      
    } else {
      // map over items 
      
      for(s in 1:S) {
        
        varparams[s] = [sigma_reg_free[s],B_int_free[s],sigma_abs_free[s],A_int_free[s]]';
        
      } 
      
      dparams[1:(dP-(sum(n_cats_grm)-8)*num_bills_grm -num_legis*T)] = append_row(ls_int,
                                           append_row(steps_votes3,
                                           append_row(steps_votes4,
                                           append_row(steps_votes5,
                                           append_row(steps_votes6,
                                           append_row(steps_votes7,
                                           append_row(steps_votes8,
                                           append_row(steps_votes9,
                                           append_row(steps_votes10,
                                           append_row(legis_x,
                                           append_row(sigma_reg_x,
                                           append_row(sigma_abs_x,extra_sd))))))))))));
      
    }
    
    // for graded response, we have to manually iterate through each one
  {  
    int skip; // local variable to make computation simpler
    if(S_type==1) {
      skip = (dP-(sum(n_cats_grm)-8)*num_bills_grm);
    } else {
      skip = (dP-(sum(n_cats_grm)-8)*num_bills_grm - num_legis*T);
    }
    
    if(n_cats_grm[1]-1>0) {
      for(c in 1:3) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm3[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[2]-1>0) {
      for(c in 1:4) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm4[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[3]-1>0) {
      for(c in 1:5) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm5[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[4]-1>0) {
      for(c in 1:6) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm6[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[5]-1>0) {
      for(c in 1:7) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm7[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[6]-1>0) {
      for(c in 1:8) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm8[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[7]-1>0) {
      for(c in 1:9) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm9[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    if(n_cats_grm[8]-1>0) {
      for(c in 1:10) {
        dparams[(skip+1):(skip+num_bills_grm)] = steps_votes_grm10[c,1:num_bills_grm];
        skip += num_bills_grm;
      }
    }
    
    // if we vectorizae over items, add person parameters to the end of the vector
    
                // need to determine whether we need to vectorize over time points
    if(S_type==0) {
      if(T>1) {
              for(l in 1:num_legis) {
                dparams[(skip+1):(skip+T)] = to_vector(L_tp1[1:T,l]);
                skip += T;
              }
            } else {
              dparams[(skip+1):(skip+num_legis)] = L_full;
            }
    }
    // end of local scope
  }

            
    