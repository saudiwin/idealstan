
  // only add the log probability of the restricted parameter if that is what we are also mapping over

  if(const_type==1 && S_type==1) {
    // determine whether mapped parameter is restricted
    
    if(ll[1]==restrict_high) {
      log_prob += normal_lpdf(L_full|fix_high,restrict_sd);
    } else if(ll[1]==restrict_low) {
      log_prob += normal_lpdf(L_full|fix_low,restrict_sd);
    } else {
      log_prob += normal_lpdf(L_full|0,legis_sd);
    }
                        
                      
  } else if(S_type==0 && const_type==2) {
    
    if(bb[1]==restrict_high) {
      log_prob += normal_lpdf(sigma_reg_free|fix_high,restrict_sd);
    } else if(bb[1]==restrict_low) {
      log_prob += normal_lpdf(sigma_reg_free|fix_low,restrict_sd);
    } else {
      log_prob += normal_lpdf(sigma_reg_free|0,discrim_reg_sd);
    }
    
    
  }

  //print(L_full);
  //print(log_prob);
  



