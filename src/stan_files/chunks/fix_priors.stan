

  if(const_type==1) {
    target += id_params(L_full,
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        sd_fix,
                        0,
                        legis_sd);
                        
    sigma_reg_free~normal(0, discrim_reg_sd);
                        
  } else {
    target += id_params(sigma_reg_free,
                        restrict_high,
                        restrict_low,
                        fix_high,
                        fix_low,
                        sd_fix,
                        0,
                        discrim_reg_sd);
                        
    L_full ~ normal(0, legis_sd);
  }
        
  // no need to constrain absence (missing) discrimination
  sigma_abs_free~normal(0, discrim_abs_sd);
  
  


  
  
  



