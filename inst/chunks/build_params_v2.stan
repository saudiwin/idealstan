    
    if(num_legis==2) {
      L_full = append_row(restrict_high,restrict_low);
    } else {
      L_full = append_row(L_free,append_row(restrict_high,restrict_low));
    }
    

    sigma_abs_full=sigma_abs_free;
    sigma_reg_full=sigma_reg_free;
