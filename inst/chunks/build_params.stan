if(constrain_par==1) {
  if(constraint_type==1) {
    //if legislators are constrained, we must take into account the potential time series 
    //dimension
    //if T=1, of course, this is just boilerplate
      L_full = append_row(L_free,restrict_low);
      
  } else if(constraint_type==2) {
    
      L_full = append_row(L_free,restrict_high);
      
  } else if(constraint_type==3) {

      L_full = append_row(L_free,append_row(restrict_high,restrict_low));

  } else if(constraint_type==4) {

      L_full = append_row(L_free,pinned_pars);

  }
  sigma_abs_full=sigma_abs_free;
  sigma_reg_full=sigma_reg_free;
} else if(constrain_par==2) {
  if(constraint_type==1) {
    sigma_abs_full = append_row(sigma_abs_free,restrict_low);
  } else if(constraint_type==2) {
    sigma_abs_full = append_row(sigma_abs_free,restrict_high);
  } else if(constraint_type==3) {
    sigma_abs_full = append_row(sigma_abs_free,append_row(restrict_high,restrict_low));
  } else if(constraint_type==4) {
    sigma_abs_full = append_row(sigma_abs_free,pinned_pars);
  }
  L_full=L_free;
  sigma_reg_full=sigma_reg_free;
} else if(constrain_par==3) {
  if(constraint_type==1) {
    sigma_reg_full = append_row(sigma_reg_free,restrict_low);
  } else if(constraint_type==2) {
    sigma_reg_full = append_row(sigma_reg_free,restrict_high);
  } else if(constraint_type==3) {
    sigma_reg_full = append_row(sigma_reg_free,append_row(restrict_high,restrict_low));
    //sigma_reg_full=append_row(sigma_reg_free,restrict_ord[1]);
  } else if(constraint_type==4) {
    sigma_reg_full = append_row(sigma_reg_free,pinned_pars);
  }
  sigma_abs_full=sigma_abs_free;
  L_full = L_free;
}
