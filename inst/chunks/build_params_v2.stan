if(constrain_par==1) {
  if(constraint_type==1) {
    //if legislators are constrained, we must take into account the potential time series 
    //dimension
    //if T=1, of course, this is just boilerplate
    for(t in 1:T) {
      L_full[t] = append_row(L_free[t],restrict_low[t]);
    }
  } else if(constraint_type==2) {
    for(t in 1:T) {
      L_full[t] = append_row(L_free[t],restrict_high[t]);
    }
  } else if(constraint_type==3) {
    for(t in 1:T) {
      L_full[t] = append_row(L_free[t],append_row(restrict_high[t],restrict_low[t]));
    }
  } else if(constraint_type==4) {
    for(t in 1:T) {
      L_full[t] = append_row(L_free[t],pinned_pars[t]);
    }
  }
  sigma_abs_full=append_row(1,sigma_abs_free);
  sigma_reg_full=sigma_reg_free;
} else if(constrain_par==2) {
  if(constraint_type==1) {
    sigma_abs_full = append_row(1,append_row(sigma_abs_free,restrict_low[1]));
  } else if(constraint_type==2) {
    sigma_abs_full = append_row(1,append_row(sigma_abs_free,restrict_high[1]));
  } else if(constraint_type==3) {
    sigma_abs_full = append_row(1,append_row(sigma_abs_free,append_row(restrict_high[1],restrict_low[1])));
  } else if(constraint_type==4) {
    sigma_abs_full = append_row(1,append_row(sigma_abs_free,pinned_pars[1]));
  }
  L_full=L_free;
  sigma_reg_full=sigma_reg_free;
} else if(constrain_par==3) {
  if(constraint_type==1) {
    sigma_reg_full = append_row(sigma_reg_free,restrict_low[1]);
  } else if(constraint_type==2) {
    sigma_reg_full = append_row(sigma_reg_free,restrict_high[1]);
  } else if(constraint_type==3) {
    sigma_reg_full = append_row(sigma_reg_free,append_row(restrict_high[1],restrict_low[1]));
  } else if(constraint_type==4) {
    sigma_reg_full = append_row(sigma_reg_free,pinned_pars[1]);
  }
  sigma_abs_full=append_row(1,sigma_abs_free);
  L_full = L_free;
}
