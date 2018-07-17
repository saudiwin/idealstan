/*
  hier_type_type:1=L,2=SRX,3=SAX,4=L+SRX,5=L+SAX,6=SRX+SAX,7=SRX+SAX+L,8=none
  Only need to check for AR in situations using L as a constraint
  This would be better structured as a header file
  */
  
// Version 8: we assume all parameters have hiearchical parameters, and they are multiplied by zero if
// they don't exist properly

if(constrain_par==1) {
  //different options for legislator/person constraints
  
  if(constraint_type==1) {
      restrict_low ~normal(legis_pred[1, (num_legis - num_constrain_l + 1):num_legis, ] * legis_x_cons, 
        restrict_sd);
      

  } else if(constraint_type==2) {
    restrict_high ~normal(legis_pred[1, (num_legis - num_constrain_l + 1):num_legis, ] * legis_x_cons, 
      restrict_sd);
      
  } else if(constraint_type==3) {
    restrict_high ~normal(legis_pred[1, (num_legis - (2 * num_constrain_l) + 1):(num_legis - num_constrain_l), ] * legis_x_cons, 
      restrict_sd);
      restrict_low ~normal(legis_pred[1, (num_legis - num_constrain_l + 1):num_legis, ] * legis_x_cons, restrict_sd);
      
  } 

} else if(constrain_par==2) {
  if(constraint_type==1) {
    restrict_low ~normal(sax_pred[(num_bills - num_constrain_sa + 1):num_legis, ] * sigma_abs_x_cons, restrict_sd);
  } else if(constraint_type==2) {
    restrict_high ~normal(sax_pred[(num_bills - num_constrain_sa + 1):num_bills, ] * sigma_abs_x_cons, restrict_sd);
  } else if(constraint_type==3) {
    restrict_high ~normal(sax_pred[(num_bills - (2 * num_constrain_sa) + 1):(num_bills - num_constrain_sa), ] * sigma_abs_x_cons, restrict_sd);
    restrict_low ~normal(sax_pred[(num_bills - num_constrain_sa + 1):num_bills, ] * sigma_abs_x_cons, restrict_sd);
  } else if(constrain_par==3) {
    if(constraint_type==1) {
      restrict_low ~normal(srx_pred[(num_bills - num_constrain_sr + 1):num_bills, ] * sigma_reg_x_cons, restrict_sd);
    } else if(constraint_type==2) {
      restrict_high ~normal(srx_pred[(num_bills - num_constrain_sr + 1):num_bills, ] * sigma_reg_x_cons, restrict_sd);
    } else if(constraint_type==3) {
      restrict_high ~normal(srx_pred[(num_bills - (2 * num_constrain_sr) + 1):(num_bills - num_constrain_sr), ] * sigma_reg_x_cons, restrict_sd);
      restrict_low ~normal(srx_pred[(num_bills - num_constrain_sr + 1):num_bills, ] * sigma_reg_x_cons, restrict_sd);
    }
  }

  
}

  sigma_abs_free~normal(sax_pred[num_bills - num_constrain_sa, ] * sigma_abs_x, 10);
  sigma_reg_free~normal(srx_pred[num_bills - num_constrain_sr, ] * sigma_reg_x, 10);
  
  //use either random walk or AR(1) prior
  if(use_ar==0) {
    #include l_hier_prior.stan
  } else {
    #include l_hier_ar1_prior.stan
  }
  

// fix remaining priors

if (constraint_type != 4) {
  
    pinned_pars ~normal(0, 1);

}
if (constraint_type != 1 || constraint_type != 3) {
  
    restrict_low ~normal(0, 1);

}
if (constraint_type == 4 || constraint_type == 2) {

    restrict_high ~normal(0, 1);


}
