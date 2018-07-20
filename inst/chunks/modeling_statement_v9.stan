
// Version 9: only one kind of constraint  possible for discrimination

    restrict_high ~normal(legis_pred[1, (num_legis - num_fix_high + 1):num_legis, ] * legis_x_cons, 
      restrict_sd);


  sigma_abs_free~normal(sax_pred[num_bills, ] * sigma_abs_x, 5);
  sigma_reg_free~normal(srx_pred[num_bills, ] * sigma_reg_x, 5);
  
  //use either random walk or AR(1) prior
  if(use_ar==0) {
    #include l_hier_prior.stan
  } else {
    #include l_hier_ar1_prior.stan
  }
  

