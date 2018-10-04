
// Version 9: only one kind of constraint  possible for discrimination
// We include diff/2 as an offset as we want this parameter to be halfway past 0 
// need to adjust parameters to reflect the reality that we are sampling on the log scale
if(T==1) {
      restrict_high ~normal((diff_high - restrict_high_bar) + 
                  legis_pred[1, num_legis, ] * legis_x_cons, 
                    restrict_sd);
} else {
        restrict_high ~normal((diff_high - restrict_high_bar) + 
                  legis_pred[1, num_legis, ] * legis_x_cons, 
                    restrict_sd);
}



  sigma_abs_free~normal(sax_pred[num_bills, ] * sigma_abs_x, discrim_abs_sd);
  sigma_reg_free~normal(srx_pred[num_bills, ] * sigma_reg_x, discrim_reg_sd);
  


