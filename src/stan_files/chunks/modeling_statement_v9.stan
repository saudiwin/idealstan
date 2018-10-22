
// Version 9: only one kind of constraint  possible for discrimination
// We include diff_high as an offset as we want this parameter to be halfway past 0 

        restrict_high ~normal(diff_high + 
                  legis_pred[1, num_legis, ] * legis_x_cons, 
                    restrict_sd);



  sigma_abs_free~normal(sax_pred[num_bills, ] * sigma_abs_x, discrim_abs_sd);
  sigma_reg_free~normal(srx_pred[num_bills, ] * sigma_reg_x, discrim_reg_sd);
  


  
  
  



