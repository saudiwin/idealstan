  /* Restrict_type reflects what kind of identification strategy is used.
  1 = constrain persons
  2 = constrain absence discrimination
  3 = constrain regular discrimination 
  Bidirect reflects whether parameters should be constrained on both the high and low end of the scale*/

  if(restrict_type==1 & bidirect==0) {

  } else if(restrict_type==1 & bidirect==1) {

  } else if(restrict_type==2 & bidirect==0) {

  } else if(restrict_type==2 & bidirect==1) {

  } else if(restrict_type==3 & bidirect==0) {

  } else if(restrict_type==3 & bidirect==1) {

  }
  
  // parameters
  
    if(restrict_type==1 & bidirect==0) {
      vector[num_legis-restrict] L_free;
      vector<upper=0>[restrict] L_restrict;
      vector[num_bills] sigma_full;
      vector[num_bills] sigma_abs_full;
  } else if(restrict_type==1 & bidirect==1) {
    vector[num_legis-(restrict*2)] L_free;
    vector<lower=0>[restrict] L_restrict_high;
    vector<upper=0>[restrict] L_restrict_low;
      vector[num_bills] sigma_full;
      vector[num_bills] sigma_abs_full;
  } else if(restrict_type==2 & bidirect==0) {
    vector[num_bills-restrict] sigma_free;
    vector<upper=0>[restrict] sigma_restrict;
    vector[num_legis] L_full;
    vector[num_bills] sigma_abs_full;
  } else if(restrict_type==2 & bidirect==1) {
    vector[num_bills-(restrict*2)] sigma_free;
    vector<upper=0>[restrict] sigma_restrict_low;
    vector<lower=0>[restrict] sigma_restrict_high;
    vector[num_legis] L_full;
    vector[num_bills] sigma_abs_full;
  } else if(restrict_type==3 & bidirect==0) {
    vector[num_bills-restrict] sigma_abs_free;
    vector<upper=0>[restrict] sigma_abs_restrict;
    vector[num_legis] L_full;
    vector[num_bills] sigma_full;
  } else if(restrict_type==3 & bidirect==1) {
    vector[num_bills-(restrict*2)] sigma_abs_free;
    vector<upper=0>[restrict] sigma_abs_restrict_low;
    vector<lower=0>[restrict] sigma_abs_restrict_high;
    vector[num_legis] L_full;
    vector[num_bills] sigma_full;
  }
  
  // transformed parameters
  
    if(restrict_type==1 & bidirect==0) {
    vector[num_legis] L_full;
    L_full = append_row(L_free,L_restrict);
  } else if(restrict_type==1 & bidirect==1) {
    vector[num_legis] L_full;
    L_full = append_row(L_free,L_restrict_high,L_restrict_low);
  } else if(restrict_type==2 & bidirect==0) {
    vector[num_bills] sigma_full;
    sigma_full = append_row(sigma_free,sigma_restrict);
  } else if(restrict_type==2 & bidirect==1) {
    vector[num_bills] sigma_full;
    sigma_full = append_row(sigma_free,sigma_restrict_high,sigma_restrict_low);
  } else if(restrict_type==3 & bidirect==0) {
    vector[num_bills] sigma_abs_full;
    sigma_abs_full = append_row(sigma_abs_free,sigma_abs_restrict);
  } else if(restrict_type==3 & bidirect==1) {
    vector[num_bills] sigma_abs_full;
    sigma_abs_full = append_row(sigma_abs_free,sigma_abs_restrict_high,sigma_abs_restrict_low);
  }
