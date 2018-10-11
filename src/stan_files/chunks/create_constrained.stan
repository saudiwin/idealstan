//need to figure out correct number of rows to reserve for constrained parameters
//if a vector of parameters has none, set to zero
//constraint_type 1 == low, 2==high, 3==high/low,4==pin
//constrain_par 1==legis,2==sigma abs, 3==sigma reg


if(num_legis==2) {
  num_constrain_l=1;
} else {
  if(T==1) {
    num_constrain_l=num_fix_high + num_fix_low;
  } else {
    if(use_ar==1) {
      num_constrain_l=num_fix_high +  num_fix_low;
    } else {
      num_constrain_l=num_fix_high + num_fix_low;
    }
    
  }
  
}

// determine whether to restrict variance or not

if(restrict_var==1) {
  num_var_restrict=num_legis;
  num_var_free=0;
} else {
  num_var_restrict=0;
  num_var_free=num_legis;
}


