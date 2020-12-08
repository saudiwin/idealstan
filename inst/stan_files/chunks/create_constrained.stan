//need to figure out correct number of rows to reserve for constrained parameters
//if a vector of parameters has none, set to zero
//constraint_type 1 == low, 2==high, 3==high/low,4==pin
//constrain_par 1==legis,2==sigma abs, 3==sigma reg


if(num_legis==2) {
  num_constrain_l=1;
} else {
  if(T==1) {
    num_constrain_l=2;
  } else {
    if(time_proc==3) {
      num_constrain_l=2;
    } else {
      num_constrain_l=2;
    }
    
  }
  
}


