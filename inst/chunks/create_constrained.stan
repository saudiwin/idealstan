//need to figure out correct number of rows to reserve for constrained parameters
//if a vector of parameters has none, set to zero
//constraint_type 1 == low, 2==high, 3==high/low,4==pin
//constrain_par 1==legis,2==sigma abs, 3==sigma reg
if(constraint_type==1) {
  if(constrain_par==1) {
    num_constrain_l=num_fix_low;
    num_constrain_sr=0;
    num_constrain_sa=0;
  } else if(constrain_par==2) {
    num_constrain_l=0;
    num_constrain_sr=0;
    num_constrain_sa=num_fix_low;
  } else if(constrain_par==3) {
    num_constrain_l=0;
    num_constrain_sr=num_fix_low;
    num_constrain_sa=0;
  }
  
} else if(constraint_type==2 || constraint_type==4) {
  if(constrain_par==1) {
    num_constrain_l=num_fix_high;
    num_constrain_sr=0;
    num_constrain_sa=0;
  } else if(constrain_par==2) {
    num_constrain_l=0;
    num_constrain_sr=0;
    num_constrain_sa=num_fix_high;
  } else if(constrain_par==3) {
    num_constrain_l=0;
    num_constrain_sr=num_fix_high;
    num_constrain_sa=0;
  }
} else if(constraint_type==3) {
  if(constrain_par==1) {
    num_constrain_l=num_fix_high + num_fix_low;
    num_constrain_sr=0;
    num_constrain_sa=0;
  } else if(constrain_par==2) {
    num_constrain_l=0;
    num_constrain_sr=0;
    num_constrain_sa=num_fix_high + num_fix_low;
  } else if(constrain_par==3) {
    num_constrain_l=0;
    num_constrain_sr=num_fix_high + num_fix_low;
    num_constrain_sa=0;
  }
}
