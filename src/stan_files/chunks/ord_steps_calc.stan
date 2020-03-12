// need separate calculation of priors for each type of ordinal outcome

if(n_cats_rat[1]>1) {

    steps_votes3 ~ normal(0,5);

} 

if(n_cats_rat[2]>1) {

    steps_votes4 ~ normal(0,5);

} 

if(n_cats_rat[3]>1) {

    steps_votes5 ~ normal(0,5);

} 

if(n_cats_rat[4]>1) {

    steps_votes6 ~ normal(0,5);

} 

if(n_cats_rat[5]>1) {

   steps_votes7 ~ normal(0,5);

} 

if(n_cats_rat[6]>1) {

    steps_votes8 ~ normal(0,5);

} 

if(n_cats_rat[7]>1) {

    steps_votes9 ~ normal(0,5);

} 

if(n_cats_rat[8]>1) {

    steps_votes10 ~ normal(0,5);

}

if(num_bills_grm>1) {
  if(n_cats_grm[1]>1) {
    for(b in 1:num_bills_grm) {
        for(i in 1:(n_cats_grm[1]-2)) {
          print(n_cats_grm[1]-2);
          print(i);
          //steps_votes_grm3[b,i+1] -  steps_votes_grm3[b,i] ~ normal(0,5);
        }
      }
  } 
  if(n_cats_grm[2]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm4[b]) ~ normal(0,5);
    }
  } 
  if(n_cats_grm[3]>1) {
    for(b in 1:num_bills_grm) {
      for(i in 1:(n_cats_grm[3]-2)) {
        print(n_cats_grm[3]-2);
          print(i);
        //steps_votes_grm5[b,i+1] -  steps_votes_grm5[b,i] ~ normal(0,5);
      }
    }
  } 
  if(n_cats_grm[4]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm6[b]) ~ normal(0,5);
    }
  } 
  if(n_cats_grm[5]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm7[b]) ~ normal(0,5);
    }
  } 
  if(n_cats_grm[6]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm8[b]) ~ normal(0,5);
    }
  } 
  if(n_cats_grm[7]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm9[b]) ~ normal(0,5);
    }
  } 
  if(n_cats_grm[8]>1) {
    for(b in 1:num_bills_grm) {
     to_vector(steps_votes_grm10[b]) ~ normal(0,5);
    }
  } 

}
