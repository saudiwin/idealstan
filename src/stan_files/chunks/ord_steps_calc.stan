// need separate calculation of priors for each type of ordinal outcome

if(n_cats_rat[1]>1) {
  for(i in 1:(n_cats_rat[1]-2)) {
    steps_votes3[i+1] - steps_votes3[i] ~ normal(0,5);
    }
} else if(n_cats_rat[2]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes4[i+1] - steps_votes4[i] ~ normal(0,5);
    }
} else if(n_cats_rat[3]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes5[i+1] - steps_votes5[i] ~ normal(0,5);
    }
} else if(n_cats_rat[4]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes6[i+1] - steps_votes6[i] ~ normal(0,5);
    }
} else if(n_cats_rat[5]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes7[i+1] - steps_votes7[i] ~ normal(0,5);
    }
} else if(n_cats_rat[6]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes8[i+1] - steps_votes8[i] ~ normal(0,5);
    }
} else if(n_cats_rat[7]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes9[i+1] - steps_votes9[i] ~ normal(0,5);
    }
} else if(n_cats_rat[8]>1) {
  for(i in 1:(n_cats_rat[2]-2)) {
    steps_votes10[i+1] - steps_votes10[i] ~ normal(0,5);
    }
}
if(num_bills_grm>1) {
  if(n_cats_grm[1]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm3[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[2]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm4[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[3]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm5[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[4]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm6[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[5]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm7[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[6]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm8[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[7]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm9[b] ~ normal(0,5);
    }
  } else if(n_cats_grm[8]>1) {
    for(b in 1:num_bills) {
      steps_votes_grm10[b] ~ normal(0,5);
    }
  } 

}
