  // rebuild ordered cat variables from the mapped parameters
  if(n_cats_rat[1]>1) {
    steps_votes3 = allparams[(skip_param+num_ls+1):(skip_param+num_ls+n_cats_rat[1]-1)];
  }
  
  if(n_cats_rat[2]>1) {
    steps_votes4 = allparams[(skip_param+num_ls+n_cats_rat[1]+1-1):(skip_param+num_ls+sum(n_cats_rat[1:2])-2)];
  }
  
  if(n_cats_rat[3]>1) {
    steps_votes5 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:2])-1):(skip_param+num_ls+sum(n_cats_rat[1:3])-3)];
  }
  
  if(n_cats_rat[4]>1) {
    steps_votes6 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:3])-2):(skip_param+num_ls+sum(n_cats_rat[1:4])-4)];
  }
  
  if(n_cats_rat[5]>1) {
    steps_votes7 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:4])-3):(skip_param+num_ls+sum(n_cats_rat[1:5])-5)];
  }
  
  if(n_cats_rat[6]>1) {
    steps_votes8 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:5])-4):(skip_param+num_ls+sum(n_cats_rat[1:6])-6)];
  }
  
  if(n_cats_rat[7]>1) {
    steps_votes9 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:6])-5):(skip_param+num_ls+sum(n_cats_rat[1:7])-7)];
  }
  
  if(n_cats_rat[8]>1) {
    steps_votes10 = allparams[(skip_param+num_ls+sum(n_cats_rat[1:7])-6):(skip_param+num_ls+sum(n_cats_rat)-8)];
  }
  
  // now do the same for the graded response variables
  // global scope to use counter across for loops
  // keeps code cleaner
  skip0 = (skip_param+num_ls+sum(n_cats_rat)-8+LX+SRX+SAX);
  
  if(n_cats_grm[1]>1) {
    for(c in 1:(n_cats_grm[1]-1)) {
      steps_votes_grm3[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[1]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[2]>1) {
    for(c in 1:(n_cats_grm[2]-1)) {
      steps_votes_grm4[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[2]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[3]>1) {
    for(c in 1:(n_cats_grm[3]-1)) {
      steps_votes_grm5[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[3]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[4]>1) {
    for(c in 1:(n_cats_grm[4]-1)) {
      steps_votes_grm6[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[4]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[5]>1) {
    for(c in 1:(n_cats_grm[5]-1)) {
      steps_votes_grm7[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[5]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[6]>1) {
    for(c in 1:(n_cats_grm[6]-1)) {
      steps_votes_grm8[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[6]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[7]>1) {
    for(c in 1:(n_cats_grm[7]-1)) {
      steps_votes_grm9[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[7]-1)*num_bills_grm;
  }
  
  if(n_cats_grm[8]>1) {
    for(c in 1:(n_cats_grm[8]-1)) {
      steps_votes_grm10[1:num_bills_grm,c] = to_array_1d(allparams[(skip0 + 1 + ((c-1)*num_bills_grm)):(skip0+(c*num_bills_grm))]);
    }
    skip0 += (n_cats_grm[8]-1)*num_bills_grm;
  }