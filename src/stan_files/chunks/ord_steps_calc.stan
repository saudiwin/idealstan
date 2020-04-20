// need separate calculation of priors for each type of ordinal outcome

if(n_cats_rat[1]>1) {

    vector[n_cats_rat[1]-1] spacing;
    for(i in 1:(n_cats_rat[1]-1))
      spacing[i] = -5 + (10/n_cats_rat[1])*i;
      
    //steps_votes3[1] ~ normal(spacing[1],5);
    
    for(i in 1:(n_cats_rat[1]-2))
        steps_votes3[i+1] - steps_votes3[i] ~ normal(0,5);  

} 

if(n_cats_rat[2]>1) {

    vector[n_cats_rat[2]-1] spacing;
    for(i in 1:(n_cats_rat[2]-1))
      spacing[i] = -5 + (10/n_cats_rat[2])*i;
      
    //steps_votes3[1] ~ normal(spacing[1],5);
    
    for(i in 1:(n_cats_rat[2]-2))
        steps_votes4[i+1] - steps_votes4[i] ~ normal(0,5);   


} 

if(n_cats_rat[3]>1) {

    vector[n_cats_rat[3]-1] spacing;
    for(i in 1:(n_cats_rat[3]-1))
      spacing[i] = -5 + 10/n_cats_rat[3]*i;
      
    steps_votes5[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[3]-2))
        steps_votes5[i+1] - steps_votes5[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


} 

if(n_cats_rat[4]>1) {

    vector[n_cats_rat[4]-1] spacing;
    for(i in 1:(n_cats_rat[4]-1))
      spacing[i] = -5 + 10/n_cats_rat[4]*i;
      
    steps_votes6[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[4]-2))
        steps_votes6[i+1] - steps_votes6[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


} 

if(n_cats_rat[5]>1) {

    vector[n_cats_rat[5]-1] spacing;
    for(i in 1:(n_cats_rat[5]-1))
      spacing[i] = -5 + 10/n_cats_rat[5]*i;
      
    steps_votes7[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[5]-2))
        steps_votes7[i+1] - steps_votes7[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


} 

if(n_cats_rat[6]>1) {

    vector[n_cats_rat[6]-1] spacing;
    for(i in 1:(n_cats_rat[6]-1))
      spacing[i] = -5 + 10/n_cats_rat[6]*i;
      
    steps_votes8[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[6]-2))
        steps_votes8[i+1] - steps_votes8[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


} 

if(n_cats_rat[7]>1) {

    vector[n_cats_rat[7]-1] spacing;
    for(i in 1:(n_cats_rat[7]-1))
      spacing[i] = -5 + 10/n_cats_rat[7]*i;
      
    steps_votes9[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[7]-2))
        steps_votes9[i+1] - steps_votes9[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


} 

if(n_cats_rat[8]>1) {

    vector[n_cats_rat[8]-1] spacing;
    for(i in 1:(n_cats_rat[8]-1))
      spacing[i] = -5 + 10/n_cats_rat[8]*i;
      
    steps_votes10[1] ~ normal(spacing[1],3);
    
    for(i in 1:(n_cats_rat[8]-2))
        steps_votes10[i+1] - steps_votes10[i] ~ exponential(1/(spacing[i+1] - spacing[i]));  


}

if(num_bills_grm>1) {
  if(n_cats_grm[1]>1) {
    vector[n_cats_grm[1]-1] spacing;
    for(i in 1:(n_cats_grm[1]-1))
      spacing[i] = -5 + 10/n_cats_grm[1]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm3[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[1]-2))
            steps_votes_grm3[b,i+1] - steps_votes_grm3[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[2]>1) {
    vector[n_cats_grm[2]-1] spacing;
    for(i in 1:(n_cats_grm[2]-1))
      spacing[i] = -5 + 10/n_cats_grm[2]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm4[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[2]-2))
            steps_votes_grm4[b,i+1] - steps_votes_grm4[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[3]>1) {
    vector[n_cats_grm[3]-1] spacing;
    for(i in 1:(n_cats_grm[3]-1))
      spacing[i] = -5 + 10/n_cats_grm[3]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm5[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[3]-2))
            steps_votes_grm5[b,i+1] - steps_votes_grm5[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[4]>1) {
    vector[n_cats_grm[4]-1] spacing;
    for(i in 1:(n_cats_grm[4]-1))
      spacing[i] = -5 + 10/n_cats_grm[4]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm6[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[4]-2))
            steps_votes_grm6[b,i+1] - steps_votes_grm6[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[5]>1) {
    vector[n_cats_grm[5]-1] spacing;
    for(i in 1:(n_cats_grm[5]-1))
      spacing[i] = -5 + 10/n_cats_grm[5]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm7[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[5]-2))
            steps_votes_grm7[b,i+1] - steps_votes_grm7[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[6]>1) {
    vector[n_cats_grm[6]-1] spacing;
    for(i in 1:(n_cats_grm[6]-1))
      spacing[i] = -5 + 10/n_cats_grm[6]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm8[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[6]-2))
            steps_votes_grm8[b,i+1] - steps_votes_grm8[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[7]>1) {
    vector[n_cats_grm[7]-1] spacing;
    for(i in 1:(n_cats_grm[7]-1))
      spacing[i] = -5 + 10/n_cats_grm[7]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm9[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[7]-2))
            steps_votes_grm9[b,i+1] - steps_votes_grm9[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 
  if(n_cats_grm[8]>1) {
    vector[n_cats_grm[8]-1] spacing;
    for(i in 1:(n_cats_grm[8]-1))
      spacing[i] = -5 + 10/n_cats_grm[8]*i;
    for(b in 1:num_bills_grm) {
      // weakly informative prior over the range that we expect the latent variable to fall in
          steps_votes_grm10[b,1] ~ normal(spacing[1],3);
          for(i in 1:(n_cats_grm[8]-2))
            steps_votes_grm10[b,i+1] - steps_votes_grm10[b,i] ~ exponential(1/(spacing[i+1] - spacing[i]));
              
      }
  } 

}
