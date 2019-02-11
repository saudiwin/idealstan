/*
Calculate length-scale prior for gaussian processes
takes one user input to determine number of time points to calculate maximum difference

*/
  vector gp_prior_mean(real[] x,int num_diff,int min_length) {
    vector[num_elements(x)-1] diff_elem;
    vector[2] data_out;
    real rl_mean;
    real lower_limit;
    
    for(n in 1:(num_elements(x)-1)) {
          diff_elem[n] = fabs(x[n+1] - x[n]);
      }
      
    rl_mean = mean(diff_elem)*num_diff;
    
    lower_limit=min(diff_elem)*min_length;
    return [log(rl_mean),lower_limit]';
  }
    
 