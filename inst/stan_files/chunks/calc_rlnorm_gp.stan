/*
Calculate length-scale prior for gaussian processes
takes one user input to determine number of time points to calculate maximum difference

*/
  vector gp_prior_mean(array[] real x,real num_diff,int min_length) {
    vector[num_elements(x)-1] diff_elem;
    vector[2] data_out;
    real rl_mean;
    
    
    for(n in 1:(num_elements(x)-1)) {
          diff_elem[n] = abs(x[n+1] - x[n]);
      }
      
    rl_mean = mean(diff_elem)*num_diff;
    
    return [log(rl_mean)]';
  }
    
 