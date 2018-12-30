/*
Calculate length-scale prior for gaussian processes
takes one user input to determine number of time points to calculate maximum difference

*/
  vector gp_prior_mean(real[] x,int num_diff) {
    vector[num_elements(x)-1] diff_elem;
    vector[2] data_out;
    real rl_mean;
    real lower_limit;
    
    for(n in 1:(num_elements(x)-1)) {
          diff_elem[n] = fabs(x[n+1] - x[n]);
      }
    
    if(num_diff>num_elements(x)) {
      print("You can't select a number of elements to calculate the length-scale GP prior that is greater than the number of unique time points. Using the median time index value.")
      
      rl_mean = mean(diff_elem)*num_elements(x);
      
    } else {
      
      rl_mean = mean(diff_elem)*num_diff;
    }
    
    lower_limit=min(diff_elem)*2;
    return [rl_mean,lower_limit]';
  }
    
 