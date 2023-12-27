/*
Chop up the parameter vector to be identified
By constraining two parameters to almost-fixed values (very low SD)
*/
real id_params(vector p, array[] int high, array[] int low, 
                  real fix_high, 
                  real fix_low,
                  real sd_fix_high,
                  real sd_fix_low,
                  real mean_val,
                  real sd_val) {
    
    int N = num_elements(p);
    real prob_dens = 0; // hold the calculated probability density
    
    for(n in 1:N) {
      
      if(r_in(n,high)) {
        
        prob_dens += normal_lpdf(p[n]|fix_high,sd_fix_high);
        
      } else if(r_in(n,low)) {
        
        prob_dens += normal_lpdf(p[n]|fix_low,sd_fix_low);
        
      } else {
        
        prob_dens += normal_lpdf(p[n]|mean_val,sd_val);
        
      }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
    
 