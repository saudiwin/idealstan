/*
Chop up the parameter vector to be identified
By constraining two parameters to almost-fixed values (very low SD)
*/
real id_params(vector p, array[] int high, array[] int low, 
                  array[] real fix_high, 
                  array[] real fix_low,
                  array[] real sd_fix_high,
                  array[] real sd_fix_low,
                  real mean_val,
                  real sd_val) {
    
    int N = num_elements(p);
    real prob_dens = 0; // hold the calculated probability density
    int fix_high_count = 1;
    int fix_low_count = 1;
    
    for(n in 1:N) {
      
      if(r_in(n,high)) {
        
        prob_dens += normal_lpdf(p[n]|fix_high[fix_high_count],
        sd_fix_high[fix_high_count]);
        
        fix_high_count = fix_high_count + 1;
        
      } else if(r_in(n,low)) {
        
        prob_dens += normal_lpdf(p[n]|fix_low[fix_low_count],
        sd_fix_low[fix_low_count]);
        
        fix_low_count = fix_low_count + 1;
        
      } else {
        
        prob_dens += normal_lpdf(p[n]|mean_val,sd_val);
        
      }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
    
 