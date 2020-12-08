/*
Chop up the parameter vector to be identified
By constraining two parameters to almost-fixed values (very low SD)
*/
real id_params(vector p, int high, int low, 
                  real fix_high, 
                  real fix_low,
                  real sd_fix,
                  real mean_val,
                  real sd_val) {
    
    int N = num_elements(p);
    real prob_dens = 0; // hold the calculated probability density

    // use different types of indexing depending on placement of 
    // values to fix in the vector
    if(high>low) {
      
      if(low>1) {
        prob_dens += normal_lpdf(p[1:(low-1)]|mean_val,sd_val);
      }
      
      prob_dens += normal_lpdf(p[low]|fix_low,sd_fix);
      
        if(high>(low+1)) {
          prob_dens += normal_lpdf(p[(low+1):(high-1)]|mean_val,
                                    sd_val);
          }
          
      prob_dens += normal_lpdf(p[high]|fix_high,
                                        sd_fix);
                                        
        if(high<N) {
          prob_dens += normal_lpdf(p[(high+1):N]|mean_val,sd_val);
        }
      
      
      
    } else {

      if(high>1) {
        prob_dens += normal_lpdf(p[1:(high-1)]|mean_val,sd_val);
      }
      
      prob_dens += normal_lpdf(p[high]|fix_high,sd_fix);
      
        if(low>(high+1)) {
          prob_dens += normal_lpdf(p[(high+1):(low-1)]|mean_val,
                                    sd_val);
          }
          
      prob_dens += normal_lpdf(p[low]|fix_low,
                                        sd_fix);
        if(low<N) {
          prob_dens += normal_lpdf(p[(low+1):N]|mean_val,sd_val);
        }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
    
 