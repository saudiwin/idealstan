/*
Chop up the parameter vector to be identified
By constraining two parameters to almost-fixed values (very low SD)
*/
real id_params2(vector p, array[] int high, array[] int low, 
                  real fix_high, 
                  real fix_low,
                  real sd_fix_high,
                  real sd_fix_low,
                  real mean_val,
                  real scale,
                  real shape) {
    
    int N = num_elements(p);
    real prob_dens = 0; // hold the calculated probability density
    
    for(n in 1:N) {
      
      if(r_in(n,high)) {
        
        prob_dens += genbeta_lpdf(p[n]|1000,0.1,-1,2);
        
      } else if(r_in(n,low)) {
        
        prob_dens += genbeta_lpdf(p[n]|0.1,1000,-1,2);
        
      } else {
        
        prob_dens += genbeta_lpdf(p[n]|scale,shape,-1,2); 
        
      }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
    
 