/*
Chop up the parameter vector to be identified
By constraining two parameters to almost-fixed values (very low SD)
*/
real id_params2(vector p, array[] int high, array[] int low, 
                  array[] real fix_high, 
                  array[] real fix_low,
                  array[] real sd_fix_high,
                  array[] real sd_fix_low,
                  real mean_val,
                  real scale,
                  real shape,
                  real upb,
                  real lb,
                  array[] real restrict_N_high,
                  array[] real restrict_N_low,
                  int debug_mode) {
    
    int N = num_elements(p);
    int num_high = num_elements(high);
    int num_low = num_elements(low);
    int count_high = 1;
    int count_low = 1;
    real prob_dens = 0; // hold the calculated probability density
    
    for(n in 1:N) {
      
      if(r_in(n,high)) {
        
        if(debug_mode==1) print("Adding genbeta_lpdf(p[n]|restrict_N_high,sd_fix_high,lb,upb) to target(): ",genbeta_lpdf(p[n]|restrict_N_high[count_high],sd_fix_high[count_high],lb,upb));
         
        prob_dens += genbeta_lpdf(p[n]|restrict_N_high[count_high],sd_fix_high[count_high],lb,upb);
        
        count_high = count_high + 1;
        
      } else if(r_in(n,low)) {
        
        if(debug_mode==1) print("Adding genbeta_lpdf(p[n]|sd_fix_low,restrict_N_low,lb,upb); to target(): ",genbeta_lpdf(p[n]|sd_fix_low[count_low],restrict_N_low[count_low],lb,upb));
        
        prob_dens += genbeta_lpdf(p[n]|sd_fix_low[count_low],restrict_N_low[count_low],lb,upb);
        
        count_low = count_low + 1;
        
      } else {
        
        if(debug_mode==1) print("Adding genbeta_lpdf(p[n]|scale,shape,lb,upb); to target(): ",genbeta_lpdf(p[n]|scale,shape,lb,upb));
        
        prob_dens += genbeta_lpdf(p[n]|scale,shape,lb,upb); 
        
      }
      
      
    }
    
    // return accumulated log probability
    
    return prob_dens;
    
  }
    
 