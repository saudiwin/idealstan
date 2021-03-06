// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior

if(T<center_cutoff) {
  
  // non-centered time-series
  
  for(t in 1:T) {
    if(t==1) {
      
      lt[t]  = L_tp1_var[t,s];
      
    } else {
      
    if(restrict_var==1) {
      
      if(s==1) {
        
        lt[t] = lt[t-1] + time_sd * L_tp1_var[t-1,s];
        
      } else {
        
        lt[t] = lt[t-1] + time_var_free[s-1] * L_tp1_var[t-1,s];
        
      }
      
    } else {
        
        lt[t] = lt[t-1] + time_var_free[s] * L_tp1_var[t-1,s];
      
    }
      
      
        
      
    }
    
  }
  
} else {
  //centered time series
  
  lt = to_vector(L_tp1_var[,s]);
  
  if(restrict_var==1) {
    
    if(s==1) {
      
      log_prob += normal_lpdf(lt[2:T]|lt[1:(T-1)],time_sd);
      
      
    } else {
      
      log_prob += normal_lpdf(lt[2:T]|lt[1:(T-1)],time_var_free[s-1]);
      
    }
    
  } else {
    
    log_prob += normal_lpdf(lt[2:T]|lt[1:(T-1)],time_var_free[s]);
    
  }
  
  
}

