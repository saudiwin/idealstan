// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior

if(T<50) {
  
  //non-centered time series
  
  for(t in 1:T) {
  
  if(t==1) {
    
    lt[t] = L_tp1_var[t,s];
    
  } else {
    
    if(restrict_var==1) {
      
      if(s==1) {
        
        lt[t] = L_full[s] + L_AR1[s]*lt[t-1] + time_sd * L_tp1_var[t-1,s];
        
      } else {
        
        lt[t] = L_full[s] + L_AR1[s]*lt[t-1] + time_var_free[s-1] * L_tp1_var[t-1,s];
        
      }
      
    } else {
        
        lt[t] = L_full[s] + L_AR1[s]*lt[t-1] + time_var_free[s] * L_tp1_var[t-1,s];
      
    }
     
    
  }


  }
  
} else {
  
  // centered time series
  
  //centered time series
  
  if(restrict_var==1) {
    
    if(s==1) {
      
      log_prob += normal_lpdf(L_tp1_var[2:T,s]|L_AR1[s]*L_tp1_var[1:(T-1),s],time_sd]));
      
      
    } else {
      
      log_prob += normal_lpdf(L_tp1_var[2:T,s]|L_AR1[s]*L_tp1_var[1:(T-1),s],time_var_free[s-1]));
      
    }
    
  } else {
    
    log_prob += normal_lpdf(L_tp1_var[2:T,s]|L_AR1[s]*L_tp1_var[1:(T-1),s],time_var_free[s]));
    
  }
  
  
  
  lt = to_vector(L_tp1_var[1:T,s]);
  
}
