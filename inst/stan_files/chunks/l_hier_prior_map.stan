// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior
for(t in 1:T) {
    if(t==1) {
      
      lt[t]  = L_full[s];
      
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
