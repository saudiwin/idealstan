// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior

if(T<center_cutoff) {
  
  for(t in 1:T) {
    
    if(ignore==1) {
      
      for(s in 1:num_legis) {
        
        int start = 1;
        int end = T+1;
  
      if(ignore==1) {
    
        start = ignore_mat[s,1];
        end = ignore_mat[s,2];
        
            if(end>T) {
              end = T;
            }
            
            if(start < 1) {
              
              start = 1;
              
            }
            
            if(start==T) {
              
              start = T  - 1;
              
            }
    
        }
        
        if(t==1 || (t <= start) || (t >= end)) {
          L_tp1[t,s] = L_tp1_var[t,s];
        } else {
          L_tp1[t,s] = L_tp1[t-1,s] + time_var_full[s] * L_tp1_var[t-1,s];
        }
      
      }
      
    } else {
      
      if(t==1) {
      
        L_tp1[t]  = L_tp1_var[t];
      
      } else {
      
        L_tp1[t] = L_tp1[t-1] + time_var_full .* L_tp1_var[t-1];
      
      }
      
    }
    

    
  }
  
  
} else {
  
  L_tp1 = L_tp1_var;
  
}

