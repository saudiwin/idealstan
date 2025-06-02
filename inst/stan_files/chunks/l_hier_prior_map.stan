// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior

if(T<center_cutoff) {
  
  int start_time = 1;
  int end_time = T+1;
  
  if(ignore==1) {
    
    start_time = ignore_mat[s,1];
    end_time = ignore_mat[s,2];
    
  }
  
  // non-centered time-series
  
  for(t in 1:T) {
    
    if(t==1 || (t <= start_time) || (t >= end_time)) {
      
      lt[t]  = L_tp1_var[t,s];
      
    } else {
      
    if(restrict_var==1) {
      
      if(s==1) {
        
        lt[t] = lt[t-1] + time_sd * L_tp1_var[t,s];
        
      } else {
        
        lt[t] = lt[t-1] + time_var_free[s-1] * L_tp1_var[t,s];
        
      }
      
    } else {
        
        lt[t] = lt[t-1] + time_var_free[s] * L_tp1_var[t,s];
      
    }
      
      
        
      
    }
    
  }
  
} else {
  
  int start_time = 1;
  int end_time = T;
  
  //centered time series
  
  lt = to_vector(L_tp1_var[,s]);
  
  if(ignore==1) {
    
    start_time = ignore_mat[s,1];
    end_time = ignore_mat[s,2];
    
    if(end_time>T) {
      end_time = T;
    }
    
    if(start_time < 1) {
      
      start_time = 1;
      
    }
    
    if(start_time==T) {
      
      start_time = T  - 1;
      
    }
    
  }
  
  if(restrict_var==1) {
    
    if(s==1) {
      
      log_prob += normal_lpdf(lt[(start_time+1):end_time]|lt[start_time:(end_time-1)],time_sd);
      
      
    } else {
      
      log_prob += normal_lpdf(lt[(start_time+1):end_time]|lt[start_time:(end_time-1)],time_var_free[s-1]);
      
    }
    
  } else {
    
    log_prob += normal_lpdf(lt[(start_time+1):end_time]|lt[start_time:(end_time-1)],time_var_free[s]);
    
  }
  
  if(start_time>1)  {

    log_prob += normal_lpdf(lt[2:start_time]|0,legis_sd);

  }

  if(end_time<T) {

    log_prob += normal_lpdf(lt[(end_time+1):T]|0,legis_sd);

  }
  
  
}

