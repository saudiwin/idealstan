// chunk to add splines to time series
// given degree of spline

if(ignore==1) {
  
  // whether to ignore beginning/end_time of time series
        
        int start_time = 1;
        int end_time = T+1;
    
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
            
            
       //lt = L_full[s] * to_vector(time_ind[end_time:start_time]) + to_vector(a*B[end_time:start_time,]);

      lt = to_vector(a_raw[s]*B[start_time:end_time,]);
      
    } else {
      
        lt = to_vector(a_raw[s]*B);

  }


