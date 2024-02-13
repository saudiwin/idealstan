// chunk to add splines to time series
// given degree of spline

if(ignore==1) {
  
  // whether to ignore beginning/end of time series
      
      for(s in 1:num_legis) {
        
        int start = 1;
        int end = T+1;
    
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
            
            
        L_tp1[,s] = to_array_1d(to_vector(a_raw[s]*B[end:start,]));
      
      }
      
    } else {
      
      for(s in 1:num_legis) {
            
        L_tp1[,s] = to_array_1d(to_vector(a_raw[s]*B));
      
      }


  }
  
}


