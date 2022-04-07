    
    if(num_legis==2) {
      if(T>1) {
        if(time_proc==2||time_proc==3) {
          L_full = append_row(L_free,restrict_high);
        }
      } else {
        L_full = append_row(restrict_low,restrict_high);
        
      }
      
    } else {
      if(T>1) {
        if(time_proc==2 || time_proc==3) {
          L_full = append_row(L_free,append_row(restrict_low,restrict_high));
        }
        
        
      } else {
        L_full = append_row(L_free,append_row(restrict_low,restrict_high));
      }
      
    }
