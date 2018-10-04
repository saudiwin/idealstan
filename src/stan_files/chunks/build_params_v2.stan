    
    if(num_legis==2) {
      if(T>1) {
        if(use_ar==1) {
          L_full = append_row(L_free,restrict_high);
        } else {
          L_full = append_row(restrict_low,restrict_high);
        }
      } else {
        L_full = append_row(restrict_low,restrict_high);
        
      }
      
    } else {
      if(T>1) {
        if(use_ar==1) {
          L_full = append_row(L_free,append_row(restrict_low,restrict_high));
        } else {
          L_full = append_row(L_free,append_row(restrict_low,restrict_high));
        }
        
        
      } else {
        L_full = append_row(L_free,append_row(restrict_low,restrict_high));
      }
      
    }
