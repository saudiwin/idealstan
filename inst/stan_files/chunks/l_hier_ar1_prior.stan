// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior
for(t in 1:T) {
  
  if(t==1) {
    L_tp1[t] = L_tp1_var[t];
  } else {
    L_tp1[t] = L_full + L_AR1 .* L_tp1[t - 1] + time_var_full .* L_tp1_var[t-1];
  }


}
