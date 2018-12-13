// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior
for(n in 1:gp_N) {
  
  for(t in 1:T) {
    calc_values[t] = legis_pred[t, n, ] * legis_x;
  }


L_temp[,n] ~ multi_normal_cholesky(calc_values, L_cov[n]); 
    
}
