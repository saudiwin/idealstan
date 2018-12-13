// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   cov_exp_quad(time_ind, m_sd[1], time_var[n])
      + diag_matrix(rep_vector(square(extra_sd),T));
  L_cov[n] = cholesky_decompose(cov[n]);
  
  for(t in 1:T) {
    calc_values[t] = legis_pred[t, n, ] * legis_x;
  }

  to_vector(L_tp2[,n]) ~ multi_normal_cholesky(calc_values, L_cov[n]); 
    
}
