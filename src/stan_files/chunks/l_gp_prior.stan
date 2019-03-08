// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   cov_exp_quad(time_ind, m_sd[1], time_var[n])
      + diag_matrix(rep_vector(square(gp_sd[1]),T));
  L_cov[n] = cholesky_decompose(cov[n]);
  
  to_vector(L_tp2[,n]) ~ multi_normal_cholesky(rep_vector(0,T), L_cov[n]); 
  
    
}
