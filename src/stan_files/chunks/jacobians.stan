/*
  Jacobian correction for correcting the mean
  
  Only needs to be used once to save the value as transformed data

@param N size of parameter vector being constrained
@param N_real size of parameter vector being constrained
@return log absolute determinant of the Jacobian of the transformation

*/
  real jacob_mean(int N, real N_real) {
    matrix[N,N] I;
    vector[N] col_vec;
    row_vector[N] row_vec;
    real num_div;
    real density;
    
    num_div = 1/N_real;

    col_vec = rep_vector(num_div,N);
    row_vec = rep_vector(num_div,N)';
    
    
    I = diag_matrix(rep_vector(1,N));

    //using formula for determinant of a block matrix
    //https://en.wikipedia.org/wiki/Determinant#Block_matrices

    density = log(fabs((num_div - row_vec * I * col_vec)));

    //calculation can fail for small N
    //if so, return 1 (less than calculable difference)
    if(is_inf(density)==1) {
      return(0);
    } else {
      return density;
    }
    

  }