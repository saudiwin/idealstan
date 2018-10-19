/*
  Jacobian correction for correcting the mean
  
  Only needs to be used once to save the value as transformed data

@param N size of parameter vector being constrained
@param N_real size of parameter vector being constrained
@return log absolute determinant of the Jacobian of the transformation

*/
  real jacob_mean(int N, real N_real) {
    vector[N] col_vec;
    real num_div;
    real density;
    
    num_div = 1/N_real;

    col_vec = rep_vector(num_div,N);

    density = sum(log(col_vec));

    return density;

  }
