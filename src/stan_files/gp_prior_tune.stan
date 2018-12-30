// code taken from Michael Betancourt's case study on Gaussian processes
// https://betanalpha.github.io/assets/case_studies/gp_part3/part3.html

functions {
  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;
    deltas[1] = inv_gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - 0.01;
    deltas[2] = 1 - inv_gamma_cdf(theta[2], exp(y[1]), exp(y[2])) - 0.01;
    return deltas;
  }
}

data {
  vector[2] theta; // two-length vector of min and max of distance
}

transformed data {
  vector[2] y_guess = [log(10), log(20)]';
  vector[2] y;
  real x_r[0];
  int x_i[0];
  
  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);
  
  //print("a = ", exp(y[1]));
  //print("b = ", exp(y[2]));
}

generated quantities {
  real a;
  real b;
  a = exp(y[1]);
  b = exp(y[2]);
}
