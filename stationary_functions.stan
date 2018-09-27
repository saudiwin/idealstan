/*
  Partial Autocorrelations to Autocorrelations

@param vector of coefficients of a partial autocorrelation function
@return vector of coefficients of an Autocorrelation function

*/
  vector pacf_to_acf(vector x) {
    matrix[num_elements(x), num_elements(x)] y;
    int n;
    n = num_elements(x);
    y = rep_matrix(0.0, n, n);
    for (k in 1:n) {
      for (i in 1:(k - 1)) {
        y[k, i] = y[k - 1, i] + x[k] * y[k - 1, k - i];
      }
      y[k, k] = x[k];
      print(y);
    }
    return -y[n] ';
  }
    
    /*
    Constrain vector of coefficients to the stationary and intertible region for AR or MA functions.
    
    @param vector x An unconstrained vector in (-Inf, Inf)
    @returns vector y A vector of coefficients for a stationary AR or inverible MA process.
    
    */
    vector constrain_stationary(vector x) {
    vector[num_elements(x)] r;
    int n;
    n = num_elements(x);
    // transform (-Inf, Inf) to (-1, 1)
    for (i in 1:n) {
    r[i] = x[i] / (sqrt(1.0 + pow(x[i], 2)));
    }
    // Transform PACF to ACF
    return pacf_to_acf(r);
    }
    
    /*
    Convert coefficients of an autocorrelation function to partial autocorrelations.
    
    @param vector x Coeffcients of an autocorrelation function.
    @returns vector y Coefficients of the corresponding partial autocorrelation function.
    
    */
    vector acf_to_pacf(vector x) {
    matrix[num_elements(x), num_elements(x)] y;
    vector[num_elements(x)] r;
    int n;
    n = num_elements(x);
    y = rep_matrix(0.0, n, n);
    y[n] = -x ';
    for (j in 0:(n - 1)) {
      int k;
      k = n - j;
      for (i in 1:(k - 1)) {
        y[k - 1, i] = (y[k, i] - y[k, k] * y[k, k - i]) / (1 - pow(y[k, k], 2));
      }
    }
    r = diagonal(y);
    return r;
    }

/*
  Transform from stationary and invertible space to (-Inf, Inf)

@param vector x Coeffcients of an autocorrelation function.
@returns vector y Coefficients of the corresponding partial autocorrelation function.

*/
  vector unconstrain_stationary(vector x) {
    matrix[num_elements(x), num_elements(x)] y;
    vector[num_elements(x)] r;
    vector[num_elements(x)] z;
    int n;
    n = num_elements(x);
    // Transform ACF to PACF
    r = acf_to_pacf(x);
    // Transform (-1, 1) to (-Inf, Inf)
    for (i in 1:n) {
      z[i] = r[i] / (sqrt(1.0 - pow(r[i], 2)));
    }
    return z;
  }