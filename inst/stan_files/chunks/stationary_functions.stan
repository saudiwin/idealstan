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

    }
    return -y[n] ';
  }
    
    /*
    Convert vector of coefficients to the stationary and intertible region for AR or MA functions to 
    partial auto-correlation coefficients (r).
    
    @param vector x An unconstrained vector in (-Inf, Inf)
    @param vector r Partial auto-correlation coefficients of x
    
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
    return r;
    }
    
    /*

    */ 
    
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
  
  
  /* Function to return the log-Jacobian of the transform
    Reference Moynahan (1984) 

    @param vector r Partial auto-correlation coefficients
    @returns vector l A vector of the log-density of the transofrm
    */

  vector jacobian_stationary(vector r) {
    vector[num_elements(r)] l;
    vector[num_elements(r)] t1;
    vector[num_elements(r)] t2;
    int n;
    n = num_elements(r);
    // compute log jacobian given partial autocorrelation coefficients
    if(n == 1) {
      l[1] = log(1);
    } else {
      for (i in 1:n) {
        t1[i] = log(pow(1 - r[i]^2,(i-1)/2.0));
        // only add this second term if index is even
        t2[i] = (i % 2)==0 ? log(1 + r[i]) : 0;
      }
    }
    
    l = t1 + t2;
    
    return l;
  }
  

