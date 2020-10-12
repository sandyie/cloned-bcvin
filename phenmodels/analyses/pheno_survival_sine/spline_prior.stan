functions {
  // Recursively calculate spline basis
  vector build_b_spline(real[] t, real[] ext_knots, int idx, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int idx, int order) {
    // INPUTS:
    //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    int N = size(t);
    vector[N] b_spline;
    vector[N] w1 = rep_vector(0, N);
    vector[N] w2 = rep_vector(0, N);
    
    if (order == 1)
      // B-splines of order 1 are piecewise constant
      for (n in 1:N) 
        b_spline[n] = (ext_knots[idx] <= t[n]) && (t[n] < ext_knots[idx + 1]);
    else {
      if (ext_knots[idx] != ext_knots[idx + order - 1])
        w1 = (to_vector(t) - rep_vector(ext_knots[idx], N)) /
             (ext_knots[idx + order - 1] - ext_knots[idx]);
      if (ext_knots[idx + 1] != ext_knots[idx + order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[idx + 1], N)) /
                 (ext_knots[idx + order] - ext_knots[idx + 1]);
                 
      // Calculate the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, idx, order - 1) +
                 w2 .* build_b_spline(t, ext_knots, idx + 1, order - 1);
    }
    return b_spline;
  }
}

data {
  int N_obs;             // Number of evaluation points
  real x[N_obs];         // Evaluation points
  
  int N_knots;           // Number of knots
  vector[N_knots] knots; // Knot locations
}

transformed data {
  int spline_degree = 3; // The degree of spline, equal to order - 1
  
  // Total number of B-splines
  int N_basis = N_knots + spline_degree - 1;

  // Set of extended knots
  vector[N_knots + 2 * spline_degree] ext_knots
    = append_row(append_row(rep_vector(knots[1], spline_degree), knots),
                 rep_vector(knots[N_knots], spline_degree));
    
  // Matrix of B-splines
  matrix[N_basis, N_obs] B; 
  for (n in 1:N_basis)
    B[n, :] = to_row_vector(build_b_spline(x, to_array_1d(ext_knots), n, spline_degree + 1));
  B[N_knots + spline_degree - 1, N_obs] = 1;
}

parameters {
  row_vector[N_basis] a_raw;
  real<lower=0> tau;
}

transformed parameters {
  vector[N_obs] f;
  {
    //row_vector[N_basis] a;
    //a[1] = a_raw[1];
    //for (n in 2:N_basis)
    //  a[n] = a[n - 1] + a_raw[n] * tau;
    row_vector[N_basis] a = a_raw * tau;
    f = to_vector(a * B);
  }
}

model {
  a_raw ~ normal(0, 0.5);
  tau ~ normal(0, 3);
}
