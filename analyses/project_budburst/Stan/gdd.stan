functions{
  vector gddcalc(vector x, real a) {
    vector[rows(x)] result;
    int n = 1;
    for (i in 1:rows(x)){
      if (x[i] <= a) {
	result[n] = 0;
      } else{
	result[n] = x[i] - a;
      };
      n = n + 1;
    };
    return result;
  }

  vector find_first_x(real x, vector y) {
    vector[1] n;
    for (i in 1:rows(y)) {
      if (y[i] >= x) {
	n[1] = i;
	break;
      };
    };
    return n;
  }

  real trunc_normal_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb, mu, sigma);
    real u = uniform_rng(p, 1);
    real z = inv_Phi(u);
    real y = mu + sigma * z;
    return y;
}

}

data {
  int<lower=1> n_X;
  int X[n_X];
  real<lower=0> Tmin;
  int<lower=1> n_Temperature;
  vector[n_Temperature] Temperature;
}

parameters {
  real<lower=0> mu_gdd;
  real<lower=0> sigma_gdd;
}

transformed parameters {
  vector[n_Temperature] gdd_acc;
  vector[n_X] gdd_threshold;
  
  for(i in 1:n_X){
    gdd_acc = cumulative_sum(gddcalc(Temperature, Tmin));
    gdd_threshold[i] = gdd_acc[X[i]];
  };

}
      
model {
  /* likelihood */
  gdd_threshold ~ normal(mu_gdd, sigma_gdd);
  
  /* priors */
  mu_gdd ~ normal(700, 50);
  sigma_gdd ~ cauchy(0, 10);
}

generated quantities {
  real<lower=0> gdd_gen;
  real<lower=0> X_gen;

  gdd_gen = trunc_normal_rng(mu_gdd, sigma_gdd, 0);
  X_gen = find_first_x(gdd_gen, gdd_acc)[1];  
}
