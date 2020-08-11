//A Dose Response Curve with no partial pooling of varience 
//Based on a logistic 4 parameter regression 
//started by Faith Jones 1/7/2020 (Canada day!)

//based on code from https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823
//needs x and y to be positive 

data {
  int<lower=1> N;                           // Number of observations
  vector<lower=1>[N] x;                      // Dose values (air temperature)
  vector [N] y;                                 // Response values (Winter cold hardiness). was an int in original code 
}

// Sampling space
parameters {
  real b;                             // slope
  real<lower=0> c;                     // lower asymptote
  real<lower=0> d;                     // upper asymptote
  real<lower=0> e;                           //  x where y is half way between c and d, but logged in equation
  real<lower=0> sigma_g;                        //error around the estimated hardiness 
}

// Transform parameters before calculating the posterior
transformed parameters {
  real<lower=0> ehat;
  ehat = log(e);
}

// Calculate posterior
model {
  vector[N] mu_y;

  // Priors on parameters
  c ~ normal(3, 1);                    // centred around 3, coudl very towards 0.  
  d ~ normal(30, 5);               // centred around maximum hardiness, 10 se 
  e ~ normal(30, 10);   // centred around mean temperature, se 20
  b ~ normal(10, 10);                 // centred around 1, not really sure of appropriate prior tbh  
  sigma_g ~ normal(0, 5);           // I have little concept of how much variation there shoudl be, thsi is a stab in the dark

  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d - c) / (1 + exp(b * (log(x[i]) - ehat))));
  }
  y ~ normal(mu_y, sigma_g); // I dont know why, but this was a poisson distribution in the original model 

}
