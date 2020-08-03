// A dose response model, reparameterized to be easier to fit. 
// https://www.biorxiv.org/content/10.1101/2020.03.16.993162v1
// https://discourse.mc-stan.org/t/difficulties-with-logistic-population-growth-model/8286/3

// x needs to be standardized, y goes in positive.

data {
  int<lower=1> N;                           // Number of observations
  vector[N] x;                      // Dose values (air temperature)
}

generated quantities {
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] mu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data
  real<lower = 0> beta;                     // slope
  real eta;                     // inflection point (intercept)
  real<lower = 0> y0;                     // y when x is 0
  real<lower=0> sigma_g; 

  // Priors on parameters
  // NOTE THE FABS IS ESSENTIAL FOR LOWER BOUND = 0, otherwise there will be lost of negative numbers 
  beta = fabs(gamma_rng(12, 1));                    // centred around no hardiness, could be as high as -20
  eta = fabs(normal_rng(0, 0.20));               // centred around 0 (x mid point), but can range around all x values
  sigma_g = fabs(normal_rng(0, 5));           // I have little concept of how much variation there should be, thsi is a stab in the dark
  y0 = fabs(normal_rng(15, 5));             // constraining y0 to fall inside normal hardiness values 
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = (y0*(1 + exp(beta*eta))/ (1 + exp(beta*(eta-x[i]))));
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = normal_rng(mu_y[n], sigma_g); 
}
