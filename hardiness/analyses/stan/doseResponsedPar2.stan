// A dose response model, reparameterized to be easier to fit. 
// https://www.biorxiv.org/content/10.1101/2020.03.16.993162v1
// https://discourse.mc-stan.org/t/difficulties-with-logistic-population-growth-model/8286/3

// x needs to be standardized, y gos in positive. 


data {
  int<lower=1> N;                           // Number of observations
  vector[N] x;                      // Dose values (air temperature)
  vector[N] y;                     //winter hardiness, *-1 to eb positive
}

// Sampling space
parameters {
  real beta;                             // slope
  real eta;                     // infelction point of x. 
  real<lower=0> y0;                     // y intercept (where x is 0)
  real<lower=0> sigma_g;                        //error around the estimated hardiness 

}

// Calculate posterior
model {
  vector[N] mu_y;

  // Priors on parameterssimLTEPos <- simLTE * -1
  beta ~ gamma(11, 1.25);                    // centred around no hardiness, could be as high as -20
  eta ~ normal(0, 0.20);               // centred around 0 (x mid point), but can range around all x values, but strongly constrained to sit around the mean 
  sigma_g ~ normal(0, 5);           // 
  y0 ~ normal(15, 5);             // constraining y0 to fall inside normal hardiness values 
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = (y0*(1 + exp(beta*eta))/ (1 + exp(beta*(eta-x[i]))));
  }
  y ~ normal(mu_y, sigma_g); // I dont know why, but this was a poisson distribution in the original model 

}
generated quantities {
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] mu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data

  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = (y0*(1 + exp(beta*eta))/ (1 + exp(beta*(eta-x[i]))));
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = normal_rng(mu_y[n], sigma_g); 
}
