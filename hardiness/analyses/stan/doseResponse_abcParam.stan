//
//This dose response model was writen by martin 
//https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling-on-maximum-value/17086/8
data {
  int<lower=1> N;                           // Number of observations
  vector[N] x;                      // Dose values (air temperature)
  vector[N] y;                     //winter hardiness, *-1 to eb positive
}

// Sampling space
parameters {
  real<lower=0> a;
  real<lower=0> c;
  // Enforce the constraints (yes, this is completely valid code)
  real<lower=sqrt(a * c), upper=max({a,c})> b;
  real<lower=0> sigma_g;  

}

transformed parameters {
  real alpha = log(a/(a-b) + b / (c-b));
  real beta = log((a-b)/(b-c)) + log(c) - log(a);
  real<lower=0> k = b * (a * (b - 2 *c) + b * c) / (b^2 - a * c);
}

// Calculate posterior
model {
  vector[N] logmu_y;

  // priors
  sigma_g ~ normal(0, 0.5);           
  a ~ lognormal(log(15), log(5));            
  b ~ lognormal(log(15), log(5));           
  c ~ lognormal(log(15), log(5)); 

  //likelihood function 
  for (i in 1:N) {
    //Staying on log scale, the line below is equialent to 
    // mu_y[i] = k/(1+exp(-beta*x[i] - alpha));
    logmu_y[i] = log(k) - log1p_exp(-beta*x[i] - alpha);
  }
  y ~ lognormal(logmu_y, sigma_g); 
}

generated quantities {
  
  // Reversing Stan's constraints for diagnostics
  real b_raw;
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] logmu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data

  //likelihood function 
  for (i in 1:N) {
    logmu_y[i] = log(k) - log1p_exp(-beta*x[i] - alpha);
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = lognormal_rng(logmu_y[n], sigma_g); 
  
  {
    // Recompute the bounds and reverse the constraint transform
    real b_low = max({min({a,c}), sqrt(a * c)});
    real b_up = max({a,c});
    b_raw = logit((b - b_low) / b_up);
  }
}
