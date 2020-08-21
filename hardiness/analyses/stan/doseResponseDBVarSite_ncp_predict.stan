//A Dose Response Curve with no partial pooling of varience 
//Based on a logistic 4 parameter regression 
//started by Faith Jones 1/7/2020 (Canada day!)

//based on code from https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823
//needs x and y to be positive 
// thsi model is different from doseResponseSimple in that ehat rather than e is used. 

//model has an effect of variety and site on maximum hardiness d 

data {
  int<lower=1> N;                           // Number of observations
  vector<lower=1>[N] x;                      // Dose values (air temperature)
  vector [N] y;  
  
  //Level 2 	
	int < lower = 1 > n_vars; 				// number of random effect levels (varieties) 
	int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)// Response values (Winter cold hardiness). was an int in original code 

	int < lower = 1 > n_sites; 				// number of random effect levels (sites) 
	int < lower = 1, upper = n_sites > sites[N]; // id of random effect (site)// Response values (Winter cold hardiness). was an int in original code 

  // Data for predicting
    int<lower = 0> N_p; // number of new x values 
    vector<lower=1>[N_p] newX; // a new set of mean air temp data for use in generated quantities 

}

// Sampling space
parameters {
  real<lower=0>b;                             // slope
  real<lower=0> c;                     // lower asymptote
  real<lower=0> d;                     // upper asymptote
  real<lower=0> ehat;                           //  x where y is half way between c and d, but logged in equation
  real<lower=0> sigma_g;                        //error around the estimated hardiness 

  //level 2 - variety
  real <lower = 0> d_var_sigma; 		// a standard deviation of how much d (maximum hardiness) varies 
  real d_var_raw[n_vars]; // non centred parameterisation bit 
  
  real <lower = 0> b_var_sigma; // standard deviation of how much slope b changes by variety
  real b_var_raw[n_vars]; // non centred bit 
  
  //level 2 - site
  real <lower = 0> d_site_sigma; 		// a standard deviation of how much d (maximum hardiness) varies 
  real d_site_raw[n_sites]; // non centred parameterisation bit 
}

transformed parameters{
    real var_d[n_vars]; // a list of the effect of each variety on d (maximum hardiness)
    real var_b[n_vars]; // a list of the effect of each variety on d (maximum hardiness)
     
    real site_d[n_sites]; // a list of the effect of each site on d (maximum hardiness)
    
    for (n in 1:n_vars){
      var_d[n] = d_var_sigma * d_var_raw[n];
      var_b[n] = b_var_sigma * b_var_raw[n];
    }
    
    for (n in 1:n_sites){
      site_d[n] = d_site_sigma * d_site_raw[n];
    }
}

// Calculate posterior
model {
  vector[N] mu_y;

  // Priors on parameters
  c ~ normal(2, 0.5);                    // centred around 3, coudl very towards 0.  
  d ~ normal(25, 10);               // centred around maximum hardiness, 10 se 
  ehat ~ normal(log(30), 0.15);   // centred around mean temperature, se 20
  b ~ gamma(7, 1);                 // centred around 1, not really sure of appropriate prior tbh  
  sigma_g ~ normal(0, 5);           // I have little concept of how much variation there shoudl be, thsi is a stab in the dark

  //level 2 - variety 
  d_var_sigma ~gamma(2.5, 1.75);
  d_var_raw ~ normal(0, 1);
  
  b_var_sigma ~ normal(0, 3);
  b_var_raw ~ normal(0, 1);
  
  //level 2 - site
  d_site_sigma ~gamma(2.5, 1.75);
  d_site_raw ~ normal(0, 1);
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d + var_d[variety[i]] + site_d[sites[i]] - c) / (1 + exp((b +  var_b[variety[i]]) * (log(x[i]) - ehat))));
  }
  y ~ normal(mu_y, sigma_g); // I dont know why, but this was a poisson distribution in the original model 

}


generated quantities {
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] mu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data

  //Prediting data
  vector[N_p] mu_y_p;                      // Simulated mean data from likelyhood 
  vector[N_p] y_sim_p;                           //Simulated Data
  
  real<lower=0> e;
  e = exp(ehat);
  
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d+ var_d[variety[i]] + site_d[sites[i]] - c) / (1 + exp((b +  var_b[variety[i]]) * (log(x[i]) - ehat))));
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = normal_rng(mu_y[n], sigma_g); 
  
  //Predict new y data from new x data
   for (i in 1:N_p) {
    mu_y_p[i] = c + ((d+ var_d[variety[2]] - c) / (1 + exp((b +  var_b[variety[2]]) * (log(newX[i]) - ehat))));
  }
    for (n in 1:N_p) y_sim_p[n] = normal_rng(mu_y_p[n], sigma_g); 
}


