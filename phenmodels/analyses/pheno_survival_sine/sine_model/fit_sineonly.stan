functions { 
  vector temps(vector times, real phase, real period, real amp, real baseline) {
    return amp * sin(2 * pi() * (times - phase) / period) + baseline;
  }
}

data {
  int<lower=1> N_temp_rec;
  vector[N_temp_rec] temp_time; // Days
  vector[N_temp_rec] temp_rec;  // Celcius
}

transformed data {
}

parameters {
  real<lower=0> temp_amp;  // Amplitude in Celcius
  real<lower=1> temp_per;  // Period in days; minimum is temporal scale of data
  real<lower=0,upper=temp_per> temp_phase;  // Phase in days; cannot exceed temp_per
  real temp_baseline;      // Baseline temperature in Celcius

  real<lower=0> temp_sigma; // Variability of temperature recording

}

transformed parameters {
  vector[N_temp_rec] latent_temp = temps(temp_time, temp_phase, temp_per, temp_amp, temp_baseline);
}

model {
  // Prior model
  temp_amp ~ normal(5, 10);
  temp_per ~ normal(20, 10);
  temp_phase ~ normal(6, 10);
  temp_baseline ~ normal(-10, 10);

  temp_sigma ~ normal(0, 5);
    
  // Temperature observational model
  temp_rec ~ normal(latent_temp, temp_sigma);
  
}

generated quantities {
  
}
