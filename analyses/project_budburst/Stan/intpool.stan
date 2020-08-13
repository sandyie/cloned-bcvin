
data {
	int<lower=1> N;
	int<lower=1> n_variety;
	int<lower=1, upper=n_variety> variety[N];
	vector[N] y; 		// response
}
	
parameters {
  real<lower=0> mu_a_variety;   
  real<lower=0> sigma_a_variety; 
  real<lower=0> sigma_y; 
  
  real a_variety[n_variety]; // intercept for varieties

}

transformed parameters {
   real mu_y[N];
       	for(i in 1:N){
	  mu_y[i] = a_variety[variety[i]]; // indexed by variety
		}
}
	
model {   	
  y ~ normal(mu_y, sigma_y);
  
  // priors
  a_variety ~ normal(mu_a_variety, sigma_a_variety); 
  
  mu_a_variety ~ normal(400, 50);
  sigma_a_variety ~ normal(0, 10);
  sigma_y ~ normal(0, 10);


}

