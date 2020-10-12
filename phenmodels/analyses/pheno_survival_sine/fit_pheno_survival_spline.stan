functions {
  // Wang-Engels Rescaled Temperature
  real x_WE(real T, real T_min, real T_max, real T_opt) {
    real alpha = log(2) / log( (T_max - T_min) / (T_opt - T_min) );
    return 0.5 * ( (T - T_min) / (T_opt - T_min) )^alpha;
  }
  
  // Logarithm of Wang-Engels Rescaled Temperature
  real log_x_WE(real T, real T_min, real T_max, real T_opt) {
    real alpha = log(2) / log( (T_max - T_min) / (T_opt - T_min) );
    return -0.6931471 + alpha * log( (T - T_min) / (T_opt - T_min) );
  }
  
  // Log hazard function in inverse days
  real log_hazard(real T, real gamma, real T_min, real T_max, real T_opt) {
    if (T < T_min) {
      return negative_infinity();
    } else if (T > T_max) {
      return negative_infinity();
    } else {
      real log_x = log_x_WE(T, T_min, T_max, T_opt);
      return log(gamma) + 1.3862943 + log_x + log_sum_exp(0, log_x);
    }
    return 0;
  }
  
  // Hazard function in inverse days
  real hazard(real T, real gamma, real T_min, real T_max, real T_opt) {
    if (T < T_min)
      return 0;
    else if (T > T_max) 
      return 0;
    else {
      real x = x_WE(T, T_min, T_max, T_opt);
      return gamma * 4 * x * (1 - x);
    }
  }
  
  // Survival function
  real log_survival(real[] temps, real gamma, real T_min, real T_max, real T_opt) {
    int N_days = size(temps);
    real sum_hazard = 0;
    for (n in 1:N_days) {
      sum_hazard += hazard(temps[n], gamma, T_min, T_max, T_opt);
    }
    return -sum_hazard;
  }
}

data {
  int<lower=1> N;
  int obs_tran_time[N]; // Days since dormancy (around 230)
  
  int<lower=1> N_temp_rec;
  int temp_time[N_temp_rec]; // Days
  real temp_rec[N_temp_rec];  // Celcius
}

parameters {
  real T_min;
  real T_opt;
  real T_max;
  real<lower=0> gamma;
}

model {
  // Prior model
  T_min ~ normal(4, 0.75);
  T_max ~ normal(40, 1.5);
  T_opt ~ normal(26, 2);
  gamma ~ gamma(4.63, 22.07);
  
  // Phenology observational model
  for (n in 1:N) {
    target +=   log_survival(temp_rec[1:(obs_tran_time[n] - 1)], gamma, T_min, T_max, T_opt) 
              + log_hazard(temp_rec[obs_tran_time[n]], gamma, T_min, T_max, T_opt);
  }
}

generated quantities {
  real integrated_we_pred[N_temp_rec];
  real survival_pred[N_temp_rec];
  {
    for (n in 1:N_temp_rec) {
      real int_hazard = log_survival(temp_rec[1:(n - 1)], gamma, T_min, T_max, T_opt);
      integrated_we_pred[n] = -int_hazard / gamma;
      survival_pred[n] = exp(int_hazard);
    }
  }
}
