// Joshua Alley
// Texas A&M University
// Multiple Membership Multilevel Model 


data {
  int<lower = 1> N; // Number of observations
  int<lower = 1> state[N]; // state idenifier
  int<lower = 1> S; // number of states
  vector[N] y; // outcome 
}

parameters {
  real alpha; // overall intercept
  real sigma; // variance of outcome
  vector[S] alpha_state; // state intercepts 
  real sigma_alpha; // varuance hyperparameter for the varying intercepts
}

transformed parameters {
  
}

model {
  vector[N] y_hat; // linear prediction of the mean
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]];
  
  
  alpha ~ normal(8, 3);
  sigma ~ cauchy(0, 2);
  alpha_state ~ normal(0, sigma_alpha);
  sigma_alpha ~ cauchy(0, 1);
  
  
  y ~ normal(y_hat, sigma);
}
