// Joshua Alley
// Texas A&M University
// Multiple Membership Multilevel Model 


data {
  int<lower = 1> N; // Number of observations
  int<lower = 1> state[N]; // state idenifier
  int<lower = 1> year[N];
  int<lower = 1> S; // number of states
  int<lower = 1> T; // number of years
  vector[N] y; // outcome 
}

parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma; // variance of outcome
  vector[T] alpha_year; // year intercepts 
//  real alpha_state_mean; // mean hyperparameter for the state intercepts
  vector[S] alpha_state_std; // state intercepts non-centering parameter
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts 
}

transformed parameters {
  vector[S] alpha_state; // state intercepts 
    
  alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

}

model {
  
  vector[N] y_hat; // linear prediction of the mean
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]] + alpha_year[year[i]] ;
  
  
  alpha ~ normal(7, 4);
  sigma ~ cauchy(0, 2);
//  alpha_state_mean ~ normal(0, 1);
  alpha_state_std ~ normal(0, 1); // better-behaved distribution for non-centered parameterization 
  alpha_year ~ normal(0, sigma_year);
  sigma_state ~ cauchy(0, 6);
  sigma_year ~ cauchy(0, 3);
  
  
  y ~ normal(y_hat, sigma);
}
