// Joshua Alley
// Texas A&M University
// Multiple Membership Multilevel Model 


data {
  int<lower = 1> N; // Number of observations
  int<lower = 1> S; // number of states
  int<lower = 1> T; // number of years
  int<lower = 1> A; // number of alliances
  int<lower = 1> L; // number of alliance-level variables
  int<lower = 1> M; // number of state-level variables
  int<lower = 1, upper = S> state[N]; // state idenifier
  int<lower = 1, upper = T> year[N]; // year indicator
  matrix[N, A] Z; // matrix of state membership in alliances
  matrix[A, L] X; // matrix of alliance-level variables
  matrix[N, M] W; // matrix of state-level variables
  vector[N] y; // outcome 
}

parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma; // variance of outcome
  vector[S] alpha_state_std; // better behaved distribution of state intercepts
  vector[T] alpha_year_std; // better behaved distribution of year intercepts
  vector[A] lambda_std; // better behaved distribution of the alliance intercepts
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts
  real<lower = 0> sigma_all; // variance hyperparameter of the alliances
  vector[L] beta; // vector of alliance-level coefficients
  vector[M] gamma; // vector of state-level coefficients 
}

transformed parameters {
  vector[S] alpha_state; // state intercepts
  vector[T] alpha_year; // year intercepts
  vector[A] lambda; // alliance intercepts
  vector[A] theta; // linear prediction of the mean hyperparameter for each alliances
  

 alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

alpha_year = 0 + sigma_year * alpha_year_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

theta = X * beta; // linear predction of the mean of the alliance intercepts

for (i in 1:A)
    lambda[i] = theta[i] + sigma_all * lambda_std[i]; // non-centered parameterization where lamda ~ N(theta, sigma_all)
}

model {
  
  vector[N] y_hat; // linear prediction of the mean

  
  
  // Linear prediction of the state-year spending mean Row i of the membership matrix Z will produce a scalar when postmultiplied by the vector of alliance characteristics lambda
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]] + alpha_year[year[i]] + Z[i] * lambda + W[i] * gamma;
  
  
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  alpha_year_std ~ normal(0, 1);
  alpha_state_std ~ normal(0, 1); 
  lambda_std ~ normal(0, 1);
  sigma_state ~ gamma(7, 2); // boundary avoiding prior
  sigma_year ~ gamma(2, 2); // boundary avoiding prior 
  sigma_all ~ gamma(2, 2); // boundary avoiding prior 
  beta ~  normal(0, 1);
  gamma ~ normal(0, 1); 
  
  y ~ normal(y_hat, sigma);
}
