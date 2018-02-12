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
  vector[T] alpha_year; // year intercepts 
  vector[S] alpha_state; // state intercepts
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts
  real<lower = 0> sigma_all; // variance hyperparameter of the alliances
  vector[L] beta; // vector of alliance-level coefficients
  vector[A] lambda; // alliance-specific values with mean theta
  vector[M] gamma; // vector of state-level coefficients 
}

transformed parameters {
  
}

model {
  
  vector[N] y_hat; // linear prediction of the mean
  vector[A] theta; // linear prediction of the mean hyperparameter for each alliances
  
      theta = X * beta;
  

    for (i in 1:A)
    lambda[i] ~ normal(theta[i], sigma_all);
  
  // Linear prediction of the state-year spending mean Row i of the membership matrix Z will produce a scalar when postmultiplied by the vector of alliance characteristics lambda
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]] + alpha_year[year[i]] + Z[i] * lambda + W[i] * gamma;
  
  
  alpha ~ normal(7, 4);
  sigma ~ cauchy(0, 2);
  alpha_year ~ normal(0, sigma_year);
  alpha_state ~ normal(0, sigma_state); 
  sigma_state ~ cauchy(0, 6);
  sigma_year ~ cauchy(0, 3);
  sigma_all ~ cauchy(0, 3);
  beta ~  normal(0, 2);
  gamma ~ normal(0, 2); 
  
  
  y ~ normal(y_hat, sigma);
}
