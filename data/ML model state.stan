// Joshua Alley
// Texas A&M University
// Multilevel Model: Non-alliance intercepts and state-level covariates
// For comparison with full model via loo and PPC


data {
  int<lower = 1> N; // Number of observations
  int<lower = 1> S; // number of states
  int<lower = 1> T; // number of years
  int<lower = 1> M; // number of state-level variables
  int<lower = 1, upper = S> state[N]; // state idenifier
  int<lower = 1, upper = T> year[N]; // year indicator
  matrix[N, M] W; // matrix of state-level variables
  vector[N] y; // outcome 
}

parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma; // variance of outcome
  vector[S] alpha_state_std; // better behaved distribution of state intercepts
  vector[T] alpha_year_std; // better behaved distribution of year intercepts
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts
  vector[M] gamma; // vector of state-level coefficients 
  real<lower = 4> nu; // prior for degrees of freedom in t-distribution
}

transformed parameters {
  vector[S] alpha_state; // state intercepts
  vector[T] alpha_year; // year intercepts
  

 alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

alpha_year = 0 + sigma_year * alpha_year_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

}

model {
  
  vector[N] y_hat; // linear prediction of the mean

  
  
  // Linear prediction of the state-year spending mean Row i of the membership matrix Z will produce a scalar when postmultiplied by the vector of alliance characteristics lambda
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]] + alpha_year[year[i]] + W[i] * gamma;
  
  
  alpha ~ normal(0, 3);
  sigma ~ cauchy(0, 1);
  alpha_year_std ~ normal(0, 1);
  alpha_state_std ~ normal(0, 1); 
  sigma_state ~ cauchy(0, 1); // boundary avoiding prior
  sigma_year ~ cauchy(0, 1); // boundary avoiding prior 
  gamma ~ normal(0, 1); 
  nu ~ gamma(2, 0.1); // Prior for degrees of freedom in t-dist

  
  y ~ student_t(nu, y_hat, sigma);
}

generated quantities {
 vector[N] log_lik; // Log likelihood for loo and WAIC model comparisons
 vector[N] y_pred; //  posterior predictive distribution

for (i in 1:N) 
log_lik[i] = student_t_lpdf(y[i] | nu, alpha + alpha_state[state[i]] + alpha_year[year[i]] + W[i] * gamma , sigma);

for (i in 1:N)
y_pred[i] = student_t_rng(nu, alpha + alpha_state[state[i]] + alpha_year[year[i]] + W[i] * gamma, sigma);

}
