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
  int<lower = 1> Q; // numder of non-zero elements in state membership matrix
  int<lower = 1, upper = S> state[N]; // state indicator
  int<lower = 1, upper = T> year[N]; // year indicator
  matrix[A, L] X; // matrix of alliance-level variables
  matrix[N, M] W; // matrix of state-level variables
  matrix[N, A] Z; // matrix of state membership in alliances
  vector[N] y; // outcome 
}

transformed data{
  
  // This section decompose the sparse matrix Z into a more efficient representation. Note that is uses the transpose of Z. 
  vector[Q] w;
  int v[size(csr_extract_v(Z))]; 
  int u[size(csr_extract_u(Z))]; 
  
  w = csr_extract_w(Z);
  v = csr_extract_v(Z);
  u = csr_extract_u(Z); 
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
  real<lower = 5> nu; // degrees of freedom in t-distribution of outcome

}

transformed parameters {
  vector[S] alpha_state; // state intercepts
  vector[T] alpha_year; // year intercepts
  vector[A] lambda; // alliance intercepts
  vector[A] theta; // linear prediction of the mean hyperparameter for each alliances
  vector[N] alliance_fit;  // fitted predictions from the sparse matrix multiplication below
  

 alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

alpha_year = 0 + sigma_year * alpha_year_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

theta = X * beta; // linear predction of the mean of the alliance intercepts

for (i in 1:A)
    lambda[i] = theta[i] + sigma_all * lambda_std[i]; // non-centered parameterization where lamda ~ N(theta, sigma_all)
  
alliance_fit = csr_matrix_times_vector(N, A, w, v, u, lambda); // Multiply the sparse representation of the membership matrix by the vector of lambda parameters. 
    
}

model {
  
  vector[N] y_hat; // linear prediction of the mean

  
  
  // Linear prediction of the state-year spending mean Row i of the membership matrix Z will produce a scalar when postmultiplied by the vector of alliance characteristics lambda
  for (i in 1:N)
    y_hat[i] = alpha + alpha_state[state[i]] + alpha_year[year[i]] + alliance_fit[i] + W[i] * gamma;
  
  
  alpha ~ normal(0, 3);
  sigma ~ normal(0, 1);
  alpha_year_std ~ normal(0, 1);
  alpha_state_std ~ normal(0, 1); 
  lambda_std ~ normal(0, 1);
  sigma_state ~ cauchy(0, 1);
  sigma_year ~ cauchy(0, 1); 
  sigma_all ~ cauchy(0, 1); 
  beta ~  normal(0, 1);
  gamma ~ normal(0, 1); 
  nu ~ gamma(2, 0.1); // Prior for degrees of freedom in t-dist
  
  y ~ student_t(nu, y_hat, sigma);
}

generated quantities {
 vector[N] log_lik; // Log likelihood for loo and WAIC model comparisons
 vector[N] y_pred; //  posterior predictive distribution

for (i in 1:N) 
log_lik[i] = student_t_lpdf(y[i] | nu, alpha + alpha_state[state[i]] + alpha_year[year[i]] + alliance_fit[i] + W[i] * gamma , sigma);

for (i in 1:N)
y_pred[i] = student_t_rng(nu, alpha + alpha_state[state[i]] + alpha_year[year[i]] + alliance_fit[i] + W[i] * gamma, sigma);

}
