// Joshua Alley
// Texas A&M University
// Multiple Membership Multilevel Model 
// encoded as a mixture with known proportions


data {
  int<lower = 1> N_min; // number of non-major power observations 
  int<lower = 1> N_maj; // number of major power observations 
  int<lower = 1> S; // number of states
  int<lower = 1> T; // number of years
  int<lower = 1> J; // number of state capability groups
  int<lower = 1> A_min; // number of alliances: non-major
  int<lower = 1> A_maj; // number of alliances: major 
//  int<lower = 1> A_sup; // number of alliances: super
  int<lower = 1> L; // number of alliance-level variables
  int<lower = 1> M; // number of state-level variables
  int<lower = 1, upper = S> state_min[N_min]; // state indicator: non-major
  int<lower = 1, upper = S> state_maj[N_maj]; // state indicator: major
  int<lower = 1, upper = T> year_min[N_min]; // year indicator: non-major 
  int<lower = 1, upper = T> year_maj[N_maj]; // year indicator: non-major 
//  int<lower = 1, upper = J> cap[N]; // major-power indicator
  matrix[A_min, L] X_min; // matrix of alliance-level variables: non-major 
  matrix[A_maj, L] X_maj; // matrix of alliance-level variables: major
  matrix[N_min, M] W_min; // matrix of state-level variables: non-major
  matrix[N_maj, M] W_maj; // matrix of state-level variables: major
  matrix[N_min, A_min] Z_min; // matrix of state membership in alliances: non-major
  matrix[N_maj, A_maj] Z_maj; // matrix of state membership in alliances: major
//  matrix[N, A_sup] Z_sup; // matrix of state membership in alliances: super
  vector[N_min] y_min; // outcome for non-major powers
  vector[N_maj] y_maj; // outcome for major powers
}

transformed data{
  
  // This section decomposes the sparse alliance matrices Z into a more efficient representation.
  // non-major group
  vector[rows(csr_extract_w(Z_min))] w_min;
  int v_min[size(csr_extract_v(Z_min))]; 
  int u_min[size(csr_extract_u(Z_min))]; 
  // major group
  vector[rows(csr_extract_w(Z_maj))] w_maj;
  int v_maj[size(csr_extract_v(Z_maj))]; 
  int u_maj[size(csr_extract_u(Z_maj))]; 
  
// Implement the transformations   
  w_min = csr_extract_w(Z_min);
  v_min = csr_extract_v(Z_min);
  u_min = csr_extract_u(Z_min); 

  w_maj = csr_extract_w(Z_maj);
  v_maj = csr_extract_v(Z_maj);
  u_maj = csr_extract_u(Z_maj); 
}

parameters {
  vector<lower = 0>[J] sigma; // variance of outcome- one per group
  vector[J] alpha_cap; // capability random effects
  vector[S] alpha_state_std; // better behaved distribution of state intercepts
  vector[T] alpha_year_std; // better behaved distribution of year intercepts
  vector[A_min] lambda_std_min; // better behaved distribution of alliance intercepts: non-major 
  vector[A_maj] lambda_std_maj; // better behaved distribution of alliance intercepts: major
 // vector[A_sup] lamnda_std_sup; // better behaved distribution of alliance intercepts: super
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts
  real<lower = 0> sigma_cap; // variance hyperparameter of state capability groups
  vector[M] gamma; // vector of state-level variables (slopes do not vary by group)
  matrix[J, L] mu_beta; // mean of alliance-level coefficients
  vector<lower = 0>[L] tau_beta; // mean of theta par in multivariate distribution 
  matrix[L, J] z_beta; // for non-centered Cholesky factorization 
  cholesky_factor_corr[L] L_Omega_beta; // for non-centered Cholesky factorization 
  real<lower = 0> sigma_all_min; // variance hyperparameter of the alliances: non-major
  real<lower = 0> sigma_all_maj; // variance hyperparameter of the alliances: major
//  real<lower = 0> sigma_all_sup; // variance hyperparameter of the alliances: super
  vector<lower = 3>[J] nu; // degrees of freedom in t-distribution of outcome- one per group

}

transformed parameters {
  vector[S] alpha_state; // state intercepts
  vector[T] alpha_year; // year intercepts
  matrix[J, L] beta; // effect of alliance char on treaty participation across groups 
  vector[A_min] theta_min; // mean of alliance parameters: non-major
  vector[A_min] lambda_min; // alliance parameters: non-major
  vector[A_maj] theta_maj; // mean of alliance parameters: major
  vector[A_maj] lambda_maj; // alliance parameters: major
//  vector[A_sup] theta_sup; // mean of alliance parameters: super
//  vector[A_sup] lambda_sup; // alliance parameters: super

 alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

 alpha_year = 0 + sigma_year * alpha_year_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)


// varying slopes in alliance-level regression parameters beta 
  beta = mu_beta + (diag_pre_multiply(tau_beta, L_Omega_beta) * z_beta)';
  
  theta_min = X_min * beta[1, ]';
  theta_maj = X_maj * beta[2, ]';
//  theta_sup = X_sup * beta[3, ]';

// lambdas w/ non-centered parameterization
 for (i in 1:A_min)
   lambda_min[i] = theta_min[i] + sigma_all_min * lambda_std_min[i];
 for (i in 1:A_maj)
   lambda_maj[i] = theta_maj[i] + sigma_all_maj * lambda_std_maj[i];
// for (i in 1:A_sup)
//  lambda_sup[i] = theta_sup[i] + sigma_all_sup * lambda_std_sup[i];

    
}

model {

  sigma ~ normal(0, 1);
  alpha_cap ~ normal(0, sigma_cap);
  alpha_year_std ~ normal(0, 1);
  sigma_year ~ normal(0, 1); 
  alpha_state_std ~ normal(0, 1); 
  sigma_state ~ normal(0, 1);
  sigma_all_min ~ normal(0, .5); 
  sigma_all_maj ~ normal(0, .5); 
//  sigma_all_sup ~ normal(0, 1); 
  sigma_cap ~ normal(0, 1);
  nu ~ gamma(2, 0.1); // Prior for degrees of freedom in t-dist
  gamma ~ normal(0, .5); 
  to_vector(z_beta) ~ normal(0, .5);
  L_Omega_beta ~ lkj_corr_cholesky(2);
  tau_beta ~ normal(0, .25); 
  to_vector(mu_beta) ~ normal(0, .5);
  lambda_std_maj ~ normal(0, .5);
  lambda_std_min ~ normal(0, .5);  
  
  
// Separate models for each group- information shared through common parameters
asinh(y_min) ~ student_t(nu[1], alpha_cap[1] + alpha_state[state_min] + alpha_year[year_min] + W_min * gamma + 
           csr_matrix_times_vector(N_min, A_min, w_min, v_min, u_min, lambda_min), sigma[1]);
target += -asinh(y_min);           
           
asinh(y_maj) ~ student_t(nu[2], alpha_cap[2] + alpha_state[state_maj] + alpha_year[year_maj] + W_maj * gamma + 
           csr_matrix_times_vector(N_maj, A_maj, w_maj, v_maj, u_maj, lambda_maj), sigma[2]);
 target += -asinh(y_maj);
 
}

generated quantities {
// vector[N] log_lik; // Log likelihood for loo and WAIC model comparisons
// vector[N] y_pred; //  posterior predictive distribution

// for(i in 1:N)
// log_lik[i] = student_t_lpdf(y[i] | nu, y_hat[i], sigma);


// y_pred[i] = student_t_rng(nu, y_hat[i], sigma);

}
