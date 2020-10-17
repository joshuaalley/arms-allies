// Joshua Alley
// Multiple Membership Multilevel Model 
// effect of participation depends on relative capability


data {
  int<lower = 1> N; // number of observations 
  int<lower = 1> S; // number of states
  int<lower = 1> T; // number of years
  int<lower = 1> J; // number of state capability groups
  int<lower = 1> A_sm; // number of alliances: non-major
  int<lower = 1> A_lg; // number of alliances: non-major
  int<lower = 1> L; // number of alliance-level variables
  int<lower = 1> M; // number of state-level variables
  int<lower = 1, upper = S> state[N]; // state indicator
  int<lower = 1, upper = T> year[N]; // year indicator
  
  matrix[A_sm, L] X_sm; // matrix of alliance-level variables
  matrix[A_lg, L] X_lg; // matrix of alliance-level variables
  matrix[N, M] W; // matrix of state-level variables
  
  matrix[N, A_sm] Z_sm; // matrix of state membership in alliances: small
  matrix[N, A_lg] Z_lg; // matrix of state membership in alliances: large
  vector[N] y; // outcome 
}

transformed data{
  
  // This section decomposes the sparse alliance matrices Z into a more efficient representation.
  // non-major group
  vector[rows(csr_extract_w(Z_sm))] w_sm;
  int v_sm[size(csr_extract_v(Z_sm))]; 
  int u_sm[size(csr_extract_u(Z_sm))]; 
  // major group
  vector[rows(csr_extract_w(Z_lg))] w_lg;
  int v_lg[size(csr_extract_v(Z_lg))]; 
  int u_lg[size(csr_extract_u(Z_lg))]; 
  
// Implement the transformations   
  w_sm = csr_extract_w(Z_sm);
  v_sm = csr_extract_v(Z_sm);
  u_sm = csr_extract_u(Z_sm); 

  w_lg = csr_extract_w(Z_lg);
  v_lg = csr_extract_v(Z_lg);
  u_lg = csr_extract_u(Z_lg); 

}

parameters {
  real alpha; // overall intercept 
  real<lower = 0> sigma; // variance of outcome- 
  vector[S] alpha_state_std; // better behaved distribution of state intercepts
  vector[T] alpha_year_std; // better behaved distribution of year intercepts
  vector[A_sm] lambda_std_sm; // better behaved distribution of alliance intercepts: non-major 
  vector[A_lg] lambda_std_lg; // better behaved distribution of alliance intercepts: major
  real<lower = 0> sigma_state; // variance hyperparameter of the state intercepts
  real<lower = 0> sigma_year; // variance hyperparameter of the year intercepts
  vector[M] gamma; // vector of state-level variables (slopes do not vary by group)
 
  matrix[J, L] mu_beta; // mean of alliance-level coefficients
  vector<lower = 0>[L] tau_beta; // mean of theta par in multivariate distribution 
  matrix[L, J] z_beta; // for non-centered Cholesky factorization 
  cholesky_factor_corr[L] L_Omega_beta; // for non-centered Cholesky factorization 
  real<lower = 0> sigma_all_sm; // variance hyperparameter of the alliances: non-major
  real<lower = 0> sigma_all_lg; // variance hyperparameter of the alliances: major
  real<lower = 3> nu; // degrees of freedom in t-distribution of outcome- one per group

}

transformed parameters {
  vector[S] alpha_state; // state intercepts
  vector[T] alpha_year; // year intercepts
  matrix[J, L] beta; // effect of alliance char on treaty participation across groups 
  vector[A_sm] theta_sm; // mean of alliance parameters: non-major
  vector[A_sm] lambda_sm; // alliance parameters: non-major
  vector[A_lg] theta_lg; // mean of alliance parameters: major
  vector[A_lg] lambda_lg; // alliance parameters: major

 alpha_state = 0 + sigma_state * alpha_state_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)

 alpha_year = 0 + sigma_year * alpha_year_std; // non-centered parameterization, where alpha_state ~ N(0, sigma_state)


// varying slopes in alliance-level regression parameters beta 
  beta = mu_beta + (diag_pre_multiply(tau_beta, L_Omega_beta) * z_beta)';
  
  theta_sm = X_sm * beta[1, ]';
  theta_lg = X_lg * beta[2, ]';

// lambdas w/ non-centered parameterization
 for (i in 1:A_sm)
   lambda_sm[i] = theta_sm[i] + sigma_all_sm * lambda_std_sm[i];
 for (i in 1:A_lg)
   lambda_lg[i] = theta_lg[i] + sigma_all_lg * lambda_std_lg[i];


}

model {
  
  alpha ~ std_normal(); 
  sigma ~ std_normal();
  alpha_year_std ~ std_normal();
  sigma_year ~ std_normal(); 
  alpha_state_std ~ std_normal(); 
  sigma_state ~ std_normal();
  sigma_all_sm ~ normal(0, .5); 
  sigma_all_lg ~ normal(0, .5); 
  nu ~ gamma(2, 0.1); // Prior for degrees of freedom in t-dist
  
  
  gamma ~ normal(0, .5); 
  to_vector(z_beta) ~ normal(0, .5);
  L_Omega_beta ~ lkj_corr_cholesky(2);
  tau_beta ~ normal(0, .25); 
  to_vector(mu_beta) ~ normal(0, .5);
  lambda_std_lg ~ normal(0, .5);
  lambda_std_sm ~ normal(0, .5);  
  
  
// outcome 
asinh(y) ~ student_t(nu, alpha + alpha_state[state] + alpha_year[year] + 
    W * gamma + // state-level factors 
  csr_matrix_times_vector(N, A_sm, w_sm, v_sm, u_sm, lambda_sm) + // small membership matrix
  csr_matrix_times_vector(N, A_lg, w_lg, v_lg, u_lg, lambda_lg), // large membership matrix
  sigma);
target += -asinh(y);           
 
}
