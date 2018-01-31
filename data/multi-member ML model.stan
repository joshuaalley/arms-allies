// Joshua Alley
// Texas A&M University
// Multiple Membership Multilevel Model 


data {
  int<lower = 1> N; // Number of observations
  vector[N] y; // outcome 
}

parameters {
  real alpha; // overall intercept
  real sigma; // variance of outcome
}

transformed parameters {
  
}

model {
  vector[N] y_hat; // linear prediction of the mean
  
  for (i in 1:N)
  y_hat[i] = alpha;
  
  y ~ normal(y_hat, sigma);
}