# Joshua Alley
# Texas A&M University
# Comparison of different models of arms spending and alliance membership

# Use this file after running
# 1. dataset contruction and summary.R 
# 2. analysis.R 

# load packages
library(loo)

# set working directory
setwd(here::here())
getwd()

# Initial model is ml.model from the analysis script
# Note: Do not overwrite loo objects- this will crash the R session. 

# extract the log-likelihood from the full model
log.lik.full <- extract_log_lik(ml.model)
loo.full <- loo(log.lik.full)
print(loo.full)
waic.full <- waic(log.lik.full)


# Fit a model with intercept parameters only- no alliance intercepts
stan.data.intercepts <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                                      state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                                      year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)))

# compile the model code
model.int <- stan_model(file = "data/ML model intercepts.stan")

# Fit the model
system.time(
  ml.model.int <- stan(file = "data/ML model intercepts.stan", data = stan.data.intercepts, 
                   iter = 2000, warmup = 1000, chains = 4, cores = 4
  )
)

 # Diagnose intercept only model
launch_shinystan(ml.model.int)

# extract the log-likelihood from the intercepts model
log.lik.int <- extract_log_lik(ml.model.int)
loo.int <- loo(log.lik.int)
print(loo.int)
waic.int <- waic(log.lik.int)


# Fit a model with state-level variables only
stan.data.state <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                                     state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                                     year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                                     W = reg.state.mat, M = ncol(reg.state.mat))

# compile the model code
model.state <- stan_model(file = "data/ML model state.stan")

# Fit the model
system.time(
  ml.model.state <- stan(file = "data/ML model state.stan", data = stan.data.state, 
                       iter = 2000, warmup = 1000, chains = 4, cores = 4
  )
)

# Diagnostic checks
launch_shinystan(ml.model.state)

# extract the log-likelihood from the state characteristics model
log.lik.state <- extract_log_lik(ml.model.state)
loo.state <- loo(log.lik.state)
print(loo.state)
waic.state <- waic(log.lik.state)


#### 
# Compare the three models- no appreciable difference. 
diff <- compare(loo.int, loo.state, loo.full)
print(diff)

diff1 <- compare(loo.int, loo.full)
print(diff1)

diff2 <- compare(loo.state, loo.full)
print(diff2)

# WAIC Comparison 
compare(waic.int, waic.state, waic.full)


# Posterior predictive distributions relative to observed data
y = reg.state.comp[, 3]
y.pred.full <- get_posterior_mean(ml.model, pars = "y_pred")
y.pred.int <- get_posterior_mean(ml.model.int, pars = "y_pred")
y.pred.state <- get_posterior_mean(ml.model.state, pars = "y_pred")

posterior.pred.data <- cbind.data.frame(y, y.pred.int[5], y.pred.full[5], y.pred.state[5])

posterior.pred.data <- melt(posterior.pred.data) 

ggplot(posterior.pred.data, aes(x=value, fill = variable)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Predictive Distributions")