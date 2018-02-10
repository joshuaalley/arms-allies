# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions

# Load packages
library(here)
library(arm)
library(dplyr)
library(rstan)
library(bayesplot)
library(shinystan)
library(loo)

# Set working directory to current folder 
setwd(here::here())
getwd()

# Environment is determined by use of projects and/or using this file in conjunction with
# the file dataset construction and summary.R 


# Define a state-year level dataset with no missing observations
reg.state.data <- state.char %>%
  select(ccode, year, ln.milex, lag.ln.milex, 
                                        atwar, civilwar, rival.mil, ln.GDP, polity, majpower)

# Add state membership in alliances to this data
reg.state.data <- left_join(reg.state.data, state.mem)

# Replace missing alliance values with zero 
reg.state.data[, 11: 355][is.na(reg.state.data[, 11: 355])] <- 0

# Remove observations with missing values
reg.state.comp <- reg.state.data[complete.cases(reg.state.data), ]

# Rescale the outcome and state-level regressors
# reg.state.comp[, 3:10] <- lapply(reg.state.comp[, 3:10], 
#       function(x) rescale(x, binary.inputs = "0/1"))


state.mem.mat <- as.matrix(reg.state.comp[, 11: 355])

# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp %>% group_indices(year)


# Subset data for testing the model- use eastern european states
reg.state.sub <- reg.state.comp %>%
                      filter(ccode >= 300 & ccode < 400)



# create a state index variable
reg.state.sub$state.id <- reg.state.sub %>% group_indices(ccode)
# Create a year index variable 
reg.state.sub$year.id <- reg.state.sub %>% group_indices(year)


# Subset state membership data
state.mem.sub <- reg.state.sub[, 11:20] # was 355 b/c 345 alliances



# Create the matrix of alliance-level variables
reg.all.data <- alliance.char %>%
  select(atopid, prob.det, uncond.det, compellent, bilat, armsreq, dem.prop, wartime, consul.only)

# Create an alliance index variable
alliance.id <- reg.all.data %>% group_indices(atopid)

# Check for missing data
reg.all.data <- reg.all.data[complete.cases(reg.all.data), ]

# Rescale the regressors
# reg.all.data[, 2:9] <- lapply(reg.all.data[, 2:9], 
#                                 function(x) rescale(x, binary.inputs = "0/1"))



### transform data into matrices for STAN
# State-level characeristics
reg.state.mat <- as.matrix(reg.state.sub[, 3:10])

# State membership in alliances
state.mem.sub <- as.matrix(state.mem.sub)
# Index by alliance id
colnames(state.mem.sub) <- alliance.id

# pull the alliance-level regressors into a matrix
alliance.reg.mat <- as.matrix(reg.all.data[, 2:8])
# Add a constant to the alliance-level regression 
cons <- rep(1, length = nrow(alliance.reg.mat))
alliance.reg.mat <- cbind(cons, alliance.reg.mat)

alliance.reg.mat.sub <- alliance.reg.mat[1:10, ]




# Define data for STAN model
stan.data.sub <- list(N = nrow(reg.state.sub), y = reg.state.sub[, 3],
                  state = reg.state.sub$state.id, S = length(unique(reg.state.sub$state.id)),
                  year = reg.state.sub$year.id, T = length(unique(reg.state.sub$year.id)),
                  A = length(alliance.id), L = ncol(alliance.reg.mat),
                  Z = state.mem.sub, 
                  X = alliance.reg.mat.sub)


# Compile the model code
model.1 <- stan_model(file = "data/multi-member ML model.stan")


# Run the model on a subset of the sample
ml.model.vb.sub <- vb(model.1, data = stan.data.sub, seed = 12)

system.time(
ml.model.sub <- stan(file = "data/multi-member ML model.stan", data = stan.data.sub, 
                   iter = 5000, chains = 4, cores = 3, warmup = 1000
                  )
)


# diagnose the model
launch_shinystan(ml.model.vb.sub)
launch_shinystan(ml.model.sub)

# Pairs plot
pairs(ml.model.sub, pars = c("alpha", "sigma_state", "alpha_state[1]", "alpha_state[2]", "alpha_state[15]", 
                         "alpha_state[26]"))



# run model on the full sample

stan.data <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                      state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                      year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                      A = length(alliance.id), L = ncol(alliance.reg.mat),
                      Z = state.mem.mat, 
                      X = alliance.reg.mat)


# Variational Bayes- use to check coefficients on model
ml.model.vb <- vb(model.1, data = stan.data, seed = 12)
launch_shinystan(ml.model.vb)
 
# Regular STAN
system.time(
  ml.model <- stan(file = "data/multi-member ML model.stan", data = stan.data, 
                       iter = 5000, chains = 4, cores = 3, warmup = 1000
  )
)

# diagnose full model
launch_shinystan(ml.model)

# Pairs plots to find sources of autocorrelation in the chains
# Check for correlation between the means and variances of the state intercepts
pairs(ml.model, pars = c("alpha", "sigma_state", "alpha_state[1]", "alpha_state[2]", "alpha_state[15]", 
                             "alpha_state[26]"))

# Check for correlation between the means and variances of the year intercepts
pairs(ml.model, pars = c("alpha", "sigma_year", "alpha_year[2]", "alpha_year[14]", "alpha_year[25]", 
                             "alpha_year[35]"))


# Check for correlation between the state and year intercepts
pairs(ml.model, pars = c("alpha", "alpha_state[2]", "alpha_year[14]", "alpha_state[25]", 
                         "alpha_year[35]"))


