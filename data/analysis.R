# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions

# Load packages
library(here)
library(rstan)
library(bayesplot)
library(shinystan)

# Set working directory to current folder 
setwd(here::here())
getwd()

# Environment is determined by use of projects and/or using this file in conjunction with
# the file dataset construction and summary.R 


# Define a state-year level dataset with no missing observations
reg.state.data <- state.char %>%
  select(ccode, year, ln.milex, lag.ln.milex, 
                                        atwar, civilwar, rival.mil, ln.GDP, polity, majpower)

reg.state.data <- reg.state.data[complete.cases(reg.state.data), ]

# Subset data for testing the model- use eastern european states
reg.state.sub <- reg.state.data %>%
                      filter(ccode >= 300 & ccode < 400)
reg.state.sub <- as.matrix(reg.state.sub)


# Define data for STAN model
stan.data <- list(N = nrow(reg.state.sub), y = reg.state.sub[, 3])


# Run the model
system.time(
stan.model <- stan(file = "data/multi-member ML model.stan", data = stan.data, 
                   iter = 4000, chains = 4, cores = 3)
)


# diagnose the model
launch_shinystan(stan.model)
