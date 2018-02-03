# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions

# Load packages
library(here)
library(dplyr)
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

# create a state index variable
reg.state.sub$state.id <- reg.state.sub %>% group_indices(ccode)
# Create a year index variable 
reg.state.sub$year.id <- reg.state.sub %>% group_indices(year)


# transform into a matrix for STAN
reg.state.sub <- as.matrix(reg.state.sub)


# Define data for STAN model
stan.data <- list(N = nrow(reg.state.sub), y = reg.state.sub[, 3],
                  state = reg.state.sub[, 11], S = length(unique(reg.state.sub[, 11])),
                  year = reg.state.sub[, 12], T = length(unique(reg.state.sub[, 12]))
                  )


# Run the model
# TODO(JOSH): transition the model back to a centered parameterization and 
# implement the next part of the fit- alliance-level characteristics 
system.time(
ml.model <- stan(file = "data/multi-member ML model.stan", data = stan.data, 
                   iter = 5000, chains = 4, cores = 3, warmup = 1000
                  )
)


# diagnose the model
launch_shinystan(ml.model)
 