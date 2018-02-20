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

# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Environment is determined by use of projects and/or using this file in conjunction with
# the script dataset construction and summary.R 

# NOTE: To run the variational Bayes algorithm, must compile the model file on your machine 
# In Windows, failure to compile the model on your machine will lead to a dynamic link library intitialization error


# Define a state-year level dataset with no missing observations
reg.state.data <- state.char %>%
  select(ccode, year, change.ln.milex, 
                      atwar, civilwar, rival.mil, ln.GDP, polity, majpower)

# Add state membership in alliances to this data
reg.state.data <- left_join(reg.state.data, state.mem)

# Replace missing alliance values with zero 
reg.state.data[, 10: 354][is.na(reg.state.data[, 10: 354])] <- 0

# Remove observations with missing values
reg.state.comp <- reg.state.data[complete.cases(reg.state.data), ]

# Rescale the state-level regressors
 reg.state.comp[, 4:9] <- lapply(reg.state.comp[, 4:9], 
       function(x) rescale(x, binary.inputs = "0/1"))


# Check the range and distribution of the DV
summary(reg.state.comp$change.ln.milex)
sd(reg.state.comp$change.ln.milex)
plot(density(reg.state.comp$change.ln.milex))

# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.mat <- as.matrix(reg.state.comp[, 10: 354])

# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp %>% group_indices(year)



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
reg.state.mat <- as.matrix(reg.state.comp[, 4:9])

# check correlations among state-level regressors
cor(reg.state.mat, method = "pearson")

# State membership in alliances
# pull the alliance-level regressors into a matrix
alliance.reg.mat <- as.matrix(reg.all.data[, 2:8])

# check correlation of the alliance-level regressors
cor(alliance.reg.mat, method = "pearson")

# Add a constant to the alliance-level regression 
cons <- rep(1, length = nrow(alliance.reg.mat))
alliance.reg.mat <- cbind(cons, alliance.reg.mat)

alliance.reg.mat.sub <- alliance.reg.mat[1:10, ]



# run model on the full sample
# Define the data list 
stan.data <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                      state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                      year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                      A = length(alliance.id), L = ncol(alliance.reg.mat),
                      Z = state.mem.mat, 
                      X = alliance.reg.mat,
                      W = reg.state.mat, M = ncol(reg.state.mat))


# Compile the model code for the variational Bayes algorithm
model.1 <- stan_model(file = "data/multi-member ML model.stan")

# Variational Bayes- use to check coefficients on model
ml.model.vb <- vb(model.1, data = stan.data, seed = 12)
launch_shinystan(ml.model.vb)

# Plots lambdas
## Extract lambdas from fit and combine with covariates
lambda_means <- get_posterior_mean(ml.model.vb, pars = "lambda")
lambda_df <- data_frame(lambda = as.numeric(lambda_means)) %>%  # add lambdas to df
  bind_cols(alliance.char) %>%  # add alliance characteristics to df
  mutate(alliance.type = ifelse(prob.det == 1, "Probabilistic Deterrence",  # create a single factors that indicates the categories
                                ifelse(uncond.det == 1, "Unconditional Deterrence",
                                       ifelse(cond_det == 1, "Conditional Deterrence", "Other"))))
## Violin plot for lambdas
ggplot(lambda_df, aes(x = alliance.type, y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3))  # use circle and +
ggsave("lambda-box.png", height = 6, width = 8)

# Use random forest to assess variable importance
library(party)
rf <- cforest(lambda ~ ., data = select(lambda_df, -alliance.type))  # fit forest
vi <- varimp(rf)  # calculate variable importance
vi_df <- data_frame(var_name = names(vi), importance = vi) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi_df, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip()
ggsave("varimp.png", height = 6, width = 8)
 
# Regular STAN
system.time(
  ml.model <- stan(file = "data/multi-member ML model.stan", data = stan.data, 
                       iter = 1000, warmup = 500, chains = 4, cores = 4
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





# summarize session info
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))

devtools::session_info("rstan")

                       