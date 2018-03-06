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
library(reshape2)
library(party)
library(xtable)

# Set working directory to current folder 
setwd(here::here())
getwd()

# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Environment is determined by use of projects and/or running this file in conjunction with
# the script dataset construction and summary.R 

# NOTE: To run the variational Bayes algorithm, must compile the model file on your machine 
# In Windows, failure to compile the model on your machine will lead to a dynamic link library intitialization error


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

# Rescale the state-level regressors
 reg.state.comp[, 4:10] <- lapply(reg.state.comp[, 4:10], 
       function(x) rescale(x, binary.inputs = "0/1"))

 
# Check the range and distribution of the DV
summary(reg.state.comp$ln.milex)
sd(reg.state.comp$ln.milex)
ggplot(reg.state.comp, aes(ln.milex)) + geom_density()



# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.mat <- as.matrix(reg.state.comp[, 11: 355])

# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp %>% group_indices(year)



# Create the matrix of alliance-level variables
reg.all.data <- alliance.char %>%
  select(atopid, prob.det, uncond.det, compellent, bilat, armsreq, dem.prop, wartime, onlyconsul)

# Create an alliance index variable
alliance.id <- reg.all.data %>% group_indices(atopid)

# Check for missing data
reg.all.data <- reg.all.data[complete.cases(reg.all.data), ]

# Rescale the regressors
# reg.all.data[, 2:9] <- lapply(reg.all.data[, 2:9], 
#                                 function(x) rescale(x, binary.inputs = "0/1"))



### transform data into matrices for STAN
# State-level characeristics
reg.state.mat <- as.matrix(reg.state.comp[, 4:10])

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

# Variational Bayes- use to check coefficients and posterior predictions on model
ml.model.vb <- vb(model.1, data = stan.data, seed = 12)
launch_shinystan(ml.model.vb)

# posterior predictive check from variational Bayes- did not converge
# so treat these predictions with caution
y = reg.state.comp[, 3]
vb.model.sum <- extract(ml.model.vb)
ppc_dens_overlay(y, vb.model.sum$y_pred[1:100, ])


 
# Regular STAN
system.time(
  ml.model <- stan(file = "data/multi-member ML model.stan", data = stan.data, 
                       iter = 2000, warmup = 1000, chains = 4, cores = 4
  )
)

# diagnose full model
launch_shinystan(ml.model)

check_hmc_diagnostics(ml.model)

# Traceplots for the regression parameters
traceplot(ml.model, pars = "beta")
traceplot(ml.model, pars = "gamma")

# Pairs plots to see sources of autocorrelation in the chains: Select only some intercept parameters
# Check for correlation between the means and variances of the state intercepts
pairs(ml.model, pars = c("alpha", "sigma_state", "alpha_state[1]", "alpha_state[2]", "alpha_state[15]", 
                             "alpha_state[26]"))

# Check for correlation between the means and variances of the year intercepts
pairs(ml.model, pars = c("alpha", "sigma_year", "alpha_year[2]", "alpha_year[14]", "alpha_year[25]", 
                             "alpha_year[35]"))


# Check for correlation between the state and year intercepts
pairs(ml.model, pars = c("alpha", "alpha_state[2]", "alpha_year[14]", "alpha_state[25]", 
                         "alpha_year[35]"))

# Check correlation among the parameters
# Alliance-level variables
pairs(ml.model, pars = c("beta[1]", "beta[2]", "beta[3]", "beta[4]",
                         "beta[5]", "beta[6]", "beta[7]", "beta[8]"))

# State-level variables
pairs(ml.model, pars = c("gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]",
                         "gamma[5]", "gamma[6]", "gamma[7]"))


# Extract coefficients from the model
ml.model.sum <- extract(ml.model, permuted = TRUE)

# Posterior predictive distributions relative to observed data
yrep.full <- ml.model.sum$y_pred

# plot posterior predictive denisty of first 50 simulations
ppc_dens_overlay(y, yrep.full[1:100, ])





# Plot all the beta coefficients and calculate posterior probabilities
# label columns
colnames(ml.model.sum$beta) <- colnames(alliance.reg.mat)


mean(ml.model.sum$beta[, 2] > 0) # posterior probability that a probabilistic alliance is associated with increased defense spending
mean(ml.model.sum$beta[, 3] < 0) # posterior probability that an unconditional deterrent alliance is associated with decreased spending
mean(ml.model.sum$beta[, 4] > 0) # Posterior probability that a compellent alliance is associated with increased spending
mean(ml.model.sum$beta[, 5] > 0) # Posterior probability that a bilateral alliance is associated with increased spending
mean(ml.model.sum$beta[, 6] > 0) # Posterior probability that a arms requirements in an alliance are associated with increased spending
mean(ml.model.sum$beta[, 7] < 0) # Posterior probability that more democratic members in the initial alliance is associated with decreased spending
mean(ml.model.sum$beta[, 8] > 0) # Posterior probability that a partner in the initial alliance was at war during formation is associated with increased spending

beta.melt <- melt(ml.model.sum$beta)

# Plot density of the coefficients
ggplot(beta.melt, aes(x=value, fill = Var2)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Distribution of Alliance-Level Regression Coefficients")

# Summarize intervals
beta.summary <- summary(ml.model, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
rownames(beta.summary) <- c("Constant", "Probabilistic Deterrent", "Unconditional Deterrent", 
                            "Compellent", "Bilateral", "Arms Requirement", "Share Dem. Members", 
                            "Wartime", "sigma Alliances")
print(beta.summary)
xtable(beta.summary, digits = 3)


# Similar calculations for the state-level variables
# label columns
colnames(ml.model.sum$gamma) <- colnames(reg.state.mat)

# posterior probabilities
mean(ml.model.sum$gamma[, 1] > 0) # posterior probability that lagged DV is positive
mean(ml.model.sum$gamma[, 2] > 0) # posterior probability that being at war leads to increased defense spending
mean(ml.model.sum$gamma[, 3] > 0) # posterior probability that civil wars lead to increased defense spending
mean(ml.model.sum$gamma[, 4] > 0) # posterior probability that rival military expenditures are associated with increased spending
mean(ml.model.sum$gamma[, 5] > 0) # Posterior probability that GDP is associated with increased spending
mean(ml.model.sum$gamma[, 6] < 0) # posterior probability that democracies tend to decrease spending
mean(ml.model.sum$gamma[, 7] > 0) # posterior probability that major powers increase spending

gamma.melt <- melt(ml.model.sum$gamma)

# Plot density of the coefficients
ggplot(gamma.melt, aes(x=value,  fill = Var2)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Distribution of State-Level Covariates")


# summarize posterior intervals
gamma.summary <- summary(ml.model, pars = c("gamma", "sigma_state", "alpha"), probs = c(0.05, 0.95))$summary
rownames(gamma.summary) <- c("Lagged Expenditures", "Wartime", "Civil War", "Rival Mil. Expenditure", 
                            "ln(GDP)", "Polity", "Major Power", "Sigma State", "Constant")
print(gamma.summary)
xtable(gamma.summary)


# Plot posterior probabilities that a given coefficient is positive
positive.check <- function(x){
  mean(x > 0)
}

beta.probs <- apply(ml.model.sum$beta, 2, positive.check)
gamma.probs <- apply(ml.model.sum$gamma, 2, positive.check)

# append in a dataframe to be used for plotting
coef.probs <- as.data.frame(append(beta.probs, gamma.probs))
colnames(coef.probs) <- c("Posterior Probability of Positive Coefficient")
rownames(coef.probs) <- c("Alliance Model Constant", "Probabilistic Deterrent", "Unconditional Deterrent", 
                          "Compellent", "Bilateral", "Arms Requirement", "Share Dem. Members", 
                          "Lag Expenditures",
                          "Wartime Alliance",  "Interstate War", "Civil War", "Rival Mil. Expenditure", 
                            "ln(GDP)", "Polity", "Major Power")
coef.probs$variable <- rownames(coef.probs)
coef.probs$variable <- reorder(coef.probs$variable, coef.probs$`Posterior Probability of Positive Coefficient`)
  
# Plot
ggplot(coef.probs, aes(x = variable, y = `Posterior Probability of Positive Coefficient`)) + 
  geom_col() +
  geom_text(aes(label = `Posterior Probability of Positive Coefficient`), nudge_y = .05) +
  coord_flip()
ggsave("figures/post-prob.png", height = 6, width = 8)



# Plots lambdas
## Extract lambdas from fit and combine with covariates
lambda_means <- get_posterior_mean(ml.model, pars = "lambda")
lambda_df <- data_frame(lambda = lambda_means[, 5]) %>%  # add lambdas to df
  bind_cols(alliance.char) %>%  # add alliance characteristics to df
  mutate(alliance.type = ifelse(prob.det == 1, "Probabilistic Deterrent",  # create a single factors that indicates the categories
                                ifelse(uncond.det == 1, "Unconditional Deterrent",
                                       ifelse(cond_det == 1, "Conditional Deterrent", 
                                              ifelse(pure_cond_det == 1, "Pure Conditional Deterrent", "Compellent")))))
## Violin plot for lambdas
ggplot(lambda_df, aes(x = alliance.type, y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3)) + # use circle and +
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Alliance Type and Size")
ggsave("figures/lambda-box.png", height = 6, width = 8)

# Use random forest to assess variable importance
rf <- cforest(lambda ~ ., data = select(lambda_df, -alliance.type))  # fit forest
vi <- varimp(rf)  # calculate variable importance
vi_df <- data_frame(var_name = names(vi), importance = vi) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi_df, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Alliance Covariates from a Random Forest Model")
ggsave("figures/varimp.png", height = 6, width = 8)






# summarize session info
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))

devtools::session_info("rstan")

                       