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


# Set seed
set.seed(12)


# Environment is determined by use of projects and/or running this file in conjunction with
# the script dataset construction and summary.R 

# NOTE: To run the variational Bayes algorithm, must compile the model file on your machine 
# In Windows, failure to compile the model on your machine will lead to a dynamic link library intitialization error


# Define a state-year level dataset with no missing observations
reg.state.data <- state.vars %>%
  select(ccode, year, ln.milex, lag.ln.milex,
                      atwar, civilwar.part, rival.milex, ln.gdp, polity, 
                      cold.war, disputes, majpower) 

# Add state membership in alliances to this data
reg.state.data <-  left_join(reg.state.data, state.mem.contrib) 


# Replace missing alliance values with zero 
reg.state.data[, 13: ncol(reg.state.data)][is.na(reg.state.data[, 13: ncol(reg.state.data)])] <- 0

# Remove observations with missing values
reg.state.comp <- reg.state.data[complete.cases(reg.state.data), ]


# Rescale the state-level regressors- but not the LDV
 reg.state.comp[, 5:11] <- lapply(reg.state.comp[, 5:11], 
       function(x) rescale(x, binary.inputs = "0/1"))

 
# Check the range and distribution of the DV
summary(reg.state.comp$ln.milex)
sd(reg.state.comp$ln.milex)
ggplot(reg.state.comp, aes(ln.milex)) + geom_density()



# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.mat <- as.matrix(reg.state.comp[, 13: ncol(reg.state.comp)])


# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp %>% group_indices(year)



# Create the matrix of alliance-level variables
# Make the alliance characteristics data match the membership matrix
reg.all.data <- filter(alliance.char, atopid %in% colnames(state.mem.mat)) %>%
  select(atopid, uncond.milsup, offense, num.mem, 
          dem_prop, wartime, organ1, milaid.rc, asymm, us.mem, ussr.mem)


# Create an alliance index variable
alliance.id <- reg.all.data %>% group_indices(atopid)

# Replace missing conditions (arms, instituions and military aid) with zeros
reg.all.data[is.na(reg.all.data)] <- 0



### transform data into matrices for STAN
# State-level characeristics
reg.state.mat <- as.matrix(reg.state.comp[, 4:12])

# check correlations among state-level regressors
cor(reg.state.mat, method = "pearson")

# State membership in alliances
# pull the alliance-level regressors into a matrix
alliance.reg.mat <- as.matrix(reg.all.data[, 2: ncol(reg.all.data)])
cor(alliance.reg.mat, method = "pearson")

# Add a constant to the alliance-level regression 
cons <- rep(1, length = nrow(alliance.reg.mat))
alliance.reg.mat <- cbind(cons, alliance.reg.mat)


# run model on the full sample
# Define the data list 
reg.state.comp <- as.data.frame(reg.state.comp)
stan.data <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                      state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                      year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                      A = length(alliance.id), L = ncol(alliance.reg.mat),
                      Z = state.mem.mat, 
                      X = alliance.reg.mat,
                      W = reg.state.mat, M = ncol(reg.state.mat)
)

# Compile the model code
model.1 <- stan_model(file = "data/multi-member ML model.stan")

# Variational Bayes- use to check coefficients and posterior predictions on model
ml.model.vb <- vb(model.1, data = stan.data, seed = 12)
launch_shinystan(ml.model.vb)

# posterior predictive check from variational Bayes- did not converge
# so treat these predictions with caution
y = reg.state.comp[, 3]
vb.model.sum <- extract(ml.model.vb)
ppc_dens_overlay(y, vb.model.sum$y_pred[1:100, ])

# Remove vb model and associated summary
rm(ml.model.vb)
rm(vb.model.sum)
 
# Regular STAN
system.time(
  ml.model <- sampling(model.1, data = stan.data, 
                       iter = 2000, warmup = 1000, chains = 4
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

# plot posterior predictive denisty of first 100 simulations
ppc_dens_overlay(y, yrep.full[1:100, ])





# Plot all the beta coefficients and calculate posterior probabilities
# label columns
colnames(ml.model.sum$beta) <- colnames(alliance.reg.mat)


mean(ml.model.sum$beta[, 2] > 0) # posterior probability that a probabilistic alliance is associated with increased defense spending
mean(ml.model.sum$beta[, 3] > 0) # Posterior probability that a conditional alliance is associated with increased spending
mean(ml.model.sum$beta[, 4] < 0) # posterior probability that an unconditional deterrent alliance is associated with decreased spending
mean(ml.model.sum$beta[, 5] > 0) # Posterior probability that a compellent alliance is associated with increased spending
mean(ml.model.sum$beta[, 6] > 0) # Posterior probability that more members in an alliance is associated with increased spending
mean(ml.model.sum$beta[, 7] < 0) # Posterior probability that more democratic members in the initial alliance is associated with decreased spending
mean(ml.model.sum$beta[, 8] > 0) # Posterior probability that a partner in the initial alliance was at war during formation is associated with increased spending
mean(ml.model.sum$beta[, 9] > 0) # Posterior probability that greater alliance institutionalization is associated with increased spending
mean(ml.model.sum$beta[, 10] > 0) # Posterior probability that military aid conditions are associated with increased spending
mean(ml.model.sum$beta[, 11] < 0) # Posterior probability that US membership is associated with decreased spending
mean(ml.model.sum$beta[, 12] < 0) # Posterior probability that Russian membership is associated with decreased spending



beta.melt <- melt(ml.model.sum$beta)

# Plot density of the coefficients
ggplot(beta.melt, aes(x=value, fill = Var2)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Distribution of Alliance-Level Regression Coefficients")

# Summarize intervals
beta.summary <- summary(ml.model, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary <- beta.summary[, -2]
rownames(beta.summary) <- c("Constant", "Probabilistic Deterrent", "Conditional", "Unconditional Deterrent", 
                            "Compellent", "Num. Members", "Share Dem. Members", 
                            "Wartime", "Institutionalization", "Military aid",
                            "US Member", "Russia Member", "sigma Alliances")

print(beta.summary)
xtable(beta.summary, digits = 3)


### Compare alliance coefficients
sd(ml.model.sum$beta[, 4])
# Compare unconditional and conditional coefficients
sd(ml.model.sum$beta[, 3])
mean(ml.model.sum$beta[, 4] < ml.model.sum$beta[, 3])
# Compare unconditional and probabilistic deterrent coefs
sd(ml.model.sum$beta[, 2])
mean(ml.model.sum$beta[, 4] < ml.model.sum$beta[, 2])
# Differences in posterior overlap are a function of greater uncertainty in the probabilistic deterrent estimate
# Posterior means
mean(ml.model.sum$beta[, 4]) # unconditional 
mean(ml.model.sum$beta[, 3]) # conditional
mean(ml.model.sum$beta[, 2]) # probabilistic deterrent



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
mean(ml.model.sum$gamma[, 7] > 0) # posterior probability that Cold war years are associated with increased spending

gamma.melt <- melt(ml.model.sum$gamma)

# Plot density of the coefficients
ggplot(gamma.melt, aes(x=value,  fill = Var2)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Distribution of State-Level Covariates")


# summarize posterior intervals
gamma.summary <- summary(ml.model, pars = c("gamma", "sigma_state", "alpha"), probs = c(0.05, 0.95))$summary
gamma.summary <- gamma.summary[, -2]
rownames(gamma.summary) <- c("Lagged Expenditures", "Wartime", "Civil War", "Rival Mil. Expenditure", 
                            "ln(GDP)", "Polity", "Cold War", "Sigma State", "Constant")
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
rownames(coef.probs) <- c("Alliance Model Constant", "Probabilistic Deterrent", "Conditional", "Unconditional", 
                          "Compellent", "Number of Members", 
                          "Share Dem. Members", "Wartime Alliance", "Insitutionalization", 
                          "Military Aid", "US Member", "Russia Member",
                          "Lag Expenditures",
                          "Interstate War", "Civil War", "Rival Expenditure",
                            "ln(GDP)", "Polity", "Cold War")
coef.probs$variable <- rownames(coef.probs)
coef.probs$variable <- reorder(coef.probs$variable, coef.probs$`Posterior Probability of Positive Coefficient`)
  
# Plot
ggplot(coef.probs, aes(x = variable, y = `Posterior Probability of Positive Coefficient`)) + 
  geom_col() +
  geom_text(aes(label = `Posterior Probability of Positive Coefficient`), nudge_y = .0675) +
  coord_flip()
ggsave("figures/post-prob.png", height = 6, width = 8)



# Calculate substantive impact of an unconditional deterrent alliance
# LRM = beta / (1 - alpha), where alpha is the coef on the lagged DV
lrm.uncond <- ml.model.sum$beta[, 4] / (1 - ml.model.sum$gamma[, 1])
summary(lrm.uncond)
plot(density(lrm.uncond))

positive.check(lrm.uncond)

lrm.dem <- ml.model.sum$gamma[, 6] / (1 - ml.model.sum$gamma[, 1])
summary(lrm.dem)
mean(lrm.dem < 0)
mean(lrm.uncond < lrm.dem)



#### Plots lambdas
## Extract lambdas from fit and combine with covariates
lambda_means <- get_posterior_mean(ml.model, pars = "lambda")
lambda_df <- data_frame(lambda = lambda_means[, 5]) %>%  # add lambdas to df
  bind_cols(alliance.char.model) %>%  # add alliance characteristics to df
  mutate(alliance.type = ifelse(prob_det == 1, "Probabilistic Deterrent", 
                                ifelse(unconditional == 1, "Unconditional",
                                       ifelse(conditional == 1, "Conditional Deterrent", 
                                          ifelse(compellent == 1, "Compellent", "No Support")))))

# check the validity of the type variable
count(lambda_df, alliance.type)
# All compellent alliances are nested under unconditional pacts
                                              
                                                                                           
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




## Violin plot for lambdas, broken down by military aid
ggplot(lambda_df, aes(x = milaid.rc, y = lambda)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3)) + # use circle and +
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Military Aid and Size")

## plot for lambdas, broken down by wartime
ggplot(lambda_df, aes(x = wartime, y = lambda)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3)) + # use circle and +
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Wartime and Size")

## plot for lambdas, broken down by instititutionalization
ggplot(lambda_df, aes(x = organ1, y = lambda)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3)) + # use circle and +
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Alliance Institutionalization and Size")


## plot for lambdas, broken down by bilateral factor
ggplot(lambda_df, aes(x = bilat, y = lambda)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem)) + 
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Alliance Size")



## plot for lambdas, broken down by whether pact is consultation only (to compare Benson with ATOP)
ggplot(lambda_df, aes(x = onlyconsul, y = lambda)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem, shape = factor(bilat))) +  # make size and shape corresponde to number of members
  scale_shape_manual(values = c(1, 3)) + # use circle and +
  ggtitle("Distribution of Mean Predicted Alliance Intercepts by Consultation Only and Size")



### Summarize the distribution of the estimated lambdas
# General density plot of the means
ggplot(lambda_df, aes(x = lambda)) + geom_density() + ggtitle("Density of Posterior Means for all Alliance Intercepts")

# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs <- apply(ml.model.sum$lambda, 2, positive.check)
lambda.probs <- cbind.data.frame(alliance.char.model$atopid, round(lambda_df$lambda, digits = 4), lambda_df$alliance.type, lambda.probs)
colnames(lambda.probs) <- c("atopid", "lambda.mean", "alliance.type", "pos.post.prob")
# binary indicator if posterior probability is greater than 90$ for positive or negative
lambda.probs$non.zero <- ifelse(lambda.probs$pos.post.prob >= .89 | lambda.probs$pos.post.prob <= .11, 1, 0)
sum(lambda.probs$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs$atopid <- reorder(lambda.probs$atopid, lambda.probs$pos.post.prob)

# For all alliances
ggplot(lambda.probs, aes(x = atopid, y = pos.post.prob, fill = alliance.type)) + 
  geom_col() +
  coord_flip()

# Remove the none category from these alliances
lambda.probs %>% 
  filter(alliance.type != "No Support") %>% 
ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = alliance.type)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = alliance.type)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
  
# Plot the lambda means, coloring by type
lambda.probs$atopid <- reorder(lambda.probs$atopid, lambda.probs$lambda.mean)
ggplot(lambda.probs, aes(x = atopid, y = lambda.mean, fill = alliance.type)) + 
  geom_col() 

# remove the none category
lambda.probs %>% 
  filter(alliance.type != "No Support") %>%
ggplot(aes(x = atopid, y = lambda.mean, fill = alliance.type)) + 
  geom_col() 

# For non-zero alliances 
lambda.probs %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = alliance.type)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = lambda.mean), nudge_y = 0.01, size = 4) +
  labs(y = "Posterior Mean of Alliance Weight Parameter") +
  coord_flip()
ggsave("figures/non-zero alliances.png", height = 6, width = 8)




### 
# Plot posterior densities of variance parameters
sigma.df <- cbind(ml.model.sum$sigma_year, ml.model.sum$sigma_state, ml.model.sum$sigma_all)
colnames(sigma.df) <- c("sigma.year", "sigma.state", "sigma.all")
sigma.df <- as.data.frame(sigma.df)

ggplot(sigma.df, aes(x = sigma.year)) + geom_density() + ggtitle("Posterior Density of Year Variance Parameter")
ggplot(sigma.df, aes(x = sigma.state)) + geom_density() + ggtitle("Posterior Density of State Variance Parameter")
ggplot(sigma.df, aes(x = sigma.all)) + geom_density() + ggtitle("Posterior Density of Alliance Variance Parameter")

# check the extent of overlap
mean(sigma.df$sigma.state > sigma.df$sigma.all)
mean(sigma.df$sigma.year > sigma.df$sigma.all)
mean(sigma.df$sigma.year > sigma.df$sigma.state)

# plot all three variance parameters together
sigma.df.melt <- melt(sigma.df) 

ggplot(sigma.df.melt, aes(x = value, fill = variable)) + geom_density() +
 scale_fill_brewer(palette = "Greys") +
 ggtitle("Posterior Densities of Variance Hyperparameters") + theme_classic()
ggsave("figures/variance-hyperparam-plot.png", height = 6, width = 8)


# Calculate the R^2 of the alliance level model
theta_means <- get_posterior_mean(ml.model, pars = "theta")
1 - (mean(ml.model.sum$sigma_all)^2 / var(theta_means[, 5]))

# Calculate the R^2 of the state level model
yhat_means <- get_posterior_mean(ml.model, pars = "y_hat")
1 - (mean(ml.model.sum$sigma)^2 / var(yhat_means[, 5]))

###
# Check what types of alliances the US and Russia are part of in the estimation sample
filter(alliance.char.model, us.mem == 1) %>%
  summarize(
    us.uncond = sum(unconditional, na.rm = TRUE),
    us.cond = sum(conditional, na.rm = TRUE),
    us.prob.det = sum(prob_det, na.rm = TRUE),
    us.comp = sum(compellent, na.rm = TRUE),
    us.bilat = sum(bilat, na.rm = TRUE)
  )

filter(alliance.char.model, russ.mem == 1) %>%
  summarize(
    russ.uncond = sum(unconditional, na.rm = TRUE),
    russ.cond = sum(conditional, na.rm = TRUE),
    russ.prob.det = sum(prob_det, na.rm = TRUE),
    russ.comp = sum(compellent, na.rm = TRUE),
    russ.bilat = sum(bilat, na.rm = TRUE)
  )

# Plot differences in control variables across alliances
# Group by alliance type variables and calculate means
alliance.reg.summary <- alliance.char.model %>%
  mutate(
  alliance.type = ifelse(prob_det == 1, "Probabilistic Deterrent", 
                         ifelse(unconditional == 1, "Unconditional",
                                ifelse(conditional == 1, "Conditional Deterrent", "No Support"))),
  num.mem = num.mem / (2 * sd(num.mem)) # rescale the number of members by 2 sd 
  ) %>%
  group_by(alliance.type) %>%
  summarize(
    compellent = mean(compellent, na.rm = TRUE),
    num.mem = mean(num.mem, na.rm = TRUE), 
    dem.prop = mean(dem.prop, na.rm = TRUE),
    wartime = mean(wartime, na.rm = TRUE), 
    organ1 = mean(organ1, na.rm = TRUE),
    milaid.rc = mean(milaid.rc, na.rm = TRUE),
    us.mem = mean(us.mem, na.rm = TRUE), 
    russ.mem = mean(russ.mem, na.rm = TRUE)
    )

# Melt for use with ggplot2
alliance.reg.summary <- melt(alliance.reg.summary, 
id.vars = c("alliance.type"))

# Heatmap
ggplot(data = alliance.reg.summary, aes(x= alliance.type, y = variable, fill=value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "white", high = "black", name = "Mean") +
  labs(x = "Alliance Type", y = "Variable") +
  scale_y_discrete(labels = c("Compellent", "Number of Members", "Share Democ.",
                              "Wartime", "Institutionalization", "Military Aid", "US member", "Russia Member")) +
  theme_classic() 


# summarize session info
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))

devtools::session_info("rstan")

                       