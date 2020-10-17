# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions


# Environment is determined by use of projects and/or running this file in conjunction with
# the scripts alliance-measures.R and dataset construction and summary.R 

### Model with varying slopes

# Compile the model code: 
model.lmbcap <- stan_model(file = "data/ml-model-lmbcap.stan")


# Variational Bayes- use to check model will run 
# this crashes with varying slopes. 
# ml.model.vb.lmbcap <- vb(model.lmbcap, 
#                          data = stan.data.lmbcap, seed = 12,
#                           iter = 1000)
# # launch_shinystan(ml.model.vb.lmbcap)
# 
# # Remove vb model and associated summary
# rm(list = c("ml.model.vb.lmbcap"))


# Regular STAN: takes awhile
system.time(
  ml.model.lmbcap <- sampling(model.lmbcap, data = stan.data.lmbcap, 
                              iter = 2000, warmup = 1000, chains = 4,
                              control=list(adapt_delta = 0.95, 
                                           max_treedepth = 20),
                              pars = c("beta", "lambda_sm", "lambda_lg", "gamma",
                                       "nu", "alpha", "sigma", "sigma_state", "sigma_year",
                                       "sigma_all_lg", "sigma_all_sm",
                                       "L_Omega_beta", "tau_beta"),
                              include = TRUE
  )
)


saveRDS(ml.model.lmbcap, "data/est-lmbcap.rds")

# diagnose full model
# only run shinystan line from console- can't run whole script otherwise
# launch_shinystan(ml.model.lmbcap)

check_hmc_diagnostics(ml.model.lmbcap)

# Traceplots for the regression parameters
traceplot(ml.model.lmbcap, pars = "beta")
traceplot(ml.model.lmbcap, pars = "gamma")
traceplot(ml.model.lmbcap, pars = "L_Omega_beta")
traceplot(ml.model.lmbcap, pars = "tau_beta")

# Pairs plots to see sources of autocorrelation in the chains: Select only some intercept parameters
# Check for correlation between the means and variances of the state intercepts
pairs(ml.model.lmbcap, pars = c("alpha", "sigma_state", "alpha_state[1]", "alpha_state[2]", "alpha_state[15]", 
                             "alpha_state[26]"))

# Check for correlation between the means and variances of the year intercepts
pairs(ml.model.lmbcap, pars = c("alpha", "sigma_year", "alpha_year[2]", "alpha_year[14]", "alpha_year[25]", 
                             "alpha_year[35]"))


# Check for correlation between the state and year intercepts
pairs(ml.model.lmbcap, pars = c("alpha", "alpha_state[2]", "alpha_year[14]", "alpha_state[25]", 
                         "alpha_year[35]"))



# Extract coefficients from the model
ml.model.sum <- extract(ml.model.lmbcap, pars = c("beta", "gamma", "lambda_sm", "lambda_lg",
                                           "sigma", "sigma_year", "sigma_state",
                                           "sigma_all_sm", "sigma_all_lg"),
                        permuted = TRUE)


# Plot all the beta coefficients and calculate posterior probabilities

# depth index
mean(ml.model.sum$beta[, 1, 2] < 0) # Small
mean(ml.model.sum$beta[, 2, 2] < 0) # Large
# unconditional military support
mean(ml.model.sum$beta[, 1, 3] < 0) # Small
mean(ml.model.sum$beta[, 2, 3] < 0) # Large
# econonmic agreement
mean(ml.model.sum$beta[, 1, 4] < 0) # Small
mean(ml.model.sum$beta[, 2, 4] > 0) # Large
# FP concessions
mean(ml.model.sum$beta[, 1, 5] > 0) # Small
mean(ml.model.sum$beta[, 2, 5] < 0) # Large
# number of members
mean(ml.model.sum$beta[, 1, 6] < 0) # Small
mean(ml.model.sum$beta[, 2, 6] > 0) # Large
# FP similarity
mean(ml.model.sum$beta[, 1, 7] > 0) # Small
mean(ml.model.sum$beta[, 2, 7] > 0) # Large
# avg democracy
mean(ml.model.sum$beta[, 1, 8] < 0) # Small
mean(ml.model.sum$beta[, 2, 8] > 0) # Large
# wartime
mean(ml.model.sum$beta[, 1, 9] > 0) # Small
mean(ml.model.sum$beta[, 2, 9] < 0) # Large
# asymmetric obligations
mean(ml.model.sum$beta[, 1, 10] < 0) # Small
mean(ml.model.sum$beta[, 2, 10] > 0) # Large
# mean threat
mean(ml.model.sum$beta[, 1, 11] > 0) # Small
mean(ml.model.sum$beta[, 2, 11] > 0) # Large


# Figure out way to combine below intervals 
dimnames(ml.model.sum$beta)[[3]] <- c("Constant", "Depth", "Uncond. Milsup.", "Econ. Link", 
                                      "FP Conc.",
                                      "Number Members", "FP Similarity",
                                      "Democratic Membership", 
                                      "Wartime", "Asymmetric", "Mean Threat")
dimnames(ml.model.sum$beta)[[2]] <- c("Small", "Large")

mcmc_intervals(ml.model.sum$beta[, 1, ], 
               prob = .9)
mcmc_intervals(ml.model.sum$beta[, 2, ], 
           prob = .9)

# Summarize intervals
beta.summary <- summary(ml.model.lmbcap, pars = c("beta"), 
                        probs = c(0.05, 0.95))$summary
beta.summary <- beta.summary[, -2]
rownames(beta.summary) <- c("Constant: Small", "Latent Depth: Small", 
                            "Uncond. Supp: Small", "Econ. Agreement: Small",
                            "FP Concessions: Small",
                            "Number Members: Small", "FP Similarity: Small",
                            "Democratic Membership: Small", 
                            "Wartime: Small", "Asymmetric Ob: Small",
                            "Mean Threat: Small", 
                            "Constant: Large", "Latent Depth: Large",
                            "Uncond. Supp: Large", "Econ. Agreement: Large",
                            "FP Concessions: Large",
                            "Number Members: Large", "FP Similarity: Large",
                            "Democratic Membership: Large", 
                            "Wartime: Large", "Asymmetric Ob: Large",
                            "Mean Threat: Large")

print(beta.summary)
xtable(beta.summary, digits = 3)


# Create plot of the scope parameters
depth.dens.joint <- cbind(ml.model.sum$beta[, 2, 2], ml.model.sum$beta[, 1, 2])
colnames(depth.dens.joint) <- c("Large", "Small")
depth.dens.joint <- melt(depth.dens.joint)

ggplot(depth.dens.joint, aes(x = value,  fill = Var2)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(name = "Sample", values=c("#999999", "#000000")) +
  ggtitle("Posterior Distributions of Treaty Scope: Large and Small Powers") +
  theme_classic()
ggsave("appendix/depth-dens-joint.png", height = 6, width = 8)





# Similar calculations for the state-level variables
# label columns
colnames(ml.model.sum$gamma) <- colnames(reg.state.mat)

# posterior probabilities
mean(ml.model.sum$gamma[, 1] > 0) # at war
mean(ml.model.sum$gamma[, 2] > 0) # civil war participation
mean(ml.model.sum$gamma[, 3] > 0) # rival military expenditures 
mean(ml.model.sum$gamma[, 4] > 0) # gdp growth
mean(ml.model.sum$gamma[, 5] < 0) # POLITY
mean(ml.model.sum$gamma[, 6] > 0) # Cold war years 
mean(ml.model.sum$gamma[, 7] > 0) # number of disputes

gamma.melt <- melt(ml.model.sum$gamma)

# Plot density of the coefficients
ggplot(gamma.melt, aes(x=value,  fill = Var2)) +
  geom_density(alpha=0.25) +
  ggtitle("Posterior Distribution of State-Level Covariates")


# summarize posterior intervals
gamma.summary <- summary(ml.model.lmbcap, pars = c("gamma", "sigma_state", "alpha"),
                         probs = c(0.05, 0.95))$summary
gamma.summary <- gamma.summary[, -2]
rownames(gamma.summary) <- c("Wartime", "Civil War", "Rival Mil. Expenditure", 
                            "ln(GDP)", "Polity", "Cold War", "Disputes",
                            "Sigma State", "Intercept")
print(gamma.summary)
xtable(gamma.summary)


#### Plots lambdas
# compare trends in lambdas across treaty scope in large and small

## Start with Large powers
lambda.means.lg <- get_posterior_mean(ml.model.lmbcap, pars = "lambda_lg")
lambda.df.lg <- tibble(lambda = lambda.means.lg[, 5]) %>%  # add lambdas to df
  bind_cols(filter(reg.all.data, atopid %in% colnames(state.mem.lg)))
cor.test(lambda.df.lg$lambda, lambda.df.lg$latent.depth.mean,
         alternative = "less", method = "spearman")


# plot Large powers
lambda.depth.lg.joint <- ggplot(lambda.df.lg, aes(x = latent.depth.mean, y = lambda)) +
 geom_hline(yintercept = 0) +
   geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
  ggtitle("Large Powers")
lambda.depth.lg.joint


# Use random forest to assess variable importance: Large powers
rf.lg <- cforest(lambda ~ ., data = lambda.df.lg)  # fit forest
vi.lg <- varimp(rf.lg)  # calculate variable importance
vi.df.lg <- data_frame(var_name = names(vi.lg), importance = vi.lg) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.df.lg, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Alliance Covariates from a Random Forest Model: Large Powers")
ggsave("figures/varimp-lg.png", height = 6, width = 8)



## Small powers
lambda.means.sm <- get_posterior_mean(ml.model.lmbcap, pars = "lambda_sm")
lambda.df.sm <- tibble(lambda = lambda.means.sm[, 5]) %>%  # add lambdas to df
  bind_cols(filter(reg.all.data, atopid %in% colnames(state.mem.sm)))
cor.test(lambda.df.sm$lambda, lambda.df.sm$latent.depth.mean, 
         alternative = "less", method = "spearman")

# plot Small powers
lambda.depth.sm.joint <- ggplot(lambda.df.sm, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
  ggtitle("Small Powers")
lambda.depth.sm.joint
                                              


# Use random forest to assess variable importance
rf.sm <- cforest(lambda ~ ., data = lambda.df.sm)  # fit forest
vi.sm <- varimp(rf.sm)  # calculate variable importance
vi.df.sm <- data_frame(var_name = names(vi.sm), importance = vi.sm) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.df.sm, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Alliance Covariates from a Random Forest Model: Small Powers")
ggsave("figures/varimp-sm.png", height = 6, width = 8)


# Plot Large and Small trends together
grid.arrange(lambda.depth.lg.joint, lambda.depth.sm.joint)


# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.lg <- apply(ml.model.sum$lambda_lg, 2, positive.check)
lambda.probs.lg <- cbind.data.frame(colnames(state.mem.lg), round(lambda.df.lg$lambda, digits = 4), lambda.df.lg$latent.depth.mean, lambda.probs.lg)
colnames(lambda.probs.lg) <- c("atopid", "lambda.mean", "latent.depth.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.lg$non.zero <- ifelse(lambda.probs.lg$pos.post.prob >= .90 | lambda.probs.lg$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.lg$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.lg$atopid <- reorder(lambda.probs.lg$atopid, lambda.probs.lg$pos.post.prob)

# For all alliances
ggplot(lambda.probs.lg, aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.lg %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
# ggsave("figures/non-zero-alliances-lg.png", height = 6, width = 8)




# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.sm <- apply(ml.model.sum$lambda_sm, 2, positive.check)
lambda.probs.sm <- cbind.data.frame(colnames(state.mem.sm), round(lambda.df.sm$lambda, digits = 4), lambda.df.sm$latent.depth.mean, lambda.probs.sm)
colnames(lambda.probs.sm) <- c("atopid", "lambda.mean", "latent.depth.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.sm$non.zero <- ifelse(lambda.probs.sm$pos.post.prob >= .90 | lambda.probs.sm$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.sm$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.sm$atopid <- reorder(lambda.probs.sm$atopid, lambda.probs.sm$pos.post.prob)

# For all alliances
ggplot(lambda.probs.sm, aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.sm %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
# ggsave("figures/non-zero-alliances-sm.png", height = 6, width = 8)




### 
# Plot posterior densities of variance parameters, expect for capability
sigma.df <- cbind(ml.model.sum$sigma_year, ml.model.sum$sigma_state, ml.model.sum$sigma_all_sm, 
                  ml.model.sum$sigma_all_lg)
colnames(sigma.df) <- c("sigma.year", "sigma.state", "sigma.all.sm", "sigma.all.lg")
sigma.df <- as.data.frame(sigma.df)

ggplot(sigma.df, aes(x = sigma.year)) + geom_density() + ggtitle("Posterior Density of Year Variance Parameter")
ggplot(sigma.df, aes(x = sigma.state)) + geom_density() + ggtitle("Posterior Density of State Variance Parameter")
ggplot(sigma.df, aes(x = sigma.all.sm)) + geom_density() + ggtitle("Posterior Density of Alliance Variance Parameter: Small Powers")
ggplot(sigma.df, aes(x = sigma.all.lg)) + geom_density() + ggtitle("Posterior Density of Alliance Variance Parameter: Large Powers")


# plot all variance parameters together
sigma.df.melt <- melt(sigma.df) 

ggplot(sigma.df.melt, aes(x = value, fill = variable)) + geom_density() +
 scale_fill_brewer(palette = "Greys") +
 ggtitle("Posterior Densities of Variance Hyperparameters") + theme_classic()
ggsave("figures/variance-hyperparam-plot.png", height = 6, width = 8)

rm(list = c("sigma.df", "sigma.df.melt"))

# Calculate the R^2 of the alliance level model: really odd answers w/ variance
# Small 
theta.means.sm <- get_posterior_mean(ml.model.lmbcap, pars = "theta_sm")
1 - (mean(ml.model.sum$sigma_all_sm)^2 / sd(theta.means.sm[, 5]))
# Large 
theta.means.lg <- get_posterior_mean(ml.model.lmbcap, pars = "theta_lg")
1 - (mean(ml.model.sum$sigma_all_lg)^2 / sd(theta.means.lg[, 5]))



# matrix multiplication of membership matrix by mean lambda 
agg.all.lg  <- as.matrix(state.mem.lg)%*%lambda.means.lg[, 5]

summary(agg.all.lg)
agg.all.lg <- cbind.data.frame(reg.state.comp$ccode,
                     reg.state.comp$year, agg.all.lg)
colnames(agg.all.lg) <- c("ccode", "year", "agg.all.impact")

filter(agg.all.lg, agg.all.impact != 0) %>%
ggplot(aes(x = agg.all.impact)) + geom_histogram()

# matrix multiplication of membership matrix by mean lambda 
agg.all.sm  <- as.matrix(state.mem.sm)%*%lambda.means.sm[, 5]

summary(agg.all.sm)
agg.all.sm <- cbind.data.frame(reg.state.comp$ccode,
                     reg.state.comp$year, agg.all.sm)
colnames(agg.all.sm) <- c("ccode", "year", "agg.all.impact")

ggplot(agg.all.sm, aes(x = agg.all.impact)) + geom_histogram()





# Given length of run for varying slopes model: remove from workspace
rm(ml.model.lmbcap)


# summarize session info
devtools::session_info("rstan")

                       
