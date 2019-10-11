# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions


# Environment is determined by use of projects and/or running this file in conjunction with
# the scripts alliance-measures.R and dataset construction and summary.R 



# look at distribution of outcome across the two groups: transformed for ease of viewing
ggplot(reg.state.comp, aes(x = asinh(growth.milex), color = as.factor(majpower))) +
  geom_density()


# run model on the full sample

# Define the data list 
reg.state.comp <- as.data.frame(reg.state.comp)
stan.data <- list(y_min = reg.state.comp.min$growth.milex, y_maj = reg.state.comp.maj$growth.milex,
                  state_min = min.id$state.id, state_maj = maj.id$state.id, S = length(unique(reg.state.comp$state.id)),
                  year_min = min.id$year.id, year_maj = maj.id$year.id, T = length(unique(reg.state.comp$year.id)),
                  cap = reg.state.comp$mp.id, J = length(unique(reg.state.comp$mp.id)),
                  A_min = ncol(state.mem.min), Z_min = state.mem.min,  # non-major membership
                  A_maj = ncol(state.mem.maj), Z_maj = state.mem.maj,  # major membership 
                  X_min = alliance.reg.mat.min, X_maj = alliance.reg.mat.maj, 
                  L = ncol(alliance.reg.mat.min),
                  N_min = nrow(state.mem.min), N_maj = nrow(state.mem.maj),
                  W_min = reg.state.mat.min, W_maj = reg.state.mat.maj, M = ncol(reg.state.mat.min)
)


# Compile the model code
model.1 <- stan_model(file = "data/multi-member ML model.stan")

# Variational Bayes- use to check model will run 
ml.model.vb <- vb(model.1, data = stan.data, seed = 12)

# Remove vb model and associated summary
rm(list = c("ml.model.vb", "vb.model.sum"))

 
# Regular STAN: takes about 2 days. 
system.time(
  ml.model <- sampling(model.1, data = stan.data, 
                       iter = 2000, warmup = 1000, chains = 4,
                       control=list(adapt_delta = 0.95, max_treedepth = 15)
  )
)

# diagnose full model
# only run shinystan line from console- can't run whole script otherwise
# launch_shinystan(ml.model)

check_hmc_diagnostics(ml.model)

# Traceplots for the regression parameters
traceplot(ml.model, pars = "beta")
traceplot(ml.model, pars = "gamma")
traceplot(ml.model, pars = "L_Omega_beta")
traceplot(ml.model, pars = "tau_beta")

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



# Extract coefficients from the model
ml.model.sum <- extract(ml.model, pars = c("beta", "gamma", "lambda_min", "lambda_maj",
                                           "sigma", "sigma_year", "sigma_state", "sigma_cap",
                                           "sigma_all_min", "sigma_all_maj"),
                        permuted = TRUE)

# Posterior predictive distributions relative to observed data
yrep.full <- ml.model.sum$y_pred[1:100, ]
ml.model.sum <- ml.model.sum[1:7]

# plot posterior predictive denisty of first 100 simulations
ppc_dens_overlay(y, yrep.full)


# Plot all the beta coefficients and calculate posterior probabilities

# depth index
mean(ml.model.sum$beta[, 1, 2] < 0) # non-major
mean(ml.model.sum$beta[, 2, 2] < 0) # major
# unconditional military support
mean(ml.model.sum$beta[, 1, 3] < 0) # non-major
mean(ml.model.sum$beta[, 2, 3] < 0) # major
# econonmic agreement
mean(ml.model.sum$beta[, 1, 4] > 0) # non-major
mean(ml.model.sum$beta[, 2, 4] < 0) # major
# FP concessions
mean(ml.model.sum$beta[, 1, 5] > 0) # non-major
mean(ml.model.sum$beta[, 2, 5] < 0) # major
# number of members
mean(ml.model.sum$beta[, 1, 6] < 0) # non-major
mean(ml.model.sum$beta[, 2, 6] < 0) # major
# FP similarity
mean(ml.model.sum$beta[, 1, 7] > 0) # non-major
mean(ml.model.sum$beta[, 2, 7] < 0) # major
# avg democracy
mean(ml.model.sum$beta[, 1, 8] < 0) # non-major
mean(ml.model.sum$beta[, 2, 8] > 0) # major
# wartime
mean(ml.model.sum$beta[, 1, 9] > 0) # non-major
mean(ml.model.sum$beta[, 2, 9] < 0) # major
# asymmetric 
mean(ml.model.sum$beta[, 1, 10] < 0) # non-major
mean(ml.model.sum$beta[, 2, 10] < 0) # major


# Figure out way to combine below intervals 
dimnames(ml.model.sum$beta)[[3]] <- c("Constant", "Depth", "Uncond. Milsup.", "Econ. Link", 
                                      "FP Conc.",
                                      "Number Members", "FP Similarity",
                                      "Democratic Membership", 
                                      "Wartime", "Asymmetric", "US. Mem", "USSR Mem.")
dimnames(ml.model.sum$beta)[[2]] <- c("Non-Major", "Major")

mcmc_intervals(ml.model.sum$beta[, 1, ], 
               prob = .9)
mcmc_intervals(ml.model.sum$beta[, 2, ], 
           prob = .9)

# Summarize intervals
beta.summary <- summary(ml.model, pars = c("beta"), 
                        probs = c(0.05, 0.95))$summary
beta.summary <- beta.summary[, -2]
rownames(beta.summary) <- c("Constant: Non-Major", "Latent Scope: Non-Major", 
                            "Number Members: Non-Major", "FP Similarity: Non-Major",
                            "Democratic Membership: Non-Major", 
                            "Wartime: Non-Major", "Asymmetric: Non-Major",
                            "US Member: Non-Major", "USSR Member: Non-Major", "Constant: Major", "Latent Scope: Major", 
                            "Number Members: Major", "FP Similarity: Major",
                            "Democratic Membership: Major", 
                            "Wartime: Major", "Asymmetric: Major",
                            "US Member: Major", "USSR Member: Major")

print(beta.summary)
xtable(beta.summary, digits = 3)


# Create plot of the scope parameters
depth.dens.joint <- cbind(ml.model.sum$beta[, 2, 2], ml.model.sum$beta[, 1, 2])
colnames(depth.dens.joint) <- c("Major", "Non-Major")
depth.dens.joint <- melt(depth.dens.joint)

ggplot(depth.dens.joint, aes(x = value,  fill = Var2)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(name = "Sample", values=c("#999999", "#000000")) +
  ggtitle("Posterior Distributions of Treaty Scope: Major and Non-Major Powers") +
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
gamma.summary <- summary(ml.model, pars = c("gamma", "sigma_state", "alpha_cap[1]", "alpha_cap[2]"),
                         probs = c(0.05, 0.95))$summary
gamma.summary <- gamma.summary[, -2]
rownames(gamma.summary) <- c("Wartime", "Civil War", "Rival Mil. Expenditure", 
                            "ln(GDP)", "Polity", "Cold War", "Disputes",
                            "Sigma State", "Intercept: Non-Major", "Intercept: Major")
print(gamma.summary)
xtable(gamma.summary)


#### Plots lambdas
# compare trends in lambdas across treaty scope in major and minor

## Start with major powers
lambda.means.maj <- get_posterior_mean(ml.model, pars = "lambda_maj")
lambda.df.maj <- tibble(lambda = lambda.means.maj[, 5]) %>%  # add lambdas to df
  bind_cols(filter(reg.all.data, atopid %in% colnames(state.mem.maj)))
cor.test(lambda.df.maj$lambda, lambda.df.maj$latent.depth.mean,
         alternative = "less", method = "spearman")


# plot major powers
lambda.depth.maj.joint <- ggplot(lambda.df.maj, aes(x = latent.depth.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
  ggtitle("Major Powers")
lambda.depth.maj.joint


# Use random forest to assess variable importance: major powers
rf.maj <- cforest(lambda ~ ., data = lambda.df.maj)  # fit forest
vi.maj <- varimp(rf.maj)  # calculate variable importance
vi.df.maj <- data_frame(var_name = names(vi.maj), importance = vi.maj) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.df.maj, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Alliance Covariates from a Random Forest Model: Major Powers")
ggsave("figures/varimp-maj.png", height = 6, width = 8)



## Non-major powers
lambda.means.min <- get_posterior_mean(ml.model, pars = "lambda_min")
lambda.df.min <- data_frame(lambda = lambda.means.min[, 5]) %>%  # add lambdas to df
  bind_cols(filter(reg.all.data, atopid %in% colnames(state.mem.min)))
cor.test(lambda.df.min$lambda, lambda.df.min$latent.depth.mean, 
         alternative = "less", method = "spearman")

# plot non-major powers
lambda.depth.min.joint <- ggplot(lambda.df.min, aes(x = latent.depth.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Scope", y = "Effect of Allied Spending") +
  ggtitle("Non-Major Powers")
lambda.depth.min.joint
                                              


# Use random forest to assess variable importance
rf.min <- cforest(lambda ~ ., data = lambda.df.min)  # fit forest
vi.min <- varimp(rf.min)  # calculate variable importance
vi.df.min <- data_frame(var_name = names(vi.min), importance = vi.min) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.df.min, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Alliance Covariates from a Random Forest Model: Non-Major Powers")
ggsave("figures/varimp-min.png", height = 6, width = 8)


# Plot major and non-major trends together
multiplot.ggplot(lambda.depth.maj.joint, lambda.depth.min.joint)


# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.maj <- apply(ml.model.sum$lambda_maj, 2, positive.check)
lambda.probs.maj <- cbind.data.frame(colnames(state.mem.maj), round(lambda.df.maj$lambda, digits = 4), lambda.df.maj$latent.depth.mean, lambda.probs.maj)
colnames(lambda.probs.maj) <- c("atopid", "lambda.mean", "latent.depth.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.maj$non.zero <- ifelse(lambda.probs.maj$pos.post.prob >= .90 | lambda.probs.maj$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.maj$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.maj$atopid <- reorder(lambda.probs.maj$atopid, lambda.probs.maj$pos.post.prob)

# For all alliances
ggplot(lambda.probs.maj, aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.maj %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
ggsave("figures/non-zero-alliances-maj.png", height = 6, width = 8)




# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.min <- apply(ml.model.sum$lambda_min, 2, positive.check)
lambda.probs.min <- cbind.data.frame(colnames(state.mem.min), round(lambda.df.min$lambda, digits = 4), lambda.df.min$latent.depth.mean, lambda.probs.min)
colnames(lambda.probs.min) <- c("atopid", "lambda.mean", "latent.depth.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.min$non.zero <- ifelse(lambda.probs.min$pos.post.prob >= .90 | lambda.probs.min$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.min$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.min$atopid <- reorder(lambda.probs.min$atopid, lambda.probs.min$pos.post.prob)

# For all alliances
ggplot(lambda.probs.min, aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.min %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
ggsave("figures/non-zero-alliances-min.png", height = 6, width = 8)




### 
# Plot posterior densities of variance parameters, expect for capability
sigma.df <- cbind(ml.model.sum$sigma_year, ml.model.sum$sigma_state, ml.model.sum$sigma_all_min, 
                  ml.model.sum$sigma_all_maj)
colnames(sigma.df) <- c("sigma.year", "sigma.state", "sigma.all.min", "sigma.all.maj")
sigma.df <- as.data.frame(sigma.df)

ggplot(sigma.df, aes(x = sigma.year)) + geom_density() + ggtitle("Posterior Density of Year Variance Parameter")
ggplot(sigma.df, aes(x = sigma.state)) + geom_density() + ggtitle("Posterior Density of State Variance Parameter")
ggplot(sigma.df, aes(x = sigma.all.min)) + geom_density() + ggtitle("Posterior Density of Alliance Variance Parameter: Non-Major Powers")
ggplot(sigma.df, aes(x = sigma.all.maj)) + geom_density() + ggtitle("Posterior Density of Alliance Variance Parameter: Major Powers")


# plot all variance parameters together
sigma.df.melt <- melt(sigma.df) 

ggplot(sigma.df.melt, aes(x = value, fill = variable)) + geom_density() +
 scale_fill_brewer(palette = "Greys") +
 ggtitle("Posterior Densities of Variance Hyperparameters") + theme_classic()
ggsave("figures/variance-hyperparam-plot.png", height = 6, width = 8)

rm(list = c("sigma.df", "sigma.df.melt"))

# Calculate the R^2 of the alliance level model: really odd answers
# non-major 
theta.means.min <- get_posterior_mean(ml.model, pars = "theta_min")
1 - (mean(ml.model.sum$sigma_all_min)^2 / var(theta.means.min[, 5]))
# Major 
theta.means.maj <- get_posterior_mean(ml.model, pars = "theta_maj")
1 - (mean(ml.model.sum$sigma_all_maj)^2 / var(theta.means.maj[, 5]))



# matrix multiplication of membership matrix by mean lambda 
agg.all.maj  <- state.mem.maj%*%lambda.means.maj[, 5]

summary(agg.all.maj)
agg.all.maj <- cbind.data.frame(reg.state.comp.maj$ccode,
                     reg.state.comp.maj$year, agg.all.maj)
colnames(agg.all.maj) <- c("ccode", "year", "agg.all.impact")

ggplot(agg.all.maj, aes(x = agg.all.impact)) + geom_histogram()

# matrix multiplication of membership matrix by mean lambda 
agg.all.min  <- state.mem.min%*%lambda.means.min[, 5]

summary(agg.all.min)
agg.all.min <- cbind.data.frame(reg.state.comp.min$ccode,
                     reg.state.comp.min$year, agg.all.min)
colnames(agg.all.min) <- c("ccode", "year", "agg.all.impact")

ggplot(agg.all.min, aes(x = agg.all.impact)) + geom_histogram()





# Given length of run for varying slopes model- save separately
saveRDS(ml.model, "data/ml.model.rds")
ml.model <- readRDS("data/ml.model.rds")


# summarize session info
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))

devtools::session_info("rstan")

                       
