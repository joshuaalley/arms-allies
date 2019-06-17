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
# the scripts alliance-measures.R and dataset construction and summary.R 


# Define illustrative hypothethetical curves of impact of alliance participation against scope
# Major powers 
eq.maj <- function(x){(2^(-x*2))}

# non-major powers
eq.min <- function(x){-(2^(-x*2))}

# plot curves
illus.plot <- ggplot(data.frame(x = c(0, 2)), aes(x = x))
illus.plot <- illus.plot + stat_function(fun = eq.maj, geom = "line", size = 2)
illus.plot <- illus.plot + stat_function(fun = eq.min, geom = "line", size = 2) +
  xlab("Treaty Scope") + ylab("Impact of Alliance Participation on Mil. Ex.") +
  geom_hline(yintercept = 0, size = 2) + 
  theme_classic(base_size = 18, base_line_size = 1.5) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(label = "Major Power", x = 1, y = 0.5, size = 5) +
  geom_text(label = "Non-Major Power", x = 1, y = -0.5, size = 5) +
  geom_text(label = "0", y = .05, x = -.07, size = 5)
illus.plot
ggsave("figures/illus-arg.png")


# look at distribution of outcome across the two groups: transformed for ease of viewing
ggplot(reg.state.comp, aes(x = asinh(growth.milex), color = as.factor(majpower))) +
  geom_density()


### transform data into matrices for STAN
# State-level characeristics
reg.state.mat <- as.matrix(reg.state.comp[, 4:10])

# check correlations among state-level regressors
cor(reg.state.mat, method = "pearson")

# Get alliances for major and non-major power members
state.mem.min <- as.matrix(state.mem.mat[, colnames(state.mem.mat) %in% colnames(state.mem.min)])
state.mem.maj <- as.matrix(state.mem.mat[, colnames(state.mem.mat) %in% colnames(state.mem.maj)])


# Create the matrix of alliance-level variables for major and non-major power groups
# non-major powers
# Make the alliance characteristics data match the membership matrix
reg.all.data.min <- filter(alliance.char, atopid %in% colnames(state.mem.min)) %>%
  select(latent.scope.mean, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, us.mem, ussr.mem)


# Remove two alliances where members are not in COW system: no kappa 
reg.all.data.min <- reg.all.data.min[complete.cases(reg.all.data.min), ]

# define non-major power alliance matrix
cons <- rep(1, nrow(reg.all.data.min))
alliance.reg.mat.min <- cbind(cons, reg.all.data.min)
alliance.reg.mat.min <- as.matrix(alliance.reg.mat.min)


# Make the alliance characteristics data match the membership matrix
reg.all.data.maj <- filter(alliance.char, atopid %in% colnames(state.mem.maj)) %>%
  select(latent.scope.mean, num.mem, low.kap.sc,
         avg.democ, wartime, asymm, us.mem, ussr.mem)


# Remove two alliances where non-major members are not in COW system: ATOPID 1118 and 1320
reg.all.data.maj <- reg.all.data.maj[complete.cases(reg.all.data.maj), ]

# define major power alliance matrix 
cons <- rep(1, nrow(reg.all.data.maj))
alliance.reg.mat.maj <- cbind(cons, reg.all.data.maj)
alliance.reg.mat.maj <- as.matrix(alliance.reg.mat.maj)


# Create Appropriate id variables in each subsample
maj.id <- filter(reg.state.comp, majpower == 1) %>% select(state.id, year.id)
min.id <- filter(reg.state.comp, majpower == 0) %>% select(state.id, year.id)

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

# posterior predictive check from variational Bayes- did not converge
# so treat these predictions with caution
y = reg.state.comp[, 3]
vb.model.sum <- extract(ml.model.vb)
ppc_dens_overlay(y, vb.model.sum$y_pred[1:100, ])

# Remove vb model and associated summary
rm(list = c("ml.model.vb", "vb.model.sum"))

 
# Regular STAN
system.time(
  ml.model <- sampling(model.1, data = stan.data, 
                       iter = 2000, warmup = 1000, chains = 4,
                       control=list(adapt_delta = 0.9, max_treedepth = 15)
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
# label columns
colnames(ml.model.sum$beta) <- colnames(alliance.reg.mat)

# latent scope
mean(ml.model.sum$beta[, 1, 2] > 0) # non-major
mean(ml.model.sum$beta[, 2, 2] < 0) # major
# number of members
mean(ml.model.sum$beta[, 1, 3] > 0) # non-major
mean(ml.model.sum$beta[, 2, 3] > 0) # major
# FP disagreement 
mean(ml.model.sum$beta[, 1, 4] > 0) # non-major
mean(ml.model.sum$beta[, 2, 4] < 0) # major
# democratic proportion
mean(ml.model.sum$beta[, 1, 5] < 0) # non-major
mean(ml.model.sum$beta[, 2, 5] < 0) # major
# wartime
mean(ml.model.sum$beta[, 1, 6] > 0) # non-major
mean(ml.model.sum$beta[, 2, 6] < 0) # major
# asymmetric obligations 
mean(ml.model.sum$beta[, 1, 7] < 0) # non-major
mean(ml.model.sum$beta[, 2, 7] > 0) # major
# US membership
mean(ml.model.sum$beta[, 1, 8] < 0) # non-major
mean(ml.model.sum$beta[, 2, 8] < 0) # major
# USSR membership
mean(ml.model.sum$beta[, 1, 9] < 0) # non-major
mean(ml.model.sum$beta[, 2, 9] < 0) # major


# this is not going well- need to figure out array indexin
dimnames(ml.model.sum$beta)[[3]] <- c("Constant", "Latent Scope", 
                                      "Number Members", "FP Disagreement",
                                      "Democratic Membership", 
                                      "Wartime", "Asymmetric",
                                      "US Member", "USSR Member")
dimnames(ml.model.sum$beta)[[2]] <- c("Non-Major", "Major")

mcmc_areas(ml.model.sum$beta, 
           prob = .9)

# Summarize intervals
beta.summary <- summary(ml.model, pars = c("beta"), 
                        probs = c(0.05, 0.95))$summary
beta.summary <- beta.summary[, -2]
rownames(beta.summary) <- c("Constant: Non-Major", "Latent Scope: Non-Major", 
                            "Number Members: Non-Major", "FP Disagreement: Major",
                            "Democratic Membership: Non-Major", 
                            "Wartime: Non-Major", "Asymmetric: Non-Major",
                            "US Member: Non-Major", "USSR Member: Non-Major", "Constant: Major", "Latent Scope: Major", 
                            "Number Members: Major", "FP Disagreement: Major",
                            "Democratic Membership: Major", 
                            "Wartime: Major", "Asymmetric: Major",
                            "US Member: Major", "USSR Member: Major")

print(beta.summary)
xtable(beta.summary, digits = 3)


# Create plot of the scope parameters
scope.dens.joint <- cbind(ml.model.sum$beta[, 2, 2], ml.model.sum$beta[, 1, 2])
colnames(scope.dens.joint) <- c("Major", "Non-Major")
scope.dens.joint <- melt(scope.dens.joint)

ggplot(scope.dens.joint, aes(x = value,  fill = X2)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(name = "Sample", values=c("#999999", "#000000")) +
  ggtitle("Posterior Distributions of Treaty Scope: Major and Non-Major Powers") +
  theme_classic()
ggsave("figures/scope-dens-joint.png", height = 6, width = 8)


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
ggplot(gamma.melt, aes(x=value,  fill = Var.2)) +
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


# Plot posterior probabilities that a given coefficient is positive
positive.check <- function(x){
  mean(x > 0)
}

beta.probs.min <- apply(ml.model.sum$beta[, 1, ], 2, positive.check)
beta.probs.maj <- apply(ml.model.sum$beta[, 2, ], 2, positive.check)
gamma.probs <- apply(ml.model.sum$gamma, 2, positive.check)

# append in a dataframe to be used for plotting
coef.probs <- as.data.frame(append(beta.probs, gamma.probs))
colnames(coef.probs) <- c("Posterior Probability of Positive Coefficient")
rownames(coef.probs) <- c("Alliance Model Constant", "Latent Scope", 
                            "Number Members","Democratic Membership", 
                          "Wartime", "Asymmetric",
                          "US Member", "USSR Member",
                          "At War", "Civil War", "Rival Mil. Expenditure", 
                          "ln(GDP)", "Polity", "Cold War", "Disputes", "Major Power")
coef.probs$variable <- rownames(coef.probs)
coef.probs$variable <- reorder(coef.probs$variable, coef.probs$`Posterior Probability of Positive Coefficient`)
  
# Plot
ggplot(coef.probs, aes(x = variable, y = `Posterior Probability of Positive Coefficient`)) + 
  geom_col() +
  geom_text(aes(label = `Posterior Probability of Positive Coefficient`), nudge_y = .0675) +
  coord_flip()
ggsave("figures/post-prob.png", height = 6, width = 8)







#### Plots lambdas
# compare trends in lambdas across treaty scope in major and minor

## Start with major powers
lambda.means.maj <- get_posterior_mean(ml.model, pars = "lambda_maj")
lambda.df.maj <- data_frame(lambda = lambda.means.maj[, 5]) %>%  # add lambdas to df
  bind_cols(filter(reg.all.data, atopid %in% colnames(state.mem.maj)))
cor.test(lambda.df.maj$lambda, lambda.df.maj$latent.scope.mean,
         alternative = "less", method = "spearman")


# plot major powers
lambda.scope.maj <- ggplot(lambda.df.maj, aes(x = latent.scope.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Scope", y = "Effect of Allied Spending") +
  ggtitle("Major Powers")
lambda.scope.maj


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
cor.test(lambda.df.min$lambda, lambda.df.min$latent.scope.mean, 
         alternative = "greater", method = "spearman")

# plot non-major powers
lambda.scope.min <- ggplot(lambda.df.min, aes(x = latent.scope.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Scope", y = "Effect of Allied Spending") +
  ggtitle("Non-Major Powers")
lambda.scope.min
                                              


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




# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.maj <- apply(ml.model.sum$lambda_maj, 2, positive.check)
lambda.probs.maj <- cbind.data.frame(colnames(state.mem.maj), round(lambda.df.maj$lambda, digits = 4), lambda.df.maj$latent.scope.mean, lambda.probs.maj)
colnames(lambda.probs.maj) <- c("atopid", "lambda.mean", "latent.scope.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.maj$non.zero <- ifelse(lambda.probs.maj$pos.post.prob >= .90 | lambda.probs.maj$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.maj$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.maj$atopid <- reorder(lambda.probs.maj$atopid, lambda.probs.maj$pos.post.prob)

# For all alliances
ggplot(lambda.probs.maj, aes(x = atopid, y = pos.post.prob, fill = latent.scope.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.maj %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.scope.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
ggsave("figures/non-zero-alliances-min.png", height = 6, width = 8)





# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.min <- apply(ml.model.sum$lambda_min, 2, positive.check)
lambda.probs.min <- cbind.data.frame(colnames(state.mem.min), round(lambda.df.min$lambda, digits = 4), lambda.df.min$latent.scope.mean, lambda.probs.min)
colnames(lambda.probs.min) <- c("atopid", "lambda.mean", "latent.scope.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.min$non.zero <- ifelse(lambda.probs.min$pos.post.prob >= .90 | lambda.probs.min$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.min$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.min$atopid <- reorder(lambda.probs.min$atopid, lambda.probs.min$pos.post.prob)

# For all alliances
ggplot(lambda.probs.min, aes(x = atopid, y = pos.post.prob, fill = latent.scope.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.min %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, fill = latent.scope.mean)) + 
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





# Remove stan object from workspace- save separately to conserve memory
saveRDS(ml.model, "ml.model.rds")
rm(ml.model)



# summarize session info
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))

devtools::session_info("rstan")

                       