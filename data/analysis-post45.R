# Joshua Alley
# Analysis of post-45 military spending with Nordhaus et al data from DG and Poast 2016 
# Checks reliance on COW data with rebased spending data, mostly from SIPRI 



# create a state index variable
post45.min$state.id <- post45.min %>% group_indices(ccode)
# Create a year index variable 
post45.min$year.id <- post45.min %>% group_indices(year)


post45.min <- as.data.frame(post45.min)
stan.data.post45 <- list(N = nrow(post45.min), y = post45.min[, 3],
                      state = post45.min$state.id, S = length(unique(post45.min$state.id)),
                      year = post45.min$year.id, T = length(unique(post45.min$year.id)),
                      A = ncol(post45.mem.min), L = ncol(post45.all.mat.min),
                      Z = post45.mem.min, 
                      X = post45.all.mat.min,
                      W = post45.reg.mat, M = ncol(post45.reg.mat)
)

# Compile the model code: changes w/o the transformation
# model.3 <- stan_model(file = "data/ml-model-noasinh.stan")

# Regular STAN
system.time(
  ml.model.post45 <- sampling(model.2, data = stan.data.post45, 
                           iter = 2500, warmup = 1000, chains = 4, 
                           control = list(max_treedepth = 15)
  )
)


# diagnose model
# launch_shinystan(ml.model.post45)
check_hmc_diagnostics(ml.model.post45)


# Check for correlation between the means and variances of the state intercepts
pairs(ml.model.post45, pars = c("alpha", "sigma_state", "alpha_state[1]", "alpha_state[2]", "alpha_state[15]", 
                         "alpha_state[26]"))


# Summarize the posterior intervals on alliance-level regressors
# summarize intervals for minor powers
beta.summary.post45 <- summary(ml.model.post45, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.post45 <- beta.summary.post45[, -2]
rownames(beta.summary.post45) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                                "FP Conc.",
                                "Number Members", "FP Similarity",
                                "Democratic Membership", 
                                "Wartime", "Asymmetric", "US. Mem", "USSR Mem.",
                                "sigma Alliances")

print(beta.summary.post45)




# state-level regression parameters
gamma.summary.post45 <- summary(ml.model.post45, pars = c("gamma"), probs = c(0.05, 0.95))$summary
print(gamma.summary.post45)

# Calculate posterior probabilities 
coef.post45 <- extract(ml.model.post45, pars = c("beta", "gamma"))
colnames(coef.post45$beta) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                             "FP Conc.",
                             "Number Members", "FP Similarity",
                             "Democratic Membership", 
                             "Wartime", "Asymmetric", "US. Mem", "USSR Mem.")
colnames(coef.post45$gamma) <- colnames(post45.reg.mat)

# all for non-major powers only 
mean(coef.post45$beta[, 2] < 0) # depth
mean(coef.post45$beta[, 3] < 0) # unconditional support
mean(coef.post45$beta[, 4] < 0)  # econ link
mean(coef.post45$beta[, 5] > 0) # FP concessions


# Summary for the appendix
color_scheme_set("darkgray")
mcmc_intervals(coef.post45$beta, 
               prob = .9) +
  theme_classic()
ggsave("appendix/post45-beta-res.png", height = 6, width = 8)



# Save model and take it out of the workspace (1.7 GB w/ likelihood and PPC)
saveRDS(ml.model.post45, "data/ml-model-post45.rds")
rm(ml.model.post45)
