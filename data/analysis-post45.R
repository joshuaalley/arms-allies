# Joshua Alley
# Analysis of post-45 military spending with Nordhaus et al data from DG and Poast 2016 
# Checks reliance on COW data with rebased spending data, mostly from SIPRI 


# Define the data list 
post45.comp <- as.data.frame(post45.comp)
stan.data.post45 <- list(N = nrow(post45.comp), y = post45.comp[, 3],
                         state = post45.comp$state.id, S = length(unique(post45.comp$state.id)),
                         year = post45.comp$year.id, T = length(unique(post45.comp$year.id)),
                         A_sm = ncol(post45.mem.sm), A_lg = ncol(post45.mem.lg),
                         L = ncol(post45.all.mat.sm),
                         Z_sm = post45.mem.sm, 
                         Z_lg = post45.mem.lg,
                         X_sm = post45.all.mat.sm,
                         X_lg = post45.all.mat.lg,
                         J = 2,
                         W = post45.reg.mat, M = ncol(post45.reg.mat)
)



# Compile the model code: changes w/ transformation
# no varying slopes

# quick variational bayes check
vb.lmbcap.post45 <- vb(model.lmbcap.sim, 
                    data = stan.data.post45, seed = 12,
                    iter = 10000)
rm(vb.lmbcap.post45)

# Regular STAN
system.time(
  ml.model.post45 <- sampling(model.lmbcap.sim, data = stan.data.post45, 
                           iter = 2000, warmup = 1000, chains = 4, 
                           control = list(max_treedepth = 15),
                           pars = c("beta_sm", "beta_lg",
                                    "lambda_sm", "lambda_lg", "gamma",
                                    "nu", "sigma", "sigma_state", "sigma_year",
                                    "sigma_all_lg", "sigma_all_sm"),
                           include = TRUE
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
