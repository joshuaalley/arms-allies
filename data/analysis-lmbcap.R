# Joshua Alley
# Fit a model with alliance part effects that vary by 
# whether a state is the largest or smallest alliance member


# Pull together input data
# Create a matrix of large power membership in alliances (Z in STAN model)
state.mem.full <- as.matrix(reg.state.comp[, 12: ncol(reg.state.comp)])


# Make the alliance characteristics data match the membership matrix
reg.all.data <- filter(alliance.char, atopid %in% colnames(state.mem.full)) %>%
  select(atopid, latent.depth.mean, uncond.milsup, econagg.dum, 
         fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, mean.threat,
         us.mem, ussr.mem) %>%
  na.omit()

state.mem.full <- state.mem.full[, colnames(state.mem.full) %in% 
                                   reg.all.data$atopid]

# get cap year data down to the same
cap.year.est <- left_join(select(reg.state.comp, ccode, year),
                          cap.year)
cap.year.est <- cap.year.est[, colnames(cap.year.est) %in% colnames(state.mem.full)]

# get state-mem marix for large
state.mem.lg = state.mem.full*cap.year.est
# state-mem matrix for small states
sum(cap.year.est) # total large 1s
cap.year.sm <- mutate_all(cap.year.est,  ~ifelse(. == 1, 0, 1))
sum(cap.year.sm) # total small 1s (includes 0s) 
nrow(cap.year.sm)*ncol(cap.year.sm) - sum(cap.year.est) # difference equal to above: checks
state.mem.sm = state.mem.full*cap.year.sm

# remove 0 cols (no small members or missing data on allies)
state.mem.sm <-  state.mem.sm[, colSums(state.mem.sm) != 0]
state.mem.lg <-  state.mem.lg[, colSums(state.mem.lg) != 0]



# finalize alliance-level matrix
reg.all.data.sm <- filter(reg.all.data, atopid %in% colnames(state.mem.sm))
reg.all.data.sm <- select(reg.all.data.sm, -c(atopid))
cons <- rep(1, nrow(reg.all.data.sm))
alliance.reg.mat.sm <- as.matrix(cbind(cons, reg.all.data.sm))


reg.all.data.lg <- filter(reg.all.data, atopid %in% colnames(state.mem.lg))
reg.all.data.lg <- select(reg.all.data.lg, -c(atopid))
cons <- rep(1, nrow(reg.all.data.lg))
alliance.reg.mat.lg <- as.matrix(cbind(cons, reg.all.data.lg))




# Define the data list 
reg.state.comp <- as.data.frame(reg.state.comp)
stan.data.lmbcap <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                      state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                      year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                      A_sm = ncol(state.mem.sm), A_lg = ncol(state.mem.lg),
                      L = ncol(alliance.reg.mat.sm),
                      Z_sm = state.mem.sm, 
                      Z_lg = state.mem.lg,
                      X_sm = alliance.reg.mat.sm,
                      X_lg = alliance.reg.mat.lg,
                      J = 2,
                      W = reg.state.mat, M = ncol(reg.state.mat)
)


# model with separate beta distributions in alliance-level reg
model.lmbcap.sim <- stan_model(file = "data/ml-model-lmbcap-sim.stan")


# Variational Bayes- use to check model will run 
vb.lmbcap.sim <- vb(model.lmbcap.sim, 
                         data = stan.data.lmbcap, seed = 12,
                         iter = 10000)
# launch_shinystan(vb.lmbcap.sim) # check output here if interested
# Remove vb model 
rm(vb.lmbcap.sim)

# Regular STAN: takes ~12 hours on laptop
system.time(
  ml.model.lmbcap.sim <- sampling(model.lmbcap.sim, 
                          data = stan.data.lmbcap,
                          iter = 3000, warmup = 1500, chains = 4,
                          control=list(max_treedepth = 15),
                          pars = c("beta_sm", "beta_lg", "lambda_sm", "lambda_lg", "gamma",
                            "alpha", "sigma", "sigma_state", "sigma_year",
                            "sigma_all_lg", "sigma_all_sm",
                            "y_rep"),
                          include = TRUE
  )
)
# save output 
saveRDS(ml.model.lmbcap.sim, "data/est-lmbcap-sim.rds")


# diagnose model
check_hmc_diagnostics(ml.model.lmbcap.sim)


# posterior predictive check
yrep <- extract(ml.model.lmbcap.sim, pars = "y_rep")
yrep <- yrep$y_rep[1:100, ] # take first 100
ppc_dens_overlay(asinh(stan.data.lmbcap$y), yrep)
# some crazy outlier predictions w/ , but best fit overall
ppc_dens_overlay(asinh(stan.data.lmbcap$y), yrep) +
  xlim(-2, 2)


# trace plot for appendix
traceplot(ml.model.lmbcap.sim, pars = "beta_sm")
ggsave("appendix/trace-all-sm.png", height = 6, width = 8)



# pull parameter estimates
# Summarize intervals for large powers
beta.summary.lg <- summary(ml.model.lmbcap.sim, pars = c("beta_lg", "sigma_all_lg"), probs = c(0.05, 0.95))$summary
beta.summary.lg <- beta.summary.lg[, -2] # remove s.e. of mean column
rownames(beta.summary.lg) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                                "FP Conc.",
                                "Number Members", "FP Similarity",
                                "Democratic Membership", 
                                "Wartime", "Asymmetric Ob.", "Mean Threat",
                                "US Mem.", "USSR Mem.",
                                "sigma Alliances")


print(beta.summary.lg)
xtable(beta.summary.lg, digits = 3)


# summarize intervals for small members
beta.summary.sm <- summary(ml.model.lmbcap.sim, pars = c("beta_sm", "sigma_all_sm"), probs = c(0.05, 0.95))$summary
beta.summary.sm <- beta.summary.sm[, -2]
rownames(beta.summary.sm) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                               "FP Conc.",
                               "Number Members", "FP Similarity",
                               "Democratic Membership", 
                               "Wartime", "Asymmetric Ob.", "Mean Threat",
                               "US Mem.", "USSR Mem.",
                               "sigma Alliances")
print(beta.summary.sm)
xtable(beta.summary.sm, digits = 3) # for appendix




# Summarize the alliance coefficient estimates in large and minor subsets
coef <- extract(ml.model.lmbcap.sim,                         
                pars = c("beta_sm", "beta_lg", 
                      "gamma",
                      "lambda_sm", "lambda_lg"),
                permuted = TRUE)
alliance.pars <-  c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                    "FP Conc.",
                    "Number Members", "FP Similarity",
                    "Democratic Membership", 
                    "Wartime", "Asymmetric Obligations",
                    "Mean Threat",
                    "US Mem.", "USSR Mem.") # vector of labels
colnames(coef$gamma) <- colnames(reg.state.mat)
colnames(coef$beta_sm) <- alliance.pars
colnames(coef$beta_lg) <- alliance.pars


# Graphical summary of minor power alliance-level regression 
color_scheme_set("darkgray")
mcmc_intervals(coef$beta_sm, 
               prob = .9) +
  labs(x = "Effect on Impact of Total Allied Military Spending") +
  ggtitle("Small Member Alliance-Level Regression Coefficients")

mcmc_intervals(coef$beta_lg, 
               prob = .9) +
  labs(x = "Effect on Impact of Total Allied Military Spending") +
  ggtitle("Large Member Alliance-Level Regression Coefficients")


# Baseline posterior probabilities
mean(coef$beta_lg[, 2] < 0) # depth: large
mean(coef$beta_sm[, 2] < 0) # depth: small
mean(coef$beta_lg[, 3] < 0) # uncond milsup: large
mean(coef$beta_sm[, 3] < 0) # uncond milsup: small
mean(coef$beta_lg[, 4] > 0) # econ link: large
mean(coef$beta_sm[, 4] < 0) # econ link: small
mean(coef$beta_lg[, 5] < 0) # FP concessions: large
mean(coef$beta_sm[, 5] > 0) # FP concessions: small
mean(coef$beta_lg[, 6] > 0) # number members: large
mean(coef$beta_sm[, 6] > 0) # number members: small
mean(coef$beta_lg[, 7] < 0) # FP similarity: large
mean(coef$beta_sm[, 7] > 0) # FP similarity: small
mean(coef$beta_lg[, 8] < 0) # Dem. Membership: large
mean(coef$beta_sm[, 8] < 0) # Dem. Membership: small
mean(coef$beta_lg[, 9] < 0) # wartime: large
mean(coef$beta_sm[, 9] > 0) # wartime: small
mean(coef$beta_lg[, 10] < 0) # asymmetric ob: large
mean(coef$beta_sm[, 10] > 0) # asymmetric ob: small
mean(coef$beta_lg[, 11] > 0) # threat: large
mean(coef$beta_sm[, 11] > 0) # threat: small


# compare trends in lambdas across treaty depth in major and minor

# Start with large powers
lambda.means.lg <- get_posterior_mean(ml.model.lmbcap.sim, pars = "lambda_lg")
lambda.df.lg <- tibble(lambda = lambda.means.lg[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.lg)))
cor.test(lambda.df.lg$lambda, lambda.df.lg$latent.depth.mean,
         alternative = "less", method = "spearman")

# plot large powers
lambda.depth.lg <- ggplot(lambda.df.lg, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", colour = "black") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
  ggtitle("Large Alliance Members")
lambda.depth.lg



# small powers
lambda.means.sm <- get_posterior_mean(ml.model.lmbcap.sim, pars = "lambda_sm")
lambda.df.sm <- tibble(lambda = lambda.means.sm[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.sm)))
cor.test(lambda.df.sm$lambda, lambda.df.sm$latent.depth.mean, 
         alternative = "less", method = "spearman")

# plot small powers
lambda.depth.sm <- ggplot(lambda.df.sm, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", colour = "black") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Allied Capability Coefficent") +
  ggtitle("Small Alliance Members")
lambda.depth.sm

# Compare all in one plot
grid.arrange(lambda.depth.lg, lambda.depth.sm,
             nrow = 2)




### Calculate aggregate impact of alliance participation on growth in spending


agg.imp.calc <- function(membership, lambda.mean,
                         lambda.post, size){
# matrix multiplication of membership matrix by mean lambda 
agg.all <- as.matrix(membership)%*%lambda.mean[, 5]

summary(agg.all)
agg.all <- cbind(reg.state.comp$ccode,
                     reg.state.comp$year, agg.all)


# Predicted military spending change for all individual alliances
a <- ncol(membership)
growth.pred <- rep(NA, a)
growth.pred <- list()

# Loop over matrix columns
for(i in 1:a){
  growth.pred[[i]] <- membership[, i][membership[, i] != 0] # filter out zeros 
  growth.pred[[i]] <- growth.pred[[i]]%*%t(lambda.post[, i]) # multiply by lambda
  growth.pred[[i]] <- as.data.frame(growth.pred[[i]])
}

names(growth.pred) <- c(colnames(membership)) # label each matrix with ATOPID


# Capture means and add a label variable 
growth.pred.mean <- lapply(growth.pred, function(x) apply(x, 1, mean))
for(i in 1:a){
  growth.pred.mean[[i]] <- as.data.frame(growth.pred.mean[[i]])
  growth.pred.mean[[i]]$atopid <- colnames(membership)[[i]]
}

growth.pred.sd <- lapply(growth.pred, function(x) as.data.frame(apply(x, 1, sd)))
for(i in 1:a){
  growth.pred.sd[[i]] <- as.data.frame(growth.pred.sd[[i]])
}

# combine means and sds in a dataframe 
growth.pred.res <- cbind(do.call(rbind, growth.pred.mean), unlist(growth.pred.sd))
colnames(growth.pred.res) <- c("mean.pred", "atopid", "sd.pred")
growth.pred.res$mean.pred <- sinh(growth.pred.res$mean.pred) # reverse IHS transformation
growth.pred.res$sd.pred <- sinh(growth.pred.res$sd.pred) # reverse IHS transformation
growth.pred.res$atopid <- as.numeric(growth.pred.res$atopid)

growth.pred.res <- left_join(growth.pred.res, alliance.char)

# Add alliance characteristics
assign(paste0("growth.pred.res.", size), 
       growth.pred.res,
       envir = .GlobalEnv) # place in global environment

}

# aggregate impact small states
agg.imp.calc(membership = state.mem.sm, lambda.mean = lambda.means.sm,
            lambda.post = coef$lambda_sm, size = "sm")

# Address overplotting problem w/ hexagons
growth.depth.sm <- ggplot(growth.pred.res.sm, aes(x = latent.depth.mean, y = mean.pred)) +
  geom_hline(yintercept = 0) +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("#999999","#333333"), 
                       name = "Frequency", 
                       na.value=NA) +
  labs(x = "Latent Treaty Depth",
       y = "Mean Estimated Spending Growth from Alliance") +
  ggtitle("Estimated Military Spending Growth and Treaty Depth") +
  theme_bw() 
growth.depth.sm


# aggregate impact:large states
agg.imp.calc(membership = state.mem.lg, lambda.mean = lambda.means.lg,
             lambda.post = coef$lambda_lg, size = "lg")

# Address overplotting problem w/ hexagons
growth.depth.lg <- ggplot(growth.pred.res.lg, aes(x = latent.depth.mean, y = mean.pred)) +
  geom_hline(yintercept = 0) +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("#999999","#333333"), 
                       name = "Frequency", 
                       na.value=NA) +
  labs(x = "Latent Treaty Depth",
       y = "Mean Estimated Spending Growth from Alliance") +
  ggtitle("Estimated Military Spending Growth and Treaty Depth") +
  theme_bw() 
growth.depth.lg


# remove fitted model from the workspace
rm(ml.model.lmbcap.sim)

