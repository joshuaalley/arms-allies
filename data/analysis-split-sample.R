# Joshua Alley
# Texas A&M University
# Analysis of how alliance depth affects arms decisions: Split Major and minor powers


# Environment is determined by use of projects and/or running this file in conjunction with
# the scripts alliance-measures.R and dataset construction and summary.R 


# Compile the model code: 
model.2 <- stan_model(file = "data/ml-model-split.stan")


# Pulled the relevant major and minor power dataframes from the analysis script 

### Start with Major powers

# create a state index variable
reg.state.comp.maj$state.id <- reg.state.comp.maj %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp.maj$year.id <- reg.state.comp.maj %>% group_indices(year)


# run model on the full sample
# Define the data list 
reg.state.comp.maj <- as.data.frame(reg.state.comp.maj)
stan.data.maj <- list(N = nrow(reg.state.comp.maj), y = reg.state.comp.maj[, 3],
                  state = reg.state.comp.maj$state.id, S = length(unique(reg.state.comp.maj$state.id)),
                  year = reg.state.comp.maj$year.id, T = length(unique(reg.state.comp.maj$year.id)),
                  A = ncol(state.mem.maj), L = ncol(alliance.reg.mat.maj),
                  Z = state.mem.maj, 
                  X = alliance.reg.mat.maj,
                  W = reg.state.mat.maj, M = ncol(reg.state.mat.maj)
)


# Regular STAN
system.time(
  ml.model.maj <- sampling(model.2, data = stan.data.maj, 
                       iter = 2000, warmup = 1000, chains = 4, 
                       control = list(max_treedepth = 15)
  )
)

# diagnose model
# launch_shinystan(ml.model.maj)
check_hmc_diagnostics(ml.model.maj)

# posterior predictive check
yrep.maj <- extract(ml.model.maj, pars = "y_pred")
yrep.maj <- yrep.maj$y_pred[1:100, ]
ppc_dens_overlay(asinh(stan.data.maj$y), yrep.maj)


# trace plot for appendix
traceplot(ml.model.maj, pars = "beta")
ggsave("appendix/trace-all-maj.png", height = 6, width = 8)




### Now move to non-major powers

# create a state index variable
reg.state.comp.min$state.id <- reg.state.comp.min %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp.min$year.id <- reg.state.comp.min %>% group_indices(year)


reg.state.comp.min <- as.data.frame(reg.state.comp.min)
stan.data.min <- list(N = nrow(reg.state.comp.min), y = reg.state.comp.min[, 3],
                      state = reg.state.comp.min$state.id, S = length(unique(reg.state.comp.min$state.id)),
                      year = reg.state.comp.min$year.id, T = length(unique(reg.state.comp.min$year.id)),
                      A = ncol(state.mem.min), L = ncol(alliance.reg.mat.min),
                      Z = state.mem.min, 
                      X = alliance.reg.mat.min,
                      W = reg.state.mat.min, M = ncol(reg.state.mat.min)
)

# Compile the model code: 
model.2 <- stan_model(file = "data/ml-model-split.stan")

# Regular STAN
system.time(
  ml.model.min <- sampling(model.2, data = stan.data.min, 
                           iter = 2000, warmup = 1000, chains = 4, 
                           control = list(max_treedepth = 15)
  )
)

# diagnose model
# launch_shinystan(ml.model.min)
check_hmc_diagnostics(ml.model.min)


# posterior predictive check
yrep.min <- extract(ml.model.min, pars = "y_pred")
yrep.min <- yrep.min$y_pred[1:100, ]
ppc_dens_overlay(asinh(stan.data.min$y), yrep.min)


# trace plot for appendix
traceplot(ml.model.min, pars = "beta")
ggsave("appendix/trace-all-min.png", height = 6, width = 8)



### Compare the results

# Summarize Scope by major and minor powers
summary(reg.all.data.maj$latent.depth.mean) # major powers
summary(reg.all.data.min$latent.depth.mean) # minor powers


# Summarize intervals for major powers
beta.summary.maj <- summary(ml.model.maj, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.maj <- beta.summary.maj[, -2]
rownames(beta.summary.maj) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                                "FP Conc.",
                                "Number Members", "FP Similarity",
                                "Democratic Membership", 
                                "Wartime", "Asymmetric", "US. Mem", "USSR Mem.",
                                "sigma Alliances")


print(beta.summary.maj)
xtable(beta.summary.maj, digits = 3)


# summarize intervals for minor powers
beta.summary.min <- summary(ml.model.min, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.min <- beta.summary.min[, -2]
rownames(beta.summary.min) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                                 "FP Conc.",
                                 "Number Members", "FP Similarity",
                                 "Democratic Membership", 
                                 "Wartime", "Asymmetric", "US. Mem", "USSR Mem.",
                                 "sigma Alliances")

print(beta.summary.min)
xtable(beta.summary.min, digits = 3) # for appendix



# Create an object with both estimates and plot (Gelman's Secret Weapon)
lscoef.summary <- rbind(beta.summary.maj[2, ], beta.summary.min[2, ])
row.names(lscoef.summary) <- c("Major Powers", "Non-Major Powers")
lscoef.summary <- as.data.frame(lscoef.summary)

ggplot(lscoef.summary, aes(x = row.names(lscoef.summary), y = mean)) +
  geom_errorbar(aes(ymin = `5%`, 
                    ymax = `95%`,
                    width=.05), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) + geom_hline(yintercept = 0) +
  theme_classic() + labs(x = "Sample", y = "Effect of Treaty Depth") +
  coord_flip()


# Summarize the alliance cofficient estimates in major and minor subsets
coef.maj <- extract(ml.model.maj, pars = c("beta", "gamma"))
colnames(coef.maj$beta) <- colnames(alliance.reg.mat.maj)
colnames(coef.maj$gamma) <- colnames(reg.state.mat.maj)
coef.min <- extract(ml.model.min, pars = c("beta", "gamma"))
colnames(coef.min$beta) <- c("Constant", "Depth", "Uncond Milsup", "Econ. Link", 
                             "FP Conc.",
                             "Number Members", "FP Similarity",
                             "Democratic Membership", 
                             "Wartime", "Asymmetric", "US. Mem", "USSR Mem.")
colnames(coef.min$gamma) <- colnames(reg.state.mat.min)


# Graphical summary of minor power alliance-level regression 
color_scheme_set("darkgray")
mcmc_intervals(coef.min$beta, 
           prob = .9) +
  labs(x = "Effect on Impact of Total Allied Military Spending") +
  ggtitle("90% Credible Intervals of Alliance-Level Regression Coefficients")
ggsave("figures/alliance-reg-nonmaj.png", height = 6, width = 8)


# Baseline posterior probabilities
mean(coef.maj$beta[, 2] < 0) # depth: major
mean(coef.min$beta[, 2] < 0) # depth: non-major
mean(coef.maj$beta[, 3] < 0) # uncond milsup: major
mean(coef.min$beta[, 3] < 0) # uncond milsup: non-major
mean(coef.maj$beta[, 4] > 0) # econ link: major
mean(coef.min$beta[, 4] < 0) # econ link: non-major
mean(coef.maj$beta[, 5] < 0) # FP concessions: major
mean(coef.min$beta[, 5] > 0) # FP concessions: non-major
mean(coef.maj$beta[, 6] > 0) # number members: major
mean(coef.min$beta[, 6] < 0) # number members: non-major
mean(coef.maj$beta[, 7] < 0) # FP similarity: major
mean(coef.min$beta[, 7] > 0) # FP similarity: non-major
mean(coef.maj$beta[, 8] < 0) # Dem. Membership: major
mean(coef.min$beta[, 8] < 0) # Dem. Membership: non-major
mean(coef.maj$beta[, 9] < 0) # wartime: major
mean(coef.min$beta[, 9] > 0) # wartime: non-major
mean(coef.maj$beta[, 10] < 0) # asymmetric: major
mean(coef.min$beta[, 10] > 0) # asymmetric: non-major


# Create a nice comparison plot: secret weapon
depth.dens <- cbind(coef.maj$beta[, 2], coef.min$beta[, 2])
colnames(depth.dens) <- c("Major", "Non-Major")
depth.dens <- melt(depth.dens)

ggplot(depth.dens, aes(x = value,  fill = Var2)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(name = "Sample", values=c("#999999", "#000000")) +
  ggtitle("Posterior Distributions of Treaty Depth: Major and Non-Major Powers") +
  theme_classic()
ggsave("figures/depth-dens-split.png", height = 6, width = 8)



# Examine state-level parameters
mcmc_areas(coef.maj$gamma, 
           prob = .9)
mcmc_areas(coef.min$gamma, 
           prob = .9)




# compare trends in lambdas across treaty depth in major and minor

# Start with major powers
lambda.means.maj <- get_posterior_mean(ml.model.maj, pars = "lambda")
lambda.df.maj <- tibble(lambda = lambda.means.maj[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.maj)))
cor.test(lambda.df.maj$lambda, lambda.df.maj$latent.depth.mean,
         alternative = "less", method = "spearman")

# plot major powers
lambda.depth.maj <- ggplot(lambda.df.maj, aes(x = latent.depth.mean, y = lambda)) +
                    geom_hline(yintercept = 0) +
                   geom_point() +
                   geom_smooth(method = "lm") + theme_classic() +
            labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
            ggtitle("Major Powers")
lambda.depth.maj



# Non-major powers
lambda.means.min <- get_posterior_mean(ml.model.min, pars = "lambda")
lambda.df.min <- tibble(lambda = lambda.means.min[, 5]) %>%  # add lambdas to df
    bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.min)))
cor.test(lambda.df.min$lambda, lambda.df.min$latent.depth.mean, 
         alternative = "less", method = "spearman")

# plot non-major powers
lambda.depth.min <- ggplot(lambda.df.min, aes(x = latent.depth.mean, y = lambda)) +
            geom_hline(yintercept = 0) +
             geom_point() +
                  geom_smooth(method = "lm") + theme_classic() +
            labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
            ggtitle("Non-Major Powers: Association Between Depth and Alliance Impact")
lambda.depth.min
ggsave("figures/lambda-ld-nonmaj.png", height = 6, width = 8)


# Compare all in one plot w/ multiplot.ggplot function
multiplot.ggplot(lambda.depth.maj, lambda.depth.min)



# Compare lambdas for alliances with major and minor power members
lambda.df.mix <- lambda.df.min %>%
  select(atopid, lambda) %>%
  left_join(select(lambda.df.maj, atopid, lambda), 
            by = c("atopid")) %>% 
     rename(
     lambda.min = lambda.x,
     lambda.maj = lambda.y
     )
lambda.df.mix <- as.data.frame(lambda.df.mix)
lambda.df.mix <- melt(lambda.df.mix, 
                id = c("atopid"))

ggplot(lambda.df.mix, aes(x = variable, y = value)) +
  geom_violin() + theme_dark()



# More detailed posterior inference and comparisons
# Extract coefficients from the major-power model
sum.maj.post <- extract(ml.model.maj, pars = c("beta", "gamma", "lambda"),
                        permuted = TRUE) # major power

# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.maj <- apply(sum.maj.post$lambda, 2, positive.check)
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
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, latent.depth.mean)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()





# Extract coefficients from non-major power model
sum.min.post <- extract(ml.model.min, pars = c("beta", "gamma", "lambda"),
                        permuted = TRUE) # non-major power

# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.min <- apply(sum.min.post$lambda, 2, positive.check)
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
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, latent.depth.mean)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()



# Plot Lambdas for US alliances: error bars

# Get credible intervals
lambda.min.sum <- apply(sum.min.post$lambda, 2, function(x) quantile(x, c(.05, .95)))

lambda.min.sum <- cbind.data.frame(lambda.probs.min$atopid, reg.all.data.min$us.mem,
                                   lambda.probs.min$lambda.mean, reg.all.data.min$latent.depth.mean,
                                  t(lambda.min.sum))
colnames(lambda.min.sum) <- c("atopid", "us.mem", "lambda.mean", 
                              "latent.depth.mean", "lower", "upper")


# plot non-major power lambdas against depth w/ credible intervals
# hard to read, so this did not make the paper
ggplot(lambda.min.sum, aes(x = latent.depth.mean, y = lambda.mean)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper, alpha = .15)) +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth", y = "Effect of Allied Spending") +
  ggtitle("Non-Major Powers: Association Between Depth and Alliance Impact")



# Make the plot
lambda.us.int <- lambda.min.sum %>%
                 filter(us.mem == 1) %>%
                   ggplot( aes(x = lambda.mean, y = atopid)) +
                    geom_point(size = 2) +
                    geom_errorbarh(aes(xmin = lower, xmax = upper),
                      size = 1.5) +
                    ggtitle("Impact of Alliance with US on Non-major Power Military Spending") +
                  labs(x = "Alliance Impact", y = "ATOPID") +
                  geom_vline(xintercept = 0) +
                  theme_classic()
lambda.us.int
ggsave("figures/lambda-us-min.png", height = 6, width = 8)



# Highlight alliances with US membership 
lambda.depth.us <- ggplot(lambda.min.sum, aes(x = latent.depth.mean, y = lambda.mean, shape = factor(us.mem))) +
                    geom_hline(yintercept = median(lambda.min.sum$lambda.mean)) +
                    geom_vline(xintercept = median(lambda.min.sum$latent.depth.mean)) +
  geom_point(mapping = aes(color = factor(us.mem)), size = 3) +
  scale_color_manual(values = c("#999999", "#000000")) +
  labs(x = "Latent Depth", y = "Alliance Impact",
       color = "US Membership", shape = "US Membership") +
  ggtitle("Depth and Impact of US Alliances") +
  theme_classic()
lambda.depth.us
ggsave("figures/lambda-depth-us.png", height = 6, width = 8)

multiplot.ggplot(lambda.us.int, lambda.depth.us)


### Calculate aggregate impact of alliance participation on growth in spending 

# Major powers 
dim(state.mem.maj)

# matrix multiplication of membership matrix by mean lambda 
agg.all.maj  <- state.mem.maj%*%lambda.means.maj[, 5]

summary(agg.all.maj)
agg.all.maj <- cbind(reg.state.comp.maj$ccode,
                     reg.state.comp.maj$year, agg.all.maj)

# non-major power
dim(state.mem.min)

# matrix multiplication of membership matrix by mean lambda 
agg.all.min  <- state.mem.min%*%lambda.means.min[, 5]

summary(agg.all.min)
agg.all.min <- cbind(reg.state.comp.min$ccode,
                     reg.state.comp.min$year, agg.all.min)


# Predicted military spending change for all individual alliances
a.min <- ncol(state.mem.min)
growth.pred.min <- rep(NA, a.min)
growth.pred.min <- list()

# Loop over matrix columns
for(i in 1:a.min){
  growth.pred.min[[i]] <- state.mem.min[, i][state.mem.min[, i] != 0] # filter out zeros 
  growth.pred.min[[i]] <- growth.pred.min[[i]]%*%t(sum.min.post$lambda[, i]) # multiply by lambda
  growth.pred.min[[i]] <- as.data.frame(growth.pred.min[[i]])
}

names(growth.pred.min) <- c(colnames(state.mem.min)) # label each matrix with ATOPID


# Capture means and add a label variable 
growth.pred.mean <- lapply(growth.pred.min, function(x) apply(x, 1, mean))
for(i in 1:a.min){
  growth.pred.mean[[i]] <- as.data.frame(growth.pred.mean[[i]])
  growth.pred.mean[[i]]$atopid <-  colnames(state.mem.min)[[i]]
}

growth.pred.sd <- lapply(growth.pred.min, function(x) as.data.frame(apply(x, 1, sd)))
for(i in 1:a.min){
  growth.pred.sd[[i]] <- as.data.frame(growth.pred.sd[[i]])
}

# combine means and sds in a dataframe 
growth.pred.res <- cbind(do.call(rbind, growth.pred.mean), unlist(growth.pred.sd))
colnames(growth.pred.res) <- c("mean.pred", "atopid", "sd.pred")
growth.pred.res$atopid <- as.numeric(growth.pred.res$atopid)

# Add alliance characteristics
growth.pred.res <- left_join(growth.pred.res, alliance.char)

# Create a dataframe with maximum predicted change, positive or negative 
growth.pred.res.max <- growth.pred.res %>%
                        group_by(atopid) %>% 
                         filter(mean.pred == max(abs(mean.pred)))    



# plot: illeible
ggplot(growth.pred.res, aes(x = latent.depth.mean, y = mean.pred)) +
  geom_hline(yintercept = 0) +
  geom_point(position = position_jitter(width = 0.1), alpha = .25) + 
#  geom_errorbar(aes(ymin = mean.pred - 2*sd.pred, ymax = mean.pred + 2*sd.pred)) +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Depth", y = "Mean Predicted Growth from Alliance") +
  theme_classic() 
cor.test(growth.pred.res$latent.depth.mean, growth.pred.res$mean.pred)




# Save model and take it out of the workspace (1.7 GB w/ likelihood and PPC)
saveRDS(ml.model.min, "data/ml-model-min-znorm.rds")
rm(ml.model.min)
