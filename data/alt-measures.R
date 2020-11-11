# Joshua Alley
# Texas A&M University 
# Compare latent depth with others


# model with depth rescaled 
stan.data.lmbcap.rsd <- stan.data.lmbcap
# shift distribution of latent depth for min 0
# small 
stan.data.lmbcap.rsd$X_sm[, 2] <- stan.data.lmbcap.rsd$X_sm[, 2] +
                                    abs(min(stan.data.lmbcap.rsd$X_sm[, 2]))
summary(stan.data.lmbcap.rsd$X_sm[, 2])
# large
stan.data.lmbcap.rsd$X_lg[, 2] <- stan.data.lmbcap.rsd$X_lg[, 2] +
  abs(min(stan.data.lmbcap.rsd$X_lg[, 2]))
summary(stan.data.lmbcap.rsd$X_lg[, 2])


# fit model- only save alliance pars
system.time(
  ml.model.lmbcap.rsd <- sampling(model.lmbcap.sim, 
                                  data = stan.data.lmbcap.rsd,
                                  iter = 2000, warmup = 1000, chains = 4,
                                  control=list(max_treedepth = 15),
                                  pars = c("beta_sm", "beta_lg",
                                           "lambda_sm", "lambda_lg"),
                                  include = TRUE
  )
)

check_hmc_diagnostics(ml.model.lmbcap.rsd)


# extract coefs 
coef.rsd <- extract(ml.model.lmbcap.rsd)
colnames(coef.rsd$beta_sm) <- colnames(coef$beta_sm)
colnames(coef.rsd$beta_lg) <- colnames(coef$beta_lg)

# Summary for the appendix
color_scheme_set("darkgray")
sm.intervals.rsd <- mcmc_intervals(coef.rsd$beta_sm, 
                       prob = .9) +
                   ggtitle("Small Alliance Members") +
                   theme_classic()
sm.intervals.rsd
# large state
lg.intervals.rsd <- mcmc_intervals(coef.rsd$beta_lg, 
                        prob = .9) +
                     ggtitle("Large Alliance Members") + 
                      theme_classic()
lg.intervals.rsd
# Start with large powers
lambda.means.lg.rsd <- get_posterior_mean(ml.model.lmbcap.rsd, pars = "lambda_lg")
lambda.df.lg.rsd <- cbind.data.frame(lambda.means.lg.rsd[, 5], stan.data.lmbcap.rsd$X_lg)
colnames(lambda.df.lg.rsd)[1] <- "lambda"
cor.test(lambda.df.lg.rsd$lambda, lambda.df.lg.rsd$latent.depth.mean,
         alternative = "less", method = "spearman")

# plot karge powers
lambda.depth.lg.rsd <- ggplot(lambda.df.lg.rsd, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth (Shifted)", y = "Effect of Allied Spending") +
  ggtitle("Large Alliance Members")
lambda.depth.lg.rsd



# small powers
lambda.means.sm.rsd <- get_posterior_mean(ml.model.lmbcap.rsd, pars = "lambda_sm")
lambda.df.sm.rsd <- cbind.data.frame(lambda.means.sm.rsd[, 5], stan.data.lmbcap.rsd$X_sm)
colnames(lambda.df.sm.rsd)[1] <- "lambda"
cor.test(lambda.df.sm.rsd$lambda, lambda.df.sm.rsd$latent.depth.mean, 
         alternative = "less", method = "spearman")

# plot small powers
lambda.depth.sm.rsd <- ggplot(lambda.df.sm.rsd, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Latent Treaty Depth (Shifted)", y = "Allied Capability Coefficent") +
  ggtitle("Small Alliance Members")
lambda.depth.sm.rsd


# Combine small and large regression plots 
grid.arrange(sm.intervals.rsd, lambda.depth.sm.rsd,
             lg.intervals.rsd, lambda.depth.lg.rsd)
# set as own object and export
rsd.plots <- arrangeGrob(sm.intervals.rsd, lambda.depth.sm.rsd,
                          lg.intervals.rsd, lambda.depth.lg.rsd)
ggsave("appendix/rsd-plots.png", rsd.plots, height = 6, width = 8)



### Leeds and Anac 2005: Military Institutionalization
# Plot the two variables against each other
ggplot(atop.milsup, aes(x = as.ordered(milinst), y = latent.depth.mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .5) +
  labs(x = "Leeds and Anac Military Institutionalization",
       y = "Latent Treaty Depth") +
  ggtitle("Comparison of Latent BFA Measure and Military Institutionalization") +
  theme_bw()
ggsave("appendix/leeds-anac-comp.png", height = 6, width = 8)

# Correlation between my latent measure and milinst
cor.test(atop.milsup$latent.depth.mean, atop.milsup$milinst)
# Strong positive correlation


# Reanalysis with milinst measure
# Make the alliance characteristics data match the membership matrix
reg.all.data.milinst <- filter(alliance.char, atopid %in% colnames(state.mem.full)) %>%
  select(atopid, milinst, uncond.milsup, econagg.dum, 
         fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, mean.threat) %>%
  na.omit


# finalize alliance-level matrices
# small states
all.data.milinst.sm <- filter(reg.all.data.milinst, 
                              atopid %in% colnames(state.mem.sm)) 
# 5 w/ NA instiutionalization
state.mem.sm.milinst <- state.mem.sm[, colnames(state.mem.sm) %in% all.data.milinst.sm$atopid]
all.data.milinst.sm <- select(all.data.milinst.sm, -c(atopid))
cons <- rep(1, nrow(all.data.milinst.sm))
alliance.mat.milinst.sm <- as.matrix(cbind(cons, all.data.milinst.sm))

# large state
all.data.milinst.lg <- filter(reg.all.data.milinst, 
                              atopid %in% colnames(state.mem.lg)) 
# 5 w/ NA instiutionalization
state.mem.lg.milinst <- state.mem.lg[, colnames(state.mem.lg) %in% all.data.milinst.lg$atopid]
all.data.milinst.lg <- select(all.data.milinst.lg, -c(atopid))
cons <- rep(1, nrow(all.data.milinst.lg))
alliance.mat.milinst.lg <- as.matrix(cbind(cons, all.data.milinst.lg))




# Define the data list 
reg.state.comp <- as.data.frame(reg.state.comp)
stan.data.milinst <- list(N = nrow(reg.state.comp), y = reg.state.comp[, 3],
                         state = reg.state.comp$state.id, S = length(unique(reg.state.comp$state.id)),
                         year = reg.state.comp$year.id, T = length(unique(reg.state.comp$year.id)),
                         A_sm = ncol(state.mem.sm.milinst), A_lg = ncol(state.mem.lg.milinst),
                         L = ncol(alliance.reg.mat.sm),
                         Z_sm = state.mem.sm.milinst, 
                         Z_lg = state.mem.lg.milinst,
                         X_sm = alliance.mat.milinst.sm,
                         X_lg = alliance.mat.milinst.lg,
                         J = 2,
                         W = reg.state.mat, M = ncol(reg.state.mat)
)

# Regular STAN
system.time(
  ml.model.milinst <- sampling(model.lmbcap.sim, data = stan.data.milinst, 
                           iter = 2400, warmup = 1200, chains = 4, 
                           control = list(max_treedepth = 15),
                           pars = c("beta_sm", "beta_lg", "lambda_sm", "lambda_lg", "gamma",
                                    "alpha", "sigma", "sigma_state", "sigma_year",
                                    "sigma_all_lg", "sigma_all_sm",
                                    "y_rep"),
                           include = TRUE
  )
)
# Save model 
saveRDS(ml.model.milinst, "data/ml-model-milinst.rds")
# diagnose model
check_hmc_diagnostics(ml.model.milinst)


# summarize intervals for minor powers w/ milinst measure
beta.summary.milinst <- summary(ml.model.milinst, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.milinst <- beta.summary.milinst[, -2]
rownames(beta.summary.milinst) <- c("Constant", "Military Inst.", "Uncond. Milsup.", "Econ. Link", 
                                "FP Conc.",
                                "Number Members", "FP Similarity",
                                "Democratic Membership", 
                                "Wartime", "Asymmetric", "US. Mem", "USSR Mem.",
                                "sigma Alliances")

print(beta.summary.milinst)
xtable(beta.summary.milinst, digits = 3) # for appendix


# check posterior probabilities
coef.milinst <- extract(ml.model.milinst, pars = c("beta"))
mean(coef.milinst$beta[, 2] < 0) 

# Produce plot of lambda against military institutionalization
lambda.means.milinst <- get_posterior_mean(ml.model.milinst, pars = "lambda")
lambda.df.milinst <- tibble(lambda = lambda.means.milinst[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.milinst)))
cor.test(lambda.df.milinst$lambda, lambda.df.milinst$latent.depth.mean,
         alternative = "less", method = "spearman")

# plot
ggplot(lambda.df.milinst, aes(x = as.factor(milinst), y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .5) +
  theme_bw() +
  labs(x = "Military Instituionalization", y = "Effect of Allied Spending") 
ggsave("appendix/milinst-lambda.png", height = 5, width = 7)

# remove fitted model
rm(ml.model.milinst)


### Look at the Benson and Clinton results
benson.clinton.2016 <- read.csv("data/benson-clinton-scores.csv")
head(benson.clinton.2016)

# combine with my data
benson.clinton.comp <- left_join(atop.milsup, benson.clinton.2016, by = "atopid")

# rescale variables to facilitate comparison
benson.clinton.comp$latent.depth.rs <- (benson.clinton.comp$latent.depth.mean -
                                         mean(benson.clinton.comp$latent.depth.mean)) /
                                           sd(benson.clinton.comp$latent.depth.mean)
benson.clinton.comp$Depth.score.rs <- (benson.clinton.comp$Depth.score -
                                         mean(benson.clinton.comp$Depth.score, na.rm = TRUE)) /
                                          sd(benson.clinton.comp$Depth.score, na.rm = TRUE)

# correlation is strong and positive
cor.test(benson.clinton.comp$latent.depth.mean, benson.clinton.comp$Depth.score)

# compare summary statistics
summary(benson.clinton.comp$latent.depth.rs)
summary(benson.clinton.comp$Depth.score.rs)

# plot the meaures individually, then together
hist.bfa <- ggplot(benson.clinton.comp, aes(x = latent.depth.rs)) +
  geom_histogram() +
  ggtitle("Alley")
hist.bc16 <- ggplot(benson.clinton.comp, aes(x = Depth.score.rs)) +
  geom_histogram() +
  ggtitle("Benson and Clinton")
multiplot.ggplot(hist.bfa, hist.bc16)

# Scatter plot
bc.score.comp <- ggplot(benson.clinton.comp, aes(y = Depth.score.rs, x = latent.depth.rs)) +
                  geom_point() +
                  labs(y = "Rescaled Benson and Clinton Depth Score",
                       x = "Rescaled BFA Measure of Depth") +
                  ggtitle("Comparison of Latent Depth Measures") +
                  theme_bw()
bc.score.comp


# combine with full atop data: need pure neutrality pacts
bc.comp.full <- left_join(atop, benson.clinton.2016, by = "atopid")

# neutrality only variable
bc.comp.full$neut.only <- ifelse(bc.comp.full$neutral == 1 & bc.comp.full$defense == 0 &
                                   bc.comp.full$offense == 0, 1, 0)
table(bc.comp.full$neutral, bc.comp.full$neut.only) # most neutrality pacts are only that

# look at depth scores by neutrality
ggplot(bc.comp.full, aes(y = Depth.score, x = as.factor(neut.only))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .5) +
  scale_x_discrete(labels = c("0" = "Military Support",
                              "1" = "Neutrality Only")) +
  labs(y = "Benson and Clinton Depth Score",
       x = "") +
  ggtitle("Depth of Neutrality Treaties: Benson and Clinton 2016") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11)) 
ggsave("appendix/bc2016-neutral.png", height = 6, width = 8)



# Compare factor loadings
# Load Benson and Clinton MCMC object
load("data/depth_factanal.R")

bc.loading <- depth[,1:9] # factor scores
rm(depth) # remove full MCMC

# Create a dataset of factors
bc.latent.factors <- cbind.data.frame(c("Military Contact", "Common Defense Policy",
                                  "Integrated Command", "Military Aid",
                                  "Bases", "Specific Contrib", 
                                  "Formal IO", "Economic Aid", "Secrecy"),
                                   apply(bc.loading, 2, mean),
                                   apply(bc.loading, 2, sd))

colnames(bc.latent.factors) <- c("var", "mean", "sd")

# plot factor loadings 
bc.latent.factors <- arrange(bc.latent.factors, desc(mean)) 
bc.latent.factors$var<- reorder(bc.latent.factors$var, bc.latent.factors$mean)

ggplot(bc.latent.factors, aes(x = mean, y = var)) + 
        geom_point(size = 2) +
        geom_errorbarh(aes(xmin = mean - 2*sd, 
                xmax = mean + 2*sd),
                height = .2, size = 1) +
        geom_vline(xintercept = 0) +
        labs(x = "Factor Loading", y = "Variable") +
        theme_classic()

# Plot the two loadings together
factors.data <- rbind(latent.factors, bc.latent.factors)
factors.data$Model <- c(rep("BFA", nrow(latent.factors)),
                        rep("Benson and Clinton", nrow(bc.latent.factors)))

factors.comp <- ggplot(factors.data, aes(x = mean, y = var, color = Model)) +
                 geom_point(size = 2) +
                 geom_errorbarh(aes(xmin = mean - 2*sd, 
                     xmax = mean + 2*sd),
                     height = .2, size = 1) +
                 geom_vline(xintercept = 0) +
                 labs(x = "Factor Loading", y = "Variable") +
                 ggtitle("Comparison of Factor Loadings for Alliance Treaty Depth") +
                 theme_bw()
factors.comp

# combine with scatter plot 
grid.arrange(factors.comp, bc.score.comp,
             nrow = 2)
bc.comp.plot <- arrangeGrob(factors.comp, bc.score.comp,
                               nrow = 2)
ggsave("appendix/bc-2016-comp.png", bc.comp.plot, height = 8, width = 8) #save file
