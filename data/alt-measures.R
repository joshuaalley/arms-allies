# Joshua Alley
# Texas A&M University 
# Compare my measure of depth with others


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
reg.all.data.milinst <- filter(alliance.char, atopid %in% colnames(state.mem.min)) %>%
  select(atopid, milinst, uncond.milsup, econagg.dum, fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, us.mem, ussr.mem) %>%
  na.omit

# need a new membership matrix 
state.mem.milinst <- state.mem.min[, colnames(state.mem.min) %in% reg.all.data.milinst$atopid]

reg.all.data.milinst <- select(reg.all.data.milinst, -c(atopid))

# define non-milinstor power alliance matrix
cons.milinst <- rep(1, nrow(reg.all.data.milinst))
alliance.reg.mat.milinst <- cbind(cons.milinst, reg.all.data.milinst)
alliance.reg.mat.milinst <- as.matrix(alliance.reg.mat.milinst)



# set-up the data
# STAN data 
stan.data.milinst <- list(N = nrow(reg.state.comp.min), y = reg.state.comp.min[, 3],
                      state = reg.state.comp.min$state.id, S = length(unique(reg.state.comp.min$state.id)),
                      year = reg.state.comp.min$year.id, T = length(unique(reg.state.comp.min$year.id)),
                      A = ncol(state.mem.milinst), L = ncol(alliance.reg.mat.milinst),
                      Z = state.mem.milinst, 
                      X = alliance.reg.mat.milinst,
                      W = reg.state.mat.min, M = ncol(reg.state.mat.min)
)

# Compile the model code: 
model.2 <- stan_model(file = "data/ml-model-split.stan")

# Regular STAN
system.time(
  ml.model.milinst <- sampling(model.2, data = stan.data.milinst, 
                           iter = 2000, warmup = 1000, chains = 4, 
                           control = list(max_treedepth = 15)
  )
)

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

# Save model and take it out of the workspace (1.7 GB w/ likelihood and PPC)
saveRDS(ml.model.milinst, "data/ml-model-milinst.rds")
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
                                  "Bases", "Contribution", 
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
factors.data$Model <- c(rep("Alley", nrow(latent.factors)),
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
