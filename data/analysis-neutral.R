# Joshua Alley
# Neutral, Offensive and Defensive Alliances



### Generate a latent measure of depth for alliances with military support

# Turn dummy indicators into factors in a separate dataset
atop.mil <- filter(atop, offense == 1 | defense == 1 | neutral == 1) 
atop.depth.neut <- select(atop.mil,
                     intcom, compag.mil,  
                     milaid, milcon,
                     base.dum, 
                     organ1, contrib, subord) 
atop.depth.neut <- as.data.frame(atop.depth.neut)
for(i in 1:ncol(atop.depth.neut)){
  atop.depth.neut[, i] <- as.ordered(atop.depth.neut[, i])
}



# Use Murray BFA approach
latent.depth.neut <- bfa_copula(~ intcom + compag.mil + 
                             milaid + milcon +
                             base.dum + 
                             organ1 + contrib + subord, 
                           data = atop.depth.neut, num.factor = 1,
                           restrict = list(c("base.dum", 1, ">0")),
                           factor.scales = FALSE,
                           keep.scores = TRUE, loading.prior = "normal", 
                           px = TRUE, imh.iter = 1000, imh.burn = 1000,
                           nburn = 20000, nsim = 30000, thin = 30, print.status = 2000)

# Little bit of diagnosis
plot(get_coda(latent.depth.neut))

# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.depth.neut, scores = TRUE)
effectiveSize(lcap.sam)
diag.geweke  <- geweke.diag(lcap.sam)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))

# plot density of factors
# Create a dataset of factors
latent.factors.neut <- cbind.data.frame(c("Integrated Command", "Companion Mil. Agreement", 
                                     "Military Aid", "Policy Coordination", 
                                     "Bases",
                                     "Formal IO", "Specific Contrib", "Subordination"),
                                   latent.depth.neut[["post.loadings.mean"]],
                                   sqrt(latent.depth.neut[["post.loadings.var"]])
)
colnames(latent.factors.neut) <- c("var", "mean", "sd")

# plot factor loadings 
latent.factors.neut <- arrange(latent.factors.neut, desc(mean)) 
latent.factors.neut$var<- reorder(latent.factors.neut$var, latent.factors.neut$mean)

neutral.load <- ggplot(latent.factors.neut, aes(x = mean, y = var)) + 
                  geom_point(size = 2) +
                  geom_errorbarh(aes(xmin = mean - 2*sd, 
                     xmax = mean + 2*sd),
                    height = .2, size = 1) +
                  geom_vline(xintercept = 0) +
                  labs(x = "Factor Loading", y = "Variable") +
                  theme_classic() +
                  ggtitle("Factor Loadings")
neutral.load

# get posterior scores of latent factor: mean and variance
atop.mil$latent.depth.neut.mean <- as.numeric(t(latent.depth.neut$post.scores.mean))
atop.mil$latent.depth.neut.var <- as.numeric(t(latent.depth.neut$post.scores.var))
atop.mil$latent.depth.neut.sd <- sqrt(atop.mil$latent.depth.neut.var)


# plot distribition of depth by neutral/active support

# look at depth scores by neutrality
atop.mil$neut.only <- ifelse(atop.mil$neutral == 1 & 
                               atop.mil$offense == 0 &
                               atop.mil$defense == 0,
                             1, 0)
table(atop.mil$neut.only) # checks out
neutral.dist <- ggplot(atop.mil, aes(y = latent.depth.neut.mean, x = as.factor(neut.only))) +
                  geom_boxplot(outlier.shape = NA) +
                  geom_jitter(alpha = .5) +
                  scale_x_discrete(labels = c("0" = "Military Support",
                              "1" = "Neutrality Only")) +
                  labs(y = "BFA Depth Score",
                       x = "") +
                  ggtitle("Depth of Neutrality Treaties: ATOP V4") +
                  theme_bw() +
                  theme(axis.text.x = element_text(size = 11)) 
neutral.dist

grid.arrange(neutral.load, neutral.dist, nrow = 2)
neutral.plots <- arrangeGrob(neutral.load, neutral.dist, nrow = 2)
ggsave("appendix/neutral-plot.png", neutral.plots, height = 6, width = 8)
