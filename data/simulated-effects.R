# Joshua Alley
# Simulate impact of changing alliance covariates on Lambda and growth in spending 


### simulate impact of increasing treaty depth 
all.data.ldepth <- numeric(ncol(alliance.reg.mat.sm))
names(all.data.ldepth) <- c(colnames(alliance.reg.mat.sm))

# Set values of variables for simulation 
all.data.ldepth["cons"] <- 1 
all.data.ldepth["latent.depth.mean"] <- -0.80 # key IV: minimum
all.data.ldepth["uncond.milsup"] <- 0 
all.data.ldepth["econagg.dum"] <- 0
all.data.ldepth["fp.conc.index"] <- 0
all.data.ldepth["num.mem"] <- median(reg.all.data.sm$num.mem)
all.data.ldepth["low.kap.sc"] <- median(reg.all.data.sm$low.kap.sc)
all.data.ldepth["avg.democ"] <- median(reg.all.data.sm$avg.democ)
all.data.ldepth["wartime"] <- 0
all.data.ldepth["asymm"] <- 0
all.data.ldepth["mean.threat"] <- median(reg.all.data.sm$mean.threat)

# vector with high depth
all.data.hdepth <- all.data.ldepth
all.data.hdepth["latent.depth.mean"] <- 1.5 # key IV: 3rd quartile


# Simulate the effect 
lambda.ldepth <- coef$beta_sm%*%all.data.ldepth
hist(lambda.ldepth)
lambda.hdepth <- coef$beta_sm%*%all.data.hdepth
hist(lambda.hdepth)

# Look at differnce
lambda.diff.depth <- lambda.hdepth - lambda.ldepth
mean(lambda.diff.depth < 0)


# Create a dataframe with all three objects
lambda.change.depth <- cbind.data.frame(lambda.ldepth, lambda.hdepth, lambda.diff.depth)
rm(list = c("lambda.diff.depth", "lambda.ldepth", "lambda.hdepth"))

# plot and summarize the difference
summary(lambda.change.depth$lambda.diff.depth)
quantile(lambda.change.depth$lambda.diff.depth, c(.05, .95))


ggplot(lambda.change.depth, aes(x = lambda.diff.depth)) +
 geom_vline(xintercept = 0) +
 geom_density() + theme_classic()


# Calculate impact on spending at median alliance capability 
lambda.change.depth$impact.milex <- lambda.change.depth$lambda.diff.depth*median(state.mem.cap$ally.spend.norm)
lambda.change.depth$impact.milex <- sinh(lambda.change.depth$impact.milex) # reverse IHS transformation

# Plot
ggplot(lambda.change.depth, aes(x = impact.milex)) +
  geom_vline(xintercept = 0) +
  geom_density() + theme_classic()
summary(lambda.change.depth$impact.milex)
quantile(lambda.change.depth$impact.milex, c(.05, .95))


# Split densities- predict impact of alliance participation on milex in low and high depth
# Calculate impact in high and low and bind togeter
impact.milex.high <- lambda.change.depth$lambda.hdepth*median(state.mem.cap$ally.spend.norm)
impact.milex.high <- sinh(impact.milex.high)
quantile(impact.milex.high, c(.05, .95))

# low lambda/depth
impact.milex.low  <- lambda.change.depth$lambda.ldepth*median(state.mem.cap$ally.spend.norm)
impact.milex.low <- sinh(impact.milex.low)
quantile(impact.milex.low, c(.05, .95))

# combine in dataframe for plotting intervals
impact.milex.comp <- rbind.data.frame(
                       c(mean(impact.milex.low), quantile(impact.milex.low, c(.05, .95))),
                       c(mean(impact.milex.high), quantile(impact.milex.high, c(.05, .95))),
                       c(mean(lambda.change.depth$impact.milex), quantile(lambda.change.depth$impact.milex, c(.05, .95)))
)
colnames(impact.milex.comp) <- c("mean", "lower", "upper")
impact.milex.comp$Depth <- c("Shallow", "Deep", "Difference")


# Remove simulated objects
rm(list = c("impact.milex.low", "impact.milex.high"))


# Plot the results
impact.milex.sm <- ggplot(impact.milex.comp, aes(y = Depth, x = mean)) +
                        geom_vline(xintercept = 0) +
                        geom_point(size = 4) +
                        geom_errorbarh(aes(xmin = lower, xmax = upper, height = .1), size = 2) +
                        labs(x = "Predicted Growth in Military Spending",
                         y = "Treaty Depth") +
                        ggtitle("Substantive Effect of Increasing Treaty Depth") +
                         theme_bw()
impact.milex.sm

# Graphical summary of minor power alliance-level regression 
color_scheme_set("darkgray")
sm.intervals <- mcmc_intervals(coef$beta_sm, 
               prob = .9) +
                labs(x = "Effect on Allied Capability Coefficient") +
                ggtitle("Small State Alliance Regression Coefficients") +
                theme_bw()
sm.intervals


# Examine different plot layouts 
# Alliance-level regression
grid.arrange(sm.intervals, impact.milex.sm,
             nrow = 2)

results.allreg <- arrangeGrob(sm.intervals, impact.milex.sm,
                              nrow = 2)
ggsave("figures/results-allreg.jpg", results.allreg, height = 7, width = 6) #save file

# Alliance-specific parameters and predictions
grid.arrange(lambda.depth.sm, growth.depth.sm,
             nrow = 2)
results.allpred <- arrangeGrob(lambda.depth.sm, growth.depth.sm,
                              nrow = 2)
ggsave("figures/results-allpred.jpg", results.allpred, height = 7, width = 6) #save file


grid.arrange(sm.intervals, impact.milex.sm, growth.depth.sm,
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)

# large states: 
lg.intervals <- mcmc_intervals(coef$beta_lg, 
               prob = .9) +
              labs(x = "Effect on Impact of Total Allied Military Spending") +
              ggtitle("Large State Alliance Regression Coefficients")
lg.intervals 


# all in one
grid.arrange(lg.intervals, lambda.depth.lg, growth.depth.lg,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3))
)

lg.res <- arrangeGrob(lg.intervals, lambda.depth.lg, growth.depth.lg,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3))
           )
ggsave("appendix/lg-res.png", lg.res, height = 6, width = 8)
