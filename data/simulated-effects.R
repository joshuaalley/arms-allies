# Joshua Alley
# Texas A&M University
# Simulate impact of changing alliance covariates on Lambda and growth in spending 


### simulate impact of increasing treaty depth 
all.data.ldepth <- numeric(ncol(alliance.reg.mat.min))
names(all.data.ldepth) <- c(colnames(alliance.reg.mat.min))

# Set values of variables for simulation 
all.data.ldepth["cons"] <- 1 
all.data.ldepth["latent.depth.mean"] <- -0.80 # key IV: minimum
all.data.ldepth["uncond.milsup"] <- 0 
all.data.ldepth["econagg.dum"] <- 0
all.data.ldepth["fp.conc.index"] <- 0
all.data.ldepth["num.mem"] <- median(reg.all.data.min$num.mem)
all.data.ldepth["low.kap.sc"] <- median(reg.all.data.min$low.kap.sc)
all.data.ldepth["avg.democ"] <- median(reg.all.data.min$avg.democ)
all.data.ldepth["wartime"] <- 0
all.data.ldepth["asymm"] <- 0
all.data.ldepth["mean.threat"] <- median(reg.all.data.min$mean.threat)
all.data.ldepth["us.mem"] <- 0
all.data.ldepth["ussr.mem"] <- 0

# vector with cpa present
all.data.hdepth <- all.data.ldepth
all.data.hdepth["latent.depth.mean"] <- 1.5 # key IV: 3rd quartile


# Simulate the effect 
lambda.ldepth <- coef.min$beta%*%all.data.ldepth
hist(lambda.ldepth)
lambda.hdepth <- coef.min$beta%*%all.data.hdepth
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
impact.milex.nonmaj <- ggplot(impact.milex.comp, aes(y = Depth, x = mean)) +
                        geom_point(size = 4) +
                        geom_errorbarh(aes(xmin = lower, xmax = upper, height = .1), size = 2) +
                        labs(x = "Predicted Growth in Military Spending",
                         y = "Treaty Depth") +
                        ggtitle("Substantive Effect of Increasing Treaty Depth") +
                         theme_bw()
impact.milex.nonmaj

# Graphical summary of minor power alliance-level regression 
color_scheme_set("darkgray")
nonmaj.intervals <- mcmc_intervals(coef.min$beta, 
               prob = .9) +
                labs(x = "Effect on Allied Capability Coefficient") +
                ggtitle("Alliance Regression Coefficients") +
                theme_bw()
nonmaj.intervals


# Examine different plot layouts 
# Alliance-level regression
grid.arrange(nonmaj.intervals, impact.milex.nonmaj,
             nrow = 2)

results.allreg <- arrangeGrob(nonmaj.intervals, impact.milex.nonmaj,
                              nrow = 2)
ggsave("figures/results-allreg.png", results.allreg, height = 6, width = 8) #save file

# Alliance-specific parameters and predictions
grid.arrange(lambda.depth.min, growth.depth.plot,
             nrow = 2)
results.allpred <- arrangeGrob(lambda.depth.min, growth.depth.plot,
                              nrow = 2)
ggsave("figures/results-allpred.png", results.allpred, height = 8, width = 8) #save file


grid.arrange(nonmaj.intervals, impact.milex.nonmaj, growth.depth.plot,
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)


