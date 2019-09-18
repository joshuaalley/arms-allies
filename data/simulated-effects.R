# Joshua Alley
# Texas A&M University
# Simulate impact of changing alliance covariates on Lambda and growth in spending 


### simulate impact of increasing treaty depth 
all.data.ldepth <- numeric(ncol(alliance.reg.mat.min))
names(all.data.ldepth) <- c(colnames(alliance.reg.mat.min))

# Set values of variables for simulation 
all.data.ldepth["cons"] <- 1 
all.data.ldepth["latent.depth.mean"] <- -0.6035 # key IV: minimum
all.data.ldepth["econagg.dum"] <- 1
all.data.ldepth["fp.conc.index"] <- 1
all.data.ldepth["num.mem"] <- median(reg.all.data.min$num.mem)
all.data.ldepth["low.kap.sc"] <- median(reg.all.data.min$low.kap.sc)
all.data.ldepth["avg.democ"] <- median(reg.all.data.min$avg.democ)
all.data.ldepth["wartime"] <- 0
all.data.ldepth["asymm"] <- 0
all.data.ldepth["us.mem"] <- 0
all.data.ldepth["ussr.mem"] <- 1

# vector with cpa present
all.data.hdepth <- all.data.ldepth
all.data.hdepth["latent.depth.mean"] <- 1.16 # key IV: maximum


# Simulate the effect 
lambda.ldepth <- coef.min$beta%*%all.data.ldepth
hist(lambda.ldepth)
lambda.hdepth <- coef.min$beta%*%all.data.hdepth
hist(lambda.hdepth)

# Look at differnce
lambda.diff.depth <- lambda.hdepth - lambda.ldepth
mean(lambda.diff.depth > 0)
hist(lambda.diff.depth)


# Create a dataframe with all three objects
lambda.change.depth <- cbind.data.frame(lambda.ldepth, lambda.hdepth, lambda.diff.depth)
rm(list = c("lambda.diff.depth", "lambda.ldepth", "lambda.hdepth"))

# plot and summarize the difference
summary(lambda.change.depth$lambda.diff.depth)

ggplot(lambda.change.depth, aes(x = lambda.diff.depth)) +
 geom_vline(xintercept = 0) +
 geom_density() + theme_classic()


# Calculate impact on spending at median alliance capability 
lambda.change.depth$impact.milex <- lambda.change.depth$lambda.diff.depth*median(state.mem.cap$ally.spend.rescale)

# Plot
ggplot(lambda.change.depth, aes(x = impact.milex)) +
  geom_vline(xintercept = 0) +
  geom_density() + theme_classic()
