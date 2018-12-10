# Joshua Alley
# Texas A&M University
# Analysis of how alliance credibility affects arms decisions: Split Major and minor powers

# Load packages
library(here)
library(arm)
library(dplyr)
library(rstan)
library(bayesplot)
library(shinystan)
library(reshape2)
library(party)
library(xtable)

# Set working directory to current folder 
setwd(here::here())
getwd()

# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Set seed
set.seed(12)


# Environment is determined by use of projects and/or running this file in conjunction with
# the scripts alliance-measures.R and dataset construction and summary.R 
# run this after analysis.R 



# Pulled the relevant major and minor power dataframes from the analysis script 

### Start with Major powers

# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.maj <- as.matrix(reg.state.comp.maj[, 13: ncol(reg.state.comp.maj)])
# remove alliances with no major power participation
state.mem.maj <- state.mem.maj[, colSums(state.mem.maj != 0) > 0]


# create a state index variable
reg.state.comp.maj$state.id <- reg.state.comp.maj %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp.maj$year.id <- reg.state.comp.maj %>% group_indices(year)



# Create the matrix of alliance-level variables
# Make the alliance characteristics data match the membership matrix
reg.all.data.maj <- filter(alliance.char, atopid %in% colnames(state.mem.maj)) %>%
  select(atopid, latent.str.mean, num.mem, 
         dem_prop, wartime, asymm, us.mem, ussr.mem)



# Replace missing conditions (arms, instituions and military aid) with zeros
reg.all.data.maj[is.na(reg.all.data.maj)] <- 0



### transform data into matrices for STAN
# State-level characeristics
reg.state.mat.maj <- as.matrix(reg.state.comp.maj[, 4:11])

# State membership in alliances
# pull the alliance-level regressors into a matrix
alliance.reg.mat.maj <- as.matrix(reg.all.data.maj[, 2: ncol(reg.all.data.maj)])

# Add a constant to the alliance-level regression 
cons.maj <- rep(1, length = nrow(alliance.reg.mat.maj))
alliance.reg.mat.maj <- cbind(cons.maj, alliance.reg.mat.maj)


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



# Compile the model code: no generated quantities block to keep size down
model.2 <- stan_model(file = "data/ml-model-nogq.stan")

# Regular STAN
system.time(
  ml.model.maj <- sampling(model.2, data = stan.data.maj, 
                       iter = 2000, warmup = 1000, chains = 4, 
                       control = list(max_treedepth = 15)
  )
)

# diagnose model
launch_shinystan(ml.model.maj)
check_hmc_diagnostics(ml.model.maj)




### Now move to minor powers
# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.min <- as.matrix(reg.state.comp.min[, 13: ncol(reg.state.comp.min)])
# remove alliances with no major power participation
state.mem.min <- state.mem.min[, colSums(state.mem.min != 0) > 0]


# create a state index variable
reg.state.comp.min$state.id <- reg.state.comp.min %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp.min$year.id <- reg.state.comp.min %>% group_indices(year)



# Create the matrix of alliance-level variables
# Make the alliance characteristics data match the membership matrix
reg.all.data.min <- filter(alliance.char, atopid %in% colnames(state.mem.min)) %>%
  select(atopid, latent.str.mean, num.mem, 
         dem_prop, wartime, asymm, us.mem, ussr.mem)


# Replace missing conditions (arms, instituions and military aid) with zeros
reg.all.data.min[is.na(reg.all.data.min)] <- 0



### transform data into matrices for STAN
# State-level characeristics
reg.state.mat.min <- as.matrix(reg.state.comp.min[, 4:11])

# State membership in alliances
# pull the alliance-level regressors into a matrix
alliance.reg.mat.min <- as.matrix(reg.all.data.min[, 2: ncol(reg.all.data.min)])

# Add a constant to the alliance-level regression 
cons.min <- rep(1, length = nrow(alliance.reg.mat.min))
alliance.reg.mat.min <- cbind(cons.min, alliance.reg.mat.min)


# run model on the full sample
# Define the data list 
reg.state.comp.min <- as.data.frame(reg.state.comp.min)
stan.data.min <- list(N = nrow(reg.state.comp.min), y = reg.state.comp.min[, 3],
                      state = reg.state.comp.min$state.id, S = length(unique(reg.state.comp.min$state.id)),
                      year = reg.state.comp.min$year.id, T = length(unique(reg.state.comp.min$year.id)),
                      A = ncol(state.mem.min), L = ncol(alliance.reg.mat.min),
                      Z = state.mem.min, 
                      X = alliance.reg.mat.min,
                      W = reg.state.mat.min, M = ncol(reg.state.mat.min)
)


# Regular STAN
system.time(
  ml.model.min <- sampling(model.2, data = stan.data.min, 
                           iter = 2000, warmup = 1000, chains = 4
  )
)

# diagnose model
launch_shinystan(ml.model.min)
check_hmc_diagnostics(ml.model.min)



### Compare the results
# Summarize intervals for major powers
beta.summary.maj <- summary(ml.model.maj, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.maj <- beta.summary.maj[, -2]
rownames(beta.summary.maj) <- c("Constant", "Latent Str.", 
                                "Number Members","Democratic Membership", 
                                "Wartime", "Asymmetric",
                                "US Member", "USSR Member", "sigma Alliances")


print(beta.summary.maj)
xtable(beta.summary.maj, digits = 3)

# summarize intervals for minor powers
beta.summary.min <- summary(ml.model.min, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.min <- beta.summary.min[, -2]
rownames(beta.summary.min) <- c("Constant", "Latent Str.", 
                                "Number Members","Democratic Membership", 
                                "Wartime", "Asymmetric",
                                "US Member", "USSR Member", "sigma Alliances")

print(beta.summary.min)
xtable(beta.summary.min, digits = 3)



# Create an object with all three estimates and plot (Gelman's Secret Weapon)
lscoef.summary <- rbind(beta.summary[2, ], beta.summary.maj[2, ], beta.summary.min[2, ])
row.names(lscoef.summary) <- c("Full Sample", "Major Powers", "Minor Powers")
lscoef.summary <- as.data.frame(lscoef.summary)

ggplot(lscoef.summary, aes(x = row.names(lscoef.summary), y = mean)) +
  geom_errorbar(aes(ymin = `5%`, 
                    ymax = `95%`,
                    width=.05), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) + geom_hline(yintercept = 0) +
  theme_classic() + labs(x = "Sample", y = "Effect of Treaty Strength") +
  coord_flip()


# compare trends in lambdas across treaty strength in major and minor

# Start with major powers
lambda.means.maj <- get_posterior_mean(ml.model.maj, pars = "lambda")
lambda.df.maj <- data_frame(lambda = lambda.means.maj[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.maj)))

# plot major powers
lambda.str.maj <- ggplot(lambda.df.maj, aes(x = latent.str.mean, y = lambda)) +
                  geom_point() + 
                  geom_smooth() + theme_classic() +
  labs(x = "Latent Treaty Strength", y = "Effect of Allied Spending") +
  ggtitle("Major Powers")
lambda.str.maj

# Quick comparison of US and USSR
summary(subset(reg.all.data.maj, us.mem == 1)$latent.str.mean) # USA post-45
summary(subset(reg.all.data.maj, ussr.mem == 1)$latent.str.mean) # USSR post-45


# Non-major powers
lambda.means.min <- get_posterior_mean(ml.model.min, pars = "lambda")
lambda.df.min <- data_frame(lambda = lambda.means.min[, 5]) %>%  # add lambdas to df
  bind_cols(filter(alliance.char, atopid %in% colnames(state.mem.min)))

# plot non-major powers
lambda.str.min <- ggplot(lambda.df.min, aes(x = latent.str.mean, y = lambda)) +
                  geom_point() + 
                  geom_smooth() + theme_classic() +
            labs(x = "Latent Treaty Strength", y = "Effect of Allied Spending") +
            ggtitle("Non-Major Powers")
lambda.str.min

# Compare all in one plot
# Define multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/ 
multiplot.ggplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Apply the function to major and non-major power plots  
multiplot.ggplot(lambda.str.maj, lambda.str.min)


# Compare lambdas for alliances with major and minor power members
lambda.df.mix <- lambda.df.min %>%
  select(atopid, lambda, latent.str.mean) %>%
  left_join(select(lambda.df.maj, atopid, lambda, latent.str.mean), 
            by = c("atopid", "latent.str.mean")) %>% 
     rename(
     lambda.min = lambda.x,
     lambda.maj = lambda.y
     ) %>%
  filter(complete.cases(atopid, latent.str.mean, lambda.min, lambda.maj)) %>%
  mutate(
    lambda.diff = abs(lambda.min- lambda.maj) # this isn't working
  )
lambda.df.mix <- as.data.frame(lambda.df.mix)
lambda.df.mix <- melt(lambda.df.mix, 
                id = c("atopid", "latent.str.mean", "lambda.diff"))

# plot
ggplot(lambda.df.mix, aes(x = atopid, y = value, colour = variable)) + 
  geom_point(aes(size = latent.str.mean)) + 
  labs(x = 'ATOPID', y = "Effect of Allied Spending") 



# More detailed posterior inference and comparisons
# Extract coefficients from the major-power model
sum.maj.post <- extract(ml.model.maj, pars = c("beta", "gamma", "lambda"),
                        permuted = TRUE) # major power

# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.maj <- apply(sum.maj.post$lambda, 2, positive.check)
lambda.probs.maj <- cbind.data.frame(reg.all.data.maj$atopid, round(lambda.df.maj$lambda, digits = 4), lambda.df.maj$latent.str.mean, lambda.probs.maj)
colnames(lambda.probs.maj) <- c("atopid", "lambda.mean", "latent.str.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.maj$non.zero <- ifelse(lambda.probs.maj$pos.post.prob >= .90 | lambda.probs.maj$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.maj$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.maj$atopid <- reorder(lambda.probs.maj$atopid, lambda.probs.maj$pos.post.prob)

# For all alliances
ggplot(lambda.probs.maj, aes(x = atopid, y = pos.post.prob, fill = latent.str.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.maj %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, latent.str.mean)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()





# Extract coefficients from non-major power model
sum.min.post <- extract(ml.model.min, pars = c("beta", "gamma", "lambda"),
                        permuted = TRUE) # non-major power

# Check how many lambda parameters can be reliably distinguished from zero:
lambda.probs.min <- apply(sum.min.post$lambda, 2, positive.check)
lambda.probs.min <- cbind.data.frame(reg.all.data.min$atopid, round(lambda.df.min$lambda, digits = 4), lambda.df.min$latent.str.mean, lambda.probs.min)
colnames(lambda.probs.min) <- c("atopid", "lambda.mean", "latent.str.mean", "pos.post.prob")
# binary indicator if posterior probability is greater than 90% for positive or negative
lambda.probs.min$non.zero <- ifelse(lambda.probs.min$pos.post.prob >= .90 | lambda.probs.min$pos.post.prob <= .10, 1, 0)
sum(lambda.probs.min$non.zero) # total number of non-zero alliances

# Plot posterior probabilities
lambda.probs.min$atopid <- reorder(lambda.probs.min$atopid, lambda.probs.min$pos.post.prob)

# For all alliances
ggplot(lambda.probs.min, aes(x = atopid, y = pos.post.prob, fill = latent.str.mean)) + 
  geom_col() +
  coord_flip()

# For non-zero alliances 
lambda.probs.min %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = pos.post.prob, latent.str.mean)) + 
  geom_col() +
  scale_fill_brewer(palette = "Greys") +
  geom_text(aes(label = pos.post.prob), nudge_y = .04) +
  coord_flip()
