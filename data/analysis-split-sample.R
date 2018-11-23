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
  select(atopid, uncond.milsup, offense, num.mem, 
         dem_prop, wartime, organ1, milaid.rc, asymm, us.mem, ussr.mem, base)


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
  select(atopid, uncond.milsup, offense, num.mem, 
         dem_prop, wartime, organ1, milaid.rc, asymm, us.mem, ussr.mem, base)


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
rownames(beta.summary.maj) <- c("Constant", "Uncond. Mil. Supp.", "Offense", 
                                "Number Members","Democratic Membership", 
                                "Wartime", "IO Form.", "Military Aid", "Asymmetric",
                                "US Member", "USSR Member", "Bases", "sigma Alliances")

print(beta.summary.maj)
xtable(beta.summary.maj, digits = 3)

# summarize intervals for minor powers
beta.summary.min <- summary(ml.model.min, pars = c("beta", "sigma_all"), probs = c(0.05, 0.95))$summary
beta.summary.min <- beta.summary.min[, -2]
rownames(beta.summary.min) <- c("Constant", "Uncond. Mil. Supp.", "Offense", 
                                "Number Members","Democratic Membership", 
                                "Wartime", "IO Form.", "Military Aid", "Asymmetric",
                                "US Member", "USSR Member", "Bases", "sigma Alliances")

print(beta.summary.min)
xtable(beta.summary.min, digits = 3)



# Create an object with all three estimates and plot (Gelman's Secret Weapon)
uncond.summary <- rbind(beta.summary[2, ], beta.summary.maj[2, ], beta.summary.min[2, ])
row.names(uncond.summary) <- c("Full Sample", "Major Powers", "Minor Powers")
uncond.summary <- as.data.frame(uncond.summary)

ggplot(uncond.summary, aes(x = row.names(uncond.summary), y = mean)) +
  geom_errorbar(aes(ymin = `5%`, 
                    ymax = `95%`,
                    width=.05), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) + geom_hline(yintercept = 0) +
  theme_classic() + labs(x = "Sample", y = "Effect of Unconditional Military Support") +
  coord_flip()
