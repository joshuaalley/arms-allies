# Joshua Alley
# Texas A&M University
# Compare models with alternative encodings of Z:

# Capability from CINC or normalized spending, or rescaling
# Use loo to compare model fits 


### Normalized spending by year 
# Calculate loo and WAIC
ml.model.min <- readRDS("data/ml-model-min-znorm.rds")
log.lik.min <- extract_log_lik(ml.model.min)
rm("ml.model.min") # remove from workspace to conserve memory 
r.eff <- relative_eff(exp(log.lik.min), chain_id = rep(1:4, each = 1000))
loo.znorm <- loo(log.lik.min, r_eff = r.eff, cores = 2)
print(loo.znorm)
plot(loo.znorm, label_points = TRUE)
waic.znorm <- waic(log.lik.min)

# remove log likelihood from workspace
rm(list = c("log.lik.min", "r.eff"))


### Total allied CINC 
ml.model.min <- readRDS("data/ml-model-min-zcinc.rds")
log.lik.min <- extract_log_lik(ml.model.min)
rm("ml.model.min") # remove from workspace to conserve memory
r.eff <- relative_eff(exp(log.lik.min), chain_id = rep(1:4, each = 1000))
loo.zcinc <- loo(log.lik.min, r_eff = r.eff, cores = 2)
print(loo.zcinc)
plot(loo.zcinc, label_points = TRUE)
waic.zcinc <- waic(log.lik.min)

# remove log likelihood from workspace
rm(list = c("log.lik.min", "r.eff"))


### Total allied spending- rescale 2sd 
ml.model.min <- readRDS("data/ml-model-min-zrescale2sd.rds")
log.lik.min <- extract_log_lik(ml.model.min)
rm("ml.model.min") # remove from workspace to conserve memory 
r.eff <- relative_eff(exp(log.lik.min), chain_id = rep(1:4, each = 1000))
loo.zrescale2sd <- loo(log.lik.min, r_eff = r.eff, cores = 2)
print(loo.zrescale2sd)
plot(loo.zrescale2sd, label_points = TRUE)
waic.zrescale2sd <- waic(log.lik.min)

# remove log likelihood from workspace
rm(list = c("log.lik.min", "r.eff"))


### Total allied spending- rescale 
ml.model.min <- readRDS("data/ml-model-min-zrescale.rds")
log.lik.min <- extract_log_lik(ml.model.min)
rm("ml.model.min") # remove from workspace to conserve memory 
r.eff <- relative_eff(exp(log.lik.min), chain_id = rep(1:4, each = 1000))
loo.zrescale <- loo(log.lik.min, r_eff = r.eff, cores = 2)
print(loo.zrescale)
plot(loo.zrescale, label_points = TRUE)
waic.zrescale <- waic(log.lik.min)

# remove log likelihood from workspace
rm(list = c("log.lik.min", "r.eff"))


#### 
# Compare the three models 
diff <- compare(loo.znorm, loo.zcinc, loo.zrescale2sd, loo.zrescale)
print(diff)
xtable(diff, digits = 3) # for appendix

# Series of pairwise comparisons 
compare(loo.znorm, loo.zcinc)
compare(loo.znorm, loo.zrescale2sd)
compare(loo.zcinc, loo.zrescale2sd)
compare(loo.zrescale, loo.zrescale2sd)
compare(loo.zrescale, loo.zcinc)


# WAIC Comparison 
compare(waic.znorm, waic.zcinc, waic.zrescale2sd, waic.zrescale)
