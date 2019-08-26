# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(MASS)
library(plm)
library(dplyr)
library(ggplot2)
library(margins)
library(interflex)


# Set seed
set.seed(12)

# Environment is determined by use of projects and/or using this file in conjunction with
# the file dataset construction and summary.R 

# The full dataset can provide the basis of a state-year characteristics dataset 
# with some summary variables for the alliance portfolio
# Can then merge this with the state characteristics dataset to create a dataset for 
# single-level regressions
state.char.full <- state.ally.year %>%
  group_by(ccode, year) %>%
  summarize(
    treaty.count = n(),
    total.ally.expend = sum(ally.spend[defense == 1 | offense == 1], na.rm = TRUE),
    avg.treaty.contrib = mean(alliance.contrib[defense == 1 | offense == 1], na.rm = TRUE),
    
    uncond.milsup.pres = max(uncond.milsup, na.rm = TRUE),
    uncond.milsup.expend = sum(ally.spend[uncond.milsup == 1], na.rm = TRUE),
    
    avg.depth.full = mean(latent.depth.mean, na.rm = TRUE),
    avg.depth = mean(latent.depth.mean[defense == 1 | offense == 1], na.rm = TRUE),
    avg.dem.prop = mean(avg.democ, na.rm = TRUE),
    armred.rc = max(armred.rc, na.rm = TRUE),
    avg.num.mem = mean(num.mem, na.rm = TRUE),

    defense.pres = max(defense, na.rm = TRUE),
    offense.pres = max(offense, na.rm = TRUE),
    
    econagg.pres = max(econagg.dum)
  ) %>%
  mutate(
    treaty.pres = ifelse((defense.pres == 1 | offense.pres == 1), 1, 0),
    cond.milsup.pres = ifelse((defense.pres == 1 | offense.pres == 1) & uncond.milsup.pres == 0, 1, 0),
    ln.ally.expend = log(total.ally.expend + 1),
    lag.ally.expend = lag(ln.ally.expend),
    diff.ally.expend = ln.ally.expend - lag.ally.expend
  )

# State-year characteristics including alliance portfolio summaries
state.char.full <- left_join(state.vars, state.char.full)

# Fill missing values of alliance variables with zero
state.char.full[, 44: ncol(state.char.full)][is.na(state.char.full[, 44: ncol(state.char.full)])] <- 0

state.char.full <- state.char.full %>%
                  mutate(
                    no.depth = ifelse(avg.depth == 0, 1, 0),
                    high.avg.depth = ifelse(avg.depth >= median(avg.depth[avg.depth != 0]) & no.depth == 0, 1, 0),
                    low.avg.depth = ifelse(avg.depth < median(avg.depth[avg.depth != 0]) & no.depth == 0, 1, 0)
                          )

# Check sums of expenditure and capability
summary(state.char.full$total.ally.expend)

state.char.full <- state.char.full[complete.cases(state.char.full$ccode), ]
state.char.full <- unique(state.char.full) 
# Check for duplicate ccode year pairs
duplicates <- state.char.full %>% group_by(ccode, year) %>% filter(n() > 1)
nrow(duplicates)
rm(duplicates)



# Analysis
# IV: avg alliance depth 
summary(state.char.full$avg.depth)
hist(state.char.full$avg.depth)
summary(state.char.full$avg.depth.full)


# Start with a simple linear regression: presence of scope
m1.all <- lm(growth.milex ~ high.avg.depth + low.avg.depth +
               econagg.pres +
            atwar + civilwar.part + polity + gdp.growth + majpower +
            lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
          data = state.char.full
          )
summary(m1.all)
qqnorm(m1.all$residuals)
qqline(m1.all$residuals)



# Add state and year fixed effects 
m1.reg.fe <- plm(growth.milex ~ high.avg.depth + low.avg.depth +
                   econagg.pres +
                   atwar + civilwar.part + polity + gdp.growth + majpower +
                   lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                index = c("ccode", "year"),
                effect = "individual", # unrestricted error covariance
                data = state.char.full,
                model = "within")

summary(m1.reg.fe)



# Robust Regression 
###### 
# Residuals in the above have extremely heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient
m1r.reg <- rlm(growth.milex ~ high.avg.depth + low.avg.depth +
                 econagg.pres + 
                 atwar + civilwar.part + polity + gdp.growth + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
               data = state.char.full)

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)


# subset by major and minor powers
# Major powers
rreg.maj <- rlm(growth.milex ~ high.avg.depth + low.avg.depth +
                    econagg.pres + 
                     atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 1))

summary(rreg.maj)
plot(rreg.maj$residuals, rreg.maj$w)

# minor powers
rreg.min <- rlm(growth.milex ~ high.avg.depth + low.avg.depth +
                  econagg.pres + 
                     atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 0))

summary(rreg.min)
plot(rreg.min$residuals, rreg.min$w)



# Interact major power indicator with depth
rreg.int <- rlm(growth.milex ~ as.factor(high.avg.depth) + as.factor(majpower) + as.factor(high.avg.depth):as.factor(majpower) + 
                  low.avg.depth + econagg.pres +
                 atwar + civilwar.part + polity + gdp.growth + 
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
               data = state.char.full)

summary(rreg.int)
plot(rreg.int$residuals, rreg.int$w)

# Calculate marginal effects
margins(rreg.int)
cplot(rreg.int, x = "majpower", dx = "high.avg.depth", what = "effect",
      main = "Average Marginal Effect of Treaty Depth For Major and Non-Major Powers",
      factor.lty = 0L, rug = FALSE, xvals = c(0, 1))
abline(h = 0)




# subset by year: before and after 1945
# before 1945
rreg.pre45 <- rlm(growth.milex ~ high.avg.depth + low.avg.depth +
                  econagg.pres +
                  atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                  lsthreat + avg.num.mem + avg.dem.prop,
                data = state.char.full, subset = (year <= 1945))

summary(rreg.pre45)
plot(rreg.pre45$residuals, rreg.pre45$w)

# after 1945
rreg.post45 <- rlm(growth.milex ~ high.avg.depth + low.avg.depth +
                  econagg.pres + 
                  atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                  lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                data = state.char.full, subset = (year > 1945))

summary(rreg.post45)
plot(rreg.post45$residuals, rreg.post45$w)




### Another option is to consider how total allied capability is modified by presence of economic agreement
# and to look at whether that conditional relationship shifts between major and non-major powers


# Robust regression: major
rreg.ex.maj <- rlm(growth.milex ~ as.factor(high.avg.depth) + ln.ally.expend + as.factor(high.avg.depth):ln.ally.expend +
                     low.avg.depth + econagg.pres +
                      atwar + civilwar.part + polity + gdp.growth + 
                      lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                    data = state.char.full, subset = (majpower == 1))

summary(rreg.ex.maj)

# Calculate marginal effects
margins(rreg.ex.maj)
cplot(rreg.ex.maj, x = "high.avg.depth", dx = "ln.ally.expend", what = "effect",
      main = "Average Marginal Effect of Allied Military Capability")
abline(h = 0)


# Robust regression: minor
rreg.ex.min <- rlm(growth.milex ~ as.factor(high.avg.depth) + ln.ally.expend + as.factor(high.avg.depth):ln.ally.expend +
                     low.avg.depth + econagg.pres +
                     atwar + civilwar.part + polity + gdp.growth + 
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 0))

summary(rreg.ex.min)

# Calculate marginal effects
margins(rreg.ex.min)
cplot(rreg.ex.min, x = "high.avg.depth", dx = "ln.ally.expend", what = "effect",
      main = "Average Marginal Effect of Allied Military Capability")
abline(h = 0)




# Average Treaty contribution as another proxy for size
# filter out cases with no alliances
inter.data.rel <- filter(state.char.full, treaty.pres == 1)
inter.data.rel <- as.data.frame(inter.data.rel)

# Robust regression: average relative contribution
m1.all.irel <- rlm(growth.milex ~ avg.depth + avg.treaty.contrib + avg.depth:avg.treaty.contrib + 
                     lag.ln.milex +
                     atwar + civilwar.part + polity  + majpower + ln.gdp +
                     lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                   data = inter.data.rel
)
summary(m1.all.irel)

# Calculate marginal effects
margins(m1.all.irel)
cplot(m1.all.irel, x = "avg.treaty.contrib", dx = "avg.depth", what = "effect",
      main = "Average Marginal Effect of Treaty Depth")
abline(h = 0)

cplot(m1.all.irel, x = "avg.depth", dx = "avg.treaty.contrib", what = "effect",
      main = "Average Marginal Effect of Treaty Contribution")
abline(h = 0)


# Interflex check
bin.rel <- inter.binning(Y = "change.ln.milex", D = "avg.depth", X = "avg.treaty.contrib", 
                         Z = c("lag.ln.milex", "atwar", "civilwar.part", "polity",
                               "majpower", "lsthreat", "cold.war", "avg.num.mem", 
                               "ln.ally.expend", "avg.dem.prop", "ln.gdp"), 
                         data = inter.data.rel,
                         na.rm = TRUE
)
bin.rel

# Kernel: 
kernel.rel <- inter.kernel(Y = "change.ln.milex", D = "avg.depth", X = "avg.treaty.contrib", 
                           Z = c("lag.ln.milex", "atwar", "civilwar.part", "polity",
                                 "majpower", "lsthreat", "cold.war", "avg.num.mem", 
                                 "ln.ally.expend", "avg.dem.prop", "ln.gdp"), 
                           data = inter.data.rel,
                           na.rm = TRUE,
                           nboots = 200, parallel = TRUE, cores = 4
)
kernel.rel


# GJRM approach
# Model process that produces econ agg alliances and process linking scope to growth in spending

# TODO(JOSH): improve model of high scope in alliances: check need for instrument

alliance.eq <- avg.depth ~ polity + atwar + cold.war + lsthreat + gdp.growth

growth.eq <- growth.milex ~ avg.depth + 
  atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
  lsthreat + cold.war + avg.num.mem + avg.dem.prop

# Major powers: Clayton unrotated has best AIC
joint.model.maj <- gjrm(list(alliance.eq, growth.eq), data = state.char.full,
                        subset = (state.char.full$majpower == 1),
                    margins = c("N", "N"),
                    Model = "B",
                    BivD = "G0")
conv.check(joint.model.maj)
AIC(joint.model.maj)
summary(joint.model.maj)

# Non-major powers: C270 copula provides best fit
joint.model.min <- gjrm(list(alliance.eq, growth.eq), data = state.char.full,
                        subset = (state.char.full$majpower == 0),
                        margins = c("N", "N"),
                        Model = "B",
                        BivD = "HO")
conv.check(joint.model.min)
AIC(joint.model.min)
summary(joint.model.min)



# Remove all these regressions from environment
rm(list = c("m1r.reg", "rreg.ex", "rreg.maj", "rreg.min", "m1.all", "m1.all.iabs", "m1.all.irel",
            "m1.reg.fe", "m1.fgls", "reg.ex.gls", "reg.ex", "rreg.pre45", "rreg.post45", "rreg.int"))

