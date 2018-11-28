# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(MASS)
library(plm)
library(dplyr)
library(ggplot2)
library(robustlmm)



# Set working directory to current folder 
setwd(here::here())
getwd()

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
    
    avg.dem.prop = mean(avg.democ, na.rm = TRUE),
    armred.rc = max(armred.rc, na.rm = TRUE),
    avg.num.mem = mean(num.mem, na.rm = TRUE),

    defense.pres = max(defense, na.rm = TRUE),
    offense.pres = max(offense, na.rm = TRUE)
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
state.char.full[, 36: ncol(state.char.full)][is.na(state.char.full[, 36: ncol(state.char.full)])] <- 0


# Check sums of expenditure and capability
summary(state.char.full$total.ally.expend)

state.char.full <- state.char.full[complete.cases(state.char.full$ccode), ]
state.char.full <- unique(state.char.full) 
# Check for duplicate ccode year pairs
duplicates <- state.char.full %>% group_by(ccode, year) %>% filter(n() > 1)
nrow(duplicates)
rm(duplicates)



# Analysis
# IV: Unconditional military support
summary(state.char.full$uncond.milsup.pres)
# Differences in expenditure
t.test(ln.milex ~ uncond.milsup.pres, data = state.char.full)
t.test(change.ln.milex ~ uncond.milsup.pres, data = state.char.full)


# Start with a simple linear regression: presence of unconditional support
m1.all <- lm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
            atwar + civilwar.part + polity + ln.gdp + majpower +
            lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
          data = state.char.full
          )
summary(m1.all)
qqnorm(m1.all$residuals)
qqline(m1.all$residuals)


# FGLS: "random effects" robust to intragroup heteroskedasticity and serial correlation 
m1.fgls <- pggls(ln.milex ~ uncond.milsup.pres + lag.ln.milex +
                   atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
         data = state.char.full,
         index = c("ccode", "year"),
         effect = "individual", # unrestricted error covariance
         model = "pooling")
summary(m1.fgls)


# Add state and year fixed effects and estimate in differences (otherwise Nickell bias applies)
m1.reg.fe <- plm(change.ln.milex ~ uncond.milsup.pres + 
                   atwar + civilwar.part + polity + ln.gdp + majpower +
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
m1r.reg <- rlm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
               data = state.char.full)

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)


# subset by major and minor powers
# Major powers
summary(subset(state.char.full, majpower == 1, select = uncond.milsup.pres))
rreg.maj <- rlm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
                     atwar + civilwar.part + polity + ln.gdp + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 1))

summary(rreg.maj)
plot(rreg.maj$residuals, rreg.maj$w)

# minor powers
summary(subset(state.char.full, majpower == 0, select = uncond.milsup.pres))
rreg.min <- rlm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
                     atwar + civilwar.part + polity + ln.gdp + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 0))

summary(rreg.min)
plot(rreg.min$residuals, rreg.min$w)

texreg(l = list(m1r.reg, rreg.maj, rreg.min), ci.force = TRUE)


# subset by year: before and after 1945
# before 1945
rreg.pre45 <- rlm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
                  atwar + civilwar.part + polity + ln.gdp + ln.ally.expend +
                  lsthreat + avg.num.mem + avg.dem.prop,
                data = state.char.full, subset = (year <= 1945))

summary(rreg.pre45)
plot(rreg.pre45$residuals, rreg.pre45$w)

# after 1945
rreg.post45 <- rlm(ln.milex ~ uncond.milsup.pres + cond.milsup.pres + lag.ln.milex +
                  atwar + civilwar.part + polity + ln.gdp + ln.ally.expend +
                  lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                data = state.char.full, subset = (year > 1945))

summary(rreg.post45)
plot(rreg.post45$residuals, rreg.post45$w)




### Another option is to consider the role of the total capabilities aggregated by each alliance type
# This is a crude approximation of the multilevel model with capability in the membership matrix

# Standard regression: complete pooling
reg.ex <- lm(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
               atwar + civilwar.part + polity + ln.gdp + majpower +
               lsthreat + cold.war + avg.num.mem + avg.dem.prop,
             data = state.char.full
)
summary(reg.ex)


# Use fgls to acocunt for autoregressive component of the errors
reg.ex.gls <- pggls(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
                      atwar + civilwar.part + polity + ln.gdp + majpower +
                      lsthreat + cold.war + avg.num.mem + avg.dem.prop,
              data = state.char.full,
              index = c("ccode", "year"),
              effect = "individual", # unrestricted error covariance
              model = "pooling")
summary(reg.ex.gls)
plot(density(reg.ex.gls$residuals))


# Use fixed effects
reg.ex.fe <- plm(change.ln.milex ~ uncond.milsup.expend + 
                   atwar + civilwar.part + polity + ln.gdp + majpower +
                   lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full,
                   index = c("ccode", "year"),
                   effect = "individual", # unrestricted error covariance
                   model = "within")
summary(reg.ex.fe)



# Robust regression
rreg.ex <- rlm(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + avg.dem.prop,
               data = state.char.full)

summary(rreg.ex)
plot(rreg.ex$residuals, rreg.ex$w)



### Is the effect of unconditional military support conditional on ally size? 

# Robust regression: absolute size
m1.all.iabs <- rlm(ln.milex ~ uncond.milsup.pres + ln.gdp + uncond.milsup.pres:ln.gdp + lag.ln.milex +
               atwar + civilwar.part + polity  + majpower +
               lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
             data = state.char.full
)
summary(m1.all.iabs)

# Calculate marginal effects
margins(m1.all.iabs)
cplot(m1.all.iabs, x = "ln.gdp", dx = "uncond.milsup.pres", what = "effect",
      main = "Average Marginal Effect of Unconditional Military Support")
abline(h = 0)


# Check results with interflex- continuous modifying variable
state.char.full <- as.data.frame(state.char.full)
# binning estimator
bin.abs <- inter.binning(Y = "ln.milex", D = "uncond.milsup.pres", X = "ln.gdp", 
                         Z = c("lag.ln.milex", "atwar", "civilwar.part", "polity",
                               "majpower", "lsthreat", "cold.war", "avg.num.mem", 
                               "ln.ally.expend", "avg.dem.prop"), 
                         data = state.char.full, 
                         na.rm = TRUE
)
bin.abs



# Average Treaty contribution as another proxy for size
# filter out cases with no alliances
inter.data.rel <- filter(state.char.full, treaty.pres == 1)
inter.data.rel <- as.data.frame(inter.data.rel)

# Robust regression: average relative contribution
m1.all.irel <- rlm(ln.milex ~ uncond.milsup.pres + avg.treaty.contrib + uncond.milsup.pres:avg.treaty.contrib + lag.ln.milex +
                     atwar + civilwar.part + polity  + majpower + ln.gdp +
                     lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                   data = inter.data.rel
)
summary(m1.all.irel)

# Calculate marginal effects
margins(m1.all.irel)
cplot(m1.all.irel, x = "avg.treaty.contrib", dx = "uncond.milsup.pres", what = "effect",
      main = "Average Marginal Effect of Unconditional Military Support")
abline(h = 0)

# Interflex check
bin.rel <- inter.binning(Y = "ln.milex", D = "uncond.milsup.pres", X = "avg.treaty.contrib", 
                         Z = c("lag.ln.milex", "atwar", "civilwar.part", "polity",
                               "majpower", "lsthreat", "cold.war", "avg.num.mem", 
                               "ln.ally.expend", "avg.dem.prop", "ln.gdp"), 
                         data = inter.data.rel,
                         na.rm = TRUE
)
bin.rel

# Kernel: 10+ minute run time 
kernel.rel <- inter.kernel(Y = "ln.milex", D = "uncond.milsup.pres", X = "avg.treaty.contrib", 
                           Z = c("lag.ln.milex", "atwar", "civilwar.part", "polity",
                                 "majpower", "lsthreat", "cold.war", "avg.num.mem", 
                                 "ln.ally.expend", "avg.dem.prop", "ln.gdp"), 
                           data = inter.data.rel,
                           na.rm = TRUE,
                           nboots = 200, parallel = TRUE, cores = 4
)
kernel.rel



# Remove all the robust regressions
rm(list = c("m1r.reg", "rreg.ex", "rreg.maj", "rreg.min", "m1.all", "m1.all.iabs", "m1.all.irel",
            "m1.reg.fe", "m1.fgls", "reg.ex.gls", "reg.ex", "rreg.pre45", "rreg.post45"))

