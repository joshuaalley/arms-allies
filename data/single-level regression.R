# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(MASS)
library(plm)
library(dplyr)
library(ggplot2)
library(texreg)
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
    total.ally.expend = sum(ally.spend, na.rm = TRUE),
    avg.treaty.contrib = mean(alliance.contrib, na.rm = TRUE),
    
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
m1.all <- lm(ln.milex ~ uncond.milsup.pres + lag.ln.milex +
            atwar + civilwar.part + polity + ln.gdp + majpower +
            lsthreat + cold.war + avg.num.mem + ln.ally.expend,
          data = state.char.full
          )
summary(m1.all)



# FGLS: "random effects" robust to intragroup heteroskedasticity and serial correlation 
m1.fgls <- pggls(ln.milex ~ uncond.milsup.pres + lag.ln.milex +
                   atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend,
         data = state.char.full,
         index = c("ccode", "year"),
         effect = "individual", # unrestricted error covariance
         model = "pooling")
summary(m1.fgls)


# Add state and year fixed effects and estimate in differences (otherwise Nickell bias applies)
m1.reg.fe <- plm(change.ln.milex ~ uncond.milsup.pres + 
                   atwar + civilwar.part + polity + ln.gdp + majpower +
                   lsthreat + cold.war + avg.num.mem + ln.ally.expend,
                index = c("ccode", "year"),
                effect = "individual", # unrestricted error covariance
                data = state.char.full,
                model = "within")

summary(m1.reg.fe)
plot(density(m1.reg.fe$residuals))



# Robust Regression 
###### 
# Residuals in the above have extremely heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient
m1r.reg <- rlm(ln.milex ~ ln.milex ~ uncond.milsup.pres + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend,
               data = state.char.full)

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)

plotreg(m1r.reg, omit.coef = "(Intercept)|(lag.ln.milex)")






### Another option is to consider the role of the total capabilities aggregated by each alliance type
# This is a crude approximation of the multilevel model with capability in the membership matrix

# Standard regression: complete pooling
reg.ex <- lm(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
               atwar + civilwar.part + polity + ln.gdp + majpower +
               lsthreat + cold.war + avg.num.mem + ln.ally.expend,
             data = state.char.full
)
summary(reg.ex)


# Use fgls to acocunt for autoregressive component of the errors
reg.ex.gls <- pggls(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
                      atwar + civilwar.part + polity + ln.gdp + majpower +
                      lsthreat + cold.war + avg.num.mem + ln.ally.expend,
              data = state.char.full,
              index = c("ccode", "year"),
              effect = "individual", # unrestricted error covariance
              model = "pooling")
summary(reg.ex.gls)
plot(density(reg.ex.gls$residuals))


# Use fixed effects
reg.ex.re <- plm(change.ln.milex ~ uncond.milsup.expend + 
                   atwar + civilwar.part + polity + ln.gdp + majpower +
                   lsthreat + cold.war + avg.num.mem + ln.ally.expend,
                   data = state.char.full,
                   index = c("ccode", "year"),
                   effect = "individual", # unrestricted error covariance
                   model = "within")
summary(reg.ex.re)
plot(density(reg.ex.re$residuals))



# Robust regression
rreg.ex <- rlm(ln.milex ~ uncond.milsup.expend + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend,
               data = state.char.full)

summary(rreg.ex)
plot(rreg.ex$residuals, rreg.ex$w)
plotreg(rreg.ex, omit.coef = "(Intercept)|(lag.ln.milex)")




