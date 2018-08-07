# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(MASS)
library(plm)
library(dplyr)
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
state.ally.year <- full.data.rnonagg %>%
  group_by(ccode, year) %>%
  summarize(
    treaty.count = n(),
    total.ally.expend = sum(total.expend, na.rm = TRUE),
    total.ally.cap = sum(total.cap, na.rm = TRUE),
    cond.expend = sum(total.expend[conditional == 1], na.rm = TRUE),
    uncond.expend = sum(total.expend[unconditional == 1], na.rm = TRUE),
    prob.det.expend = sum(total.expend[prob_det == 1], na.rm = TRUE),
    comp.expend = sum(total.expend[compellent == 1], na.rm = TRUE),
    mixed.expend = sum(total.expend[mixed == 1], na.rm = TRUE),
    
    prob.det.pres = max(prob_det, na.rm = TRUE),
    prob.det.total = sum(prob_det, na.rm = TRUE),
    consul.only.pres = max(onlyconsul, na.rm = TRUE),
    consul.only.total = sum(onlyconsul, na.rm = TRUE),
    bilat.total = sum(bilat, na.rm = TRUE),
    uncond.comp.pres = max(uncond_comp, na.rm = TRUE),
    uncond.comp.total = sum(uncond_comp, na.rm = TRUE),
    cond.comp.pres = max(cond_comp, na.rm = TRUE),
    cond.comp.total = sum(cond_comp, na.rm = TRUE),
    uncond.det.pres = max(uncond_det, na.rm = TRUE),
    uncond.det.total = sum(uncond_det, na.rm = TRUE),
    cond.det.pres = max(cond_det, na.rm = TRUE),
    cond.det.total = sum(cond_det, na.rm = TRUE),
    avg.dem.prop = mean(dem.prop, na.rm = TRUE),
    armred.rc = max(armred.rc, na.rm = TRUE),
    avg.num.mem = mean(num.mem, na.rm = TRUE),
    defense.total = sum(defense, na.rm = TRUE),
    offense.total = sum(offense, na.rm = TRUE),
    discret.inter.pres = max(discret_intervene, na.rm = TRUE),
    discret.inter.total = sum(discret_intervene, na.rm = TRUE),
    discret.mils.pres = max(discret_milsupport, na.rm = TRUE),
    discret.mils.total = sum(discret_milsupport, na.rm = TRUE),
    
    new.prob.det5 = max(new.prob.det5, na.rm = TRUE),
    new.conditional5 = max(new.conditional5, na.rm = TRUE),
    new.unconditional5 = max(new.unconditional5, na.rm = TRUE), 
    new.compellent5 = max(new.compellent5, na.rm = TRUE),
    
    new.prob.det10 = max(new.prob.det10, na.rm = TRUE),
    new.conditional10 = max(new.conditional10, na.rm = TRUE),
    new.unconditional10 = max(new.unconditional10, na.rm = TRUE), 
    new.compellent10 = max(new.compellent10, na.rm = TRUE),
    
    mixed.pres = max(mixed, na.rm = TRUE),
    mixed.total = sum(mixed, na.rm = TRUE)
  )

# State-year characteristics including alliance portfolio summaries
state.char.full <- left_join(state.char, state.ally.year)

# Check sums of expenditure and capability
summary(state.char.full$total.ally.expend)
summary(state.char.full$total.ally.cap)

# Compare new alliance variables with presence variables
summary(state.char.full$prob.det.pres)
summary(state.char.full$new.prob.det5)
summary(state.char.full$new.prob.det10)

# Create a couple new variables: shares of each treaty type
state.char.full <- state.char.full %>%
  mutate(
    treaty.count = treaty.count - 1,
    prob.det.share = prob.det.total / treaty.count,
    cond.det.share = cond.det.total / treaty.count, 
    comp.share = (uncond.comp.total + cond.comp.total) / treaty.count,
    uncond.det.share = uncond.det.total / treaty.count, 
    comp.pres = ifelse((uncond.comp.pres == 1 | cond.comp.pres == 1), 1 , 0),
    ln.rival.mil = log(rival.mil),
    consul.only.share = consul.only.total / treaty.count,
    offense.pres = ifelse((offense.total >= 1), 1 , 0),
    offense.share = offense.total / treaty.count, 
    defense.pres = ifelse((defense.total >= 1), 1 , 0),
    defense.share = defense.total / treaty.count,
    discret.inter.share = discret.inter.total / treaty.count,
    discret.mils.share = discret.mils.total / treaty.count,
    mixed.share = mixed.total / treaty.count
  )

state.char.full <- state.char.full[complete.cases(state.char.full$ccode), ]

# Subset data with nonmajor powers
state.char.nonmaj <- filter(state.char.full, majpower == 0)

### Look at states that are in each type of alliance
library(countrycode)
state.char.full$country.name <- countrycode(state.char.full$ccode, "cown", "country.name")


unconditional.members <- state.char.full %>% 
  filter(uncond.det.pres == 1 & majpower == 0) %>%
  group_by(country.name) %>% 
  summarize(
    n = n()
  )
View(unconditional.members)
# Probabilistic deterrent alliance members  
prob.det.members <- state.char.full %>% 
  filter(prob.det.pres == 1 & majpower == 0) %>%
  group_by(country.name) %>% 
  summarize(
    n = n()
  )
View(prob.det.members)
# Conditional deterrent alliance members
cond.det.members <- state.char.full %>% 
  filter(cond.det.pres == 1 & majpower == 0) %>%
  group_by(country.name) %>% 
  summarize(
    n = n()
  )
View(cond.det.members)

# IV1: Probabilistic Alliances
summary(state.char.nonmaj$prob.det.pres)
summary(state.char.nonmaj$prob.det.total)
# Differences in expenditure
t.test(ln.milex ~ prob.det.pres, alternative = "less", data = state.char.nonmaj)
t.test(change.ln.milex ~ prob.det.pres, alternative = "less", data = state.char.nonmaj)


# IV2: Unconditional alliances
summary(state.char.nonmaj$uncond.det.pres)
summary(state.char.nonamj$uncond.det.total)
# Differences in expenditure
t.test(ln.milex ~ uncond.det.pres, alternative = "less", data = state.char.nonmaj)
t.test(change.ln.milex ~ uncond.det.pres, alternative = "less", data = state.char.nonmaj)

# Sample is non-major powers only, and those states that have at least one alliances
# otherwise, states with no alliances enter the base category, which may alter comparisons



# Start with a simple linear model 
m1 <- plm(ln.milex ~ uncond.det.pres + comp.pres + lag.ln.milex +
           atwar + civilwar + polity + ln.GDP + avg.num.mem + disputes +
           ls.threatenv + cold.war + total.ally.expend,
         data = state.char.full, subset = (majpower == 0),
         model = "pooling")
plmtest(m1, effect = "twoways", type = "ghm")
summary(m1)


# FGLS: "random effects" robust to intragroup heteroskedasticity and serial correlation 
m1.fgls <- pggls(ln.milex ~ uncond.det.pres + comp.pres + lag.ln.milex +
           atwar + civilwar + polity + ln.GDP + avg.num.mem + disputes +
           ls.threatenv + cold.war + total.ally.expend,
         data = state.char.full, subset = (majpower == 0),
         effect = "individual", # unrestricted error covariance
         model = "pooling")
summary(m1.fgls)


# Add state and year fixed effects and estimate in differences (otherwise Nickell bias applies)
m1.reg.fe <- pggls(change.ln.milex ~ uncond.det.pres + comp.pres +
                  atwar + civilwar + polity + ln.GDP + avg.num.mem + disputes +
                  ls.threatenv + cold.war + total.ally.expend,
                index = c("ccode"),
                effect = "individual", # unrestricted error covariance
                data = state.char.full, subset = (majpower == 0),
                model = "within")

summary(m1.reg.fe)
plot(density(m1.reg.fe$residuals))

# FGLS with changes instead of levels
m1.fgls.change <- pggls(change.ln.milex ~ uncond.det.pres + comp.pres +
                   atwar + civilwar + polity + ln.GDP + avg.num.mem + disputes +
                   ls.threatenv + cold.war + total.ally.expend,
                 data = state.char.full, subset = (majpower == 0),
                 effect = "individual", # unrestricted error covariance
                 model = "pooling")
summary(m1.fgls.change)


# Robust Regression 
###### 
# Residuals in the early models have some really heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient


# Start with binary indicators
m1r.reg <- rlm(ln.milex ~ uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem + borders +
                 ls.threatenv + total.ally.expend,
               data = state.char.full, subset = (majpower == 0))

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)

plotreg(m1r.reg, omit.coef = "(Intercept)|(lag.ln.milex)")





### Test whether new alliances are what matters- this variable only encodes an alliance effect
# during with the first 5 years of an alliance 
m1.reg5 <- pggls(ln.milex ~  new.unconditional5 + new.compellent5 + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem +
                ls.threatenv + cold.war + total.ally.expend,
              data = state.char.full, subset = (majpower == 0),
              effect = "individual", # unrestricted error covariance
              model = "pooling")

summary(m1.reg5)
plot(density(m1.reg5$residuals))


# Robust regression
m1r.reg5 <- rlm(ln.milex ~ new.unconditional5 + new.compellent5 + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem +
                 ls.threatenv + total.ally.expend,
               data = state.char.full, subset = (majpower == 0))

summary(m1r.reg5)
plot(m1r.reg5$residuals, m1r.reg5$w)

plotreg(m1r.reg5, omit.coef = "(Intercept)|(lag.ln.milex)")


# First 10 years 
m1.reg10 <- pggls(ln.milex ~ new.unconditional10 + new.compellent10  + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP + nato +
                  ls.threatenv + cold.war + total.ally.expend,
                data = state.char.full, subset = (majpower == 0),
                effect = "individual", # unrestricted error covariance
                model = "pooling")

summary(m1.reg10)
plot(density(m1.reg10$residuals))

# Robust regression
m1r.reg10 <- rlm(ln.milex ~ new.unconditional10 + new.compellent10 + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP + avg.num.mem +
                  ls.threatenv + total.ally.expend,
                data = state.char.full, subset = (majpower == 0))

summary(m1r.reg10)
plot(m1r.reg10$residuals, m1r.reg10$w)

plotreg(m1r.reg10, omit.coef = "(Intercept)|(lag.ln.milex)")





### Another option is to consider the role of the total capabilities aggregated by each alliance type
# This is a crude approximation of the multilevel model with capability in the membership matrix
reg.ex <- lm(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem +
                ls.threatenv + cold.war,
              data = state.char.full, subset = (majpower == 0))
summary(reg.ex)

# Use fgls to acocunt for autoregressive component of the errors
reg.ex.re <- pggls(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem +
                ls.threatenv + cold.war,
              data = state.char.full, subset = (majpower == 0),
              effect = "individual", # unrestricted error covariance
              model = "pooling")

summary(reg.ex.re)
plot(density(reg.ex.re$residuals))


# Robust regression
rreg.ex <- rlm(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem +
                 ls.threatenv + cold.war,
               data = state.char.full, subset = (majpower == 0))

summary(rreg.ex)
plot(rreg.ex$residuals, rreg.ex$w)

plotreg(rreg.ex, omit.coef = "(Intercept)|(lag.ln.milex)")




# Try to mix robust regression with country and year-specific intercept terms
# Robustlmmm fits linear mixed effects models- could also say random effects

# Estimate model with binary indicators of presence as IVs
system.time(
rb.pres <- rlmer(ln.milex ~ uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
                   atwar + civilwar + polity + ln.GDP + avg.num.mem +
                   ls.threatenv + cold.war + total.ally.expend + (1|ccode) + (1|year),
                  state.char.nonmaj, verbose = 2)
)

summary(rb.pres)
plot(rb.pres)

# Re-tune the presence fit to improve efficiency
system.time(
  rb.pres.update <- update(rb.pres, rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                           rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
)
summary(rb.pres.update)

# Compare initial and tuned presence fits
compare(rb.pres, rb.pres.update, show.rho.functions = FALSE)



# estimate model with total expenditure of allies from each pact
system.time(
  rb.expend <- rlmer(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend + mixed.expend + avg.dem.prop + lag.ln.milex +
                      atwar + civilwar + polity + ln.GDP + avg.num.mem +
                      ls.threatenv + cold.war + (1|ccode) + (1|year),
                    state.char.nonmaj, verbose = 2)
)

summary(rb.expend)
plot(rb.expend)

# re-tune the expenditure fit
system.time(
  rb.expend.update <- update(rb.expend, rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                            rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
)
summary(rb.expend.update)


# Compare the initial and tuned expenditure fits
compare(rb.expend, rb.expend.update, show.rho.functions = FALSE)






