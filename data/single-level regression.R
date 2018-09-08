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
state.ally.year <- state.ally.year %>%
  group_by(ccode, year) %>%
  summarize(
    treaty.count = n(),
    total.ally.expend = sum(ally.spend, na.rm = TRUE),
    cond.det.expend = sum(ally.spend[cond_det == 1], na.rm = TRUE),
    uncond.det.expend = sum(ally.spend[uncond_det == 1], na.rm = TRUE),
    prob.det.expend = sum(ally.spend[prob_det == 1], na.rm = TRUE),
    uncond.comp.expend = sum(ally.spend[uncond_comp == 1], na.rm = TRUE),
    cond.comp.expend = sum(ally.spend[cond_comp == 1], na.rm = TRUE),
    
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
    
    comp.pres = ifelse((cond.comp.pres == 1 | uncond.comp.pres == 1), 1, 0),
    det.pres = ifelse((uncond.det.pres == 1 | cond.det.pres == 1 | prob.det.pres == 1), 1, 0),
    treaty.pres = ifelse((comp.pres == 1 | det.pres == 1), 1, 0)

  )

# State-year characteristics including alliance portfolio summaries
state.char.full <- left_join(state.vars, state.ally.year)

# Fill missing values of alliance variables with zero
state.char.full[, 28: ncol(state.char.full)][is.na(state.char.full[,28: ncol(state.char.full)])] <- 0


# Check sums of expenditure and capability
summary(state.char.full$total.ally.expend)

state.char.full <- state.char.full[complete.cases(state.char.full$ccode), ]
state.char.full <- unique(state.char.full)
# Check for duplicate ccode year pairs
duplicates <- state.char.full %>% group_by(ccode, year) %>% filter(n() > 1)
View(duplicates)
rm(duplicates)


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


# IV: Unconditional alliances
summary(state.char.nonmaj$uncond.det.pres)
summary(state.char.nonmaj$uncond.det.total)
# Differences in expenditure
t.test(ln.milex ~ uncond.det.pres, alternative = "less", data = state.char.nonmaj)
t.test(change.ln.milex ~ uncond.det.pres, alternative = "less", data = state.char.nonmaj)

# Sample is non-major powers only
# otherwise, states with no alliances enter the base category, which may alter comparisons


# Super blunt comparison: presence of alliance, and presence of deterrent and compellent alliances
# Any alliance: dummy
m1.all <- plm(ln.milex ~ treaty.pres + lag.ln.milex +
            atwar + civilwar.part + polity + ln.gdp + 
            lsthreat + cold.war,
          data = state.char.nonmaj, 
          index = c("ccode", "year"),
          model = "pooling")
summary(m1.all)


# Total allied spending
m1.all.ex <- plm(ln.milex ~ total.ally.expend + lag.ln.milex +
                atwar + civilwar.part + polity + ln.gdp + 
                lsthreat + cold.war,
              data = state.char.nonmaj, 
              index = c("ccode", "year"),
              model = "pooling")
summary(m1.all.ex)



# Deterrent and compellent treaty dummties
m1.cd <- plm(ln.milex ~ det.pres + comp.pres + lag.ln.milex +
            atwar + civilwar.part + polity + ln.gdp +  
            lsthreat + cold.war,
          data = state.char.full, subset = (majpower == 0),
          index = c("ccode", "year"),
          model = "pooling")
summary(m1.cd)



# simple linear model: focus on unconditional deterrent and compellent 
m1 <- plm(ln.milex ~ uncond.det.pres + comp.pres + lag.ln.milex +
           atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
           lsthreat + cold.war + total.ally.expend,
         data = state.char.full, subset = (majpower == 0),
         index = c("ccode", "year"),
         model = "pooling")
plmtest(m1, effect = "twoways", type = "ghm")
summary(m1)


# FGLS: "random effects" robust to intragroup heteroskedasticity and serial correlation 
m1.fgls <- pggls(ln.milex ~ uncond.det.pres + comp.pres + lag.ln.milex +
           atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
           lsthreat + cold.war + total.ally.expend,
         data = state.char.full, subset = (majpower == 0),
         index = c("ccode", "year"),
         effect = "individual", # unrestricted error covariance
         model = "pooling")
summary(m1.fgls)


# Add state and year fixed effects and estimate in differences (otherwise Nickell bias applies)
m1.reg.fe <- pggls(change.ln.milex ~ uncond.det.pres + comp.pres +
                  atwar + civilwar.part + polity + ln.gdp + avg.num.mem + 
                  lsthreat + cold.war + total.ally.expend,
                index = c("ccode", "year"),
                effect = "individual", # unrestricted error covariance
                data = state.char.full, subset = (majpower == 0),
                model = "within")

summary(m1.reg.fe)
plot(density(m1.reg.fe$residuals))

# FGLS with changes instead of levels
m1.fgls.change <- pggls(change.ln.milex ~ uncond.det.pres + comp.pres +
                   atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
                   lsthreat + cold.war + total.ally.expend,
                 data = state.char.full, subset = (majpower == 0),
                 index = c("ccode", "year"),
                 effect = "individual", # unrestricted error covariance
                 model = "pooling")
summary(m1.fgls.change)


# Robust Regression 
###### 
# Residuals in the early models have some really heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient


# Start with binary indicators
m1r.reg <- rlm(ln.milex ~ uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
                 lsthreat + total.ally.expend,
               data = state.char.full, subset = (majpower == 0))

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)

plotreg(m1r.reg, omit.coef = "(Intercept)|(lag.ln.milex)")






### Another option is to consider the role of the total capabilities aggregated by each alliance type
# This is a crude approximation of the multilevel model with capability in the membership matrix
# Use fgls to acocunt for autoregressive component of the errors
reg.ex.re <- pggls(ln.milex ~ prob.det.expend + cond.det.expend + uncond.det.expend +
                  uncond.comp.expend + cond.comp.expend +
                  avg.dem.prop + lag.ln.milex +
                atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
                lsthreat + cold.war,
              data = state.char.full, subset = (majpower == 0),
              index = c("ccode", "year"),
              effect = "individual", # unrestricted error covariance
              model = "pooling")

summary(reg.ex.re)
plot(density(reg.ex.re$residuals))


# Robust regression
rreg.ex <- rlm(ln.milex ~ prob.det.expend + cond.det.expend + uncond.det.expend +
                 uncond.comp.expend + cond.comp.expend +
                 avg.dem.prop + lag.ln.milex +
                 atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
                 lsthreat + cold.war,
               data = state.char.full, subset = (majpower == 0))

summary(rreg.ex)
plot(rreg.ex$residuals, rreg.ex$w)

plotreg(rreg.ex, omit.coef = "(Intercept)|(lag.ln.milex)")




# Try to mix robust regression with country and year-specific intercept terms
# Robustlmmm fits linear mixed effects models- could also say random effects

# Estimate model with binary indicators of presence as IVs
system.time(
rb.pres <- rlmer(ln.milex ~ uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
                   atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
                   lsthreatenv + cold.war + total.ally.expend + (1|ccode) + (1|year),
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
  rb.expend <- rlmer(ln.milex ~ prob.det.expend + cond.det.expend + uncond.det.expend +
                       uncond.comp.expend + cond.comp.expend +
                      avg.dem.prop + lag.ln.milex +
                      atwar + civilwar.part + polity + ln.gdp + avg.num.mem +
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






