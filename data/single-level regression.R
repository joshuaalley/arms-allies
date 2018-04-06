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
    armred = max(armred, na.rm = TRUE),
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
    new.compellent10 = max(new.compellent10, na.rm = TRUE)
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
    discret.mils.share = discret.mils.total / treaty.count
  )

state.char.full <- state.char.full[complete.cases(state.char.full$ccode), ]



# Sample is non-major powers only, and those states that have at least one alliances
# otherwise, states with no alliances enter the base category, which may alter comparisons

# Start with a simple linear model 
m1.reg <- plm(ln.milex ~ prob.det.pres + cond.det.pres + uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP + avg.num.mem +
               ls.threatenv + cold.war + total.ally.expend,
             data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
            effect = "twoways",  model = "within")

summary(m1.reg)
plot(density(m1.reg$residuals))

# Hausman test
m1.reg.re <- plm(ln.milex ~ prob.det.pres  + cond.det.pres + uncond.det.pres + comp.pres + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem + 
                ls.threatenv +  cold.war + total.ally.expend,
              data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
              effect = "twoways",  model = "random")
summary(m1.reg.re)

phtest(m1.reg, m1.reg.re, method = "aux")



# Use shares instead
m2.reg <- plm(ln.milex ~ prob.det.share + cond.det.share + uncond.det.share + comp.share + avg.dem.prop + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP + avg.num.mem + 
               ls.threatenv + cold.war + total.ally.expend, 
             data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
             effect = "twoways", model = "within")

summary(m2.reg)
plot(density(m2.reg$residuals))



# Hausman test
m2.reg.re <- plm(ln.milex ~  prob.det.share + cond.det.share + uncond.det.share + comp.share + avg.dem.prop + lag.ln.milex +
                   atwar + civilwar + polity + ln.GDP + avg.num.mem +
                   ls.threatenv + cold.war + total.ally.expend,
                 data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
                 effect = "twoways",  model = "random")
summary(m2.reg.re)

phtest(m2.reg, m2.reg.re, method = "aux")





# Robust Regression 
###### 
# Residuals in the early models have some really heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient
# Had trouble getting fixed effects into these models


# Start with binary indicators
m1r.reg <- rlm(ln.milex ~ prob.det.pres + cond.det.pres + uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem +
                 ls.threatenv + total.ally.expend,
               data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0))

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)

plotreg(m1r.reg, omit.coef = "(Intercept)|(lag.ln.milex)")


# Use shares instead
m2r.reg <- rlm(ln.milex ~ prob.det.share + cond.det.share + uncond.det.share + comp.share  + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem +
                 ls.threatenv + total.ally.expend, 
               data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0))

summary(m2r.reg)
plot(m2r.reg$residuals, m2r.reg$w)

plotreg(m2r.reg, omit.coef = "(Intercept)|(lag.ln.milex)")

# probablistic deterrent effects here are probably driven by the US. 





### Test whether new alliances are what matters- this variable only encodes an alliance effect
# during with the first 5 years of an alliance 
m1.reg5 <- plm(ln.milex ~ new.prob.det5 + new.conditional5 + new.unconditional5 + new.compellent5  + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem +
                ls.threatenv + cold.war + total.ally.expend,
              data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
              effect = "twoways",  model = "within")

summary(m1.reg5)
plot(density(m1.reg5$residuals))


# First 10 years 
m1.reg10 <- plm(ln.milex ~ new.prob.det10 + new.conditional10 + new.unconditional10 + new.compellent10  + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP + nato +
                  ls.threatenv + cold.war + total.ally.expend,
                data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
                effect = "twoways",  model = "within")

summary(m1.reg10)
plot(density(m1.reg10$residuals))





### Another option is to consider the role of the total capabilities aggregated by each alliance type
# This is a crude approximation of the multilevel model with capability in the membership matrix
reg.ex <- plm(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend  + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP + avg.num.mem +
                ls.threatenv + cold.war,
              data = state.char.full, subset = (majpower == 0),
              effect = "twoways",  model = "within")

summary(reg.ex)
plot(density(reg.ex$residuals))

# Hausman test
reg.ex.re <- plm(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend  + avg.dem.prop + lag.ln.milex +
                   atwar + civilwar + polity + ln.GDP + avg.num.mem +
                   ls.threatenv + cold.war,
                 data = state.char.full, subset = (majpower == 0),
                 effect = "twoways",  model = "random")
summary(reg.ex.re)

phtest(reg.ex, reg.ex.re, method = "aux")


# Robust regression
rreg.ex <- rlm(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend  + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP + avg.num.mem +
                 ls.threatenv + cold.war,
               data = state.char.full, subset = (majpower == 0))

summary(rreg.ex)
plot(rreg.ex$residuals, rreg.ex$w)

plotreg(rreg.ex, omit.coef = "(Intercept)|(lag.ln.milex)")




# Try to mix robust regression with country and year-specific intercept terms
# Robustlmmm fits linear mixed effects models- could also say random effects

# need to subset the data beforehand 
state.char.nonmaj <- filter(state.char.full, majpower == 0)

# Estimate model with binary indicators of presence as IVs
system.time(
rb.pres <- rlmer(ln.milex ~ prob.det.pres + cond.det.pres + uncond.det.pres + comp.pres  + avg.dem.prop + lag.ln.milex +
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



# Estimate model with shares as the independent variables
system.time(
  rb.share <- rlmer(ln.milex ~ prob.det.share + cond.det.share + uncond.det.share + comp.share  + avg.dem.prop + lag.ln.milex +
                     atwar + civilwar + polity + ln.GDP + avg.num.mem +
                     ls.threatenv + cold.war + total.ally.expend + (1|ccode) + (1|year),
                   state.char.nonmaj, verbose = 2)
)

summary(rb.share)
plot(rb.share)

# re-tune the shares fit
system.time(
  rb.share.update <- update(rb.share, rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                            rho.sigma.b = psi2propII(smoothPsi, k = 2.28))
)
summary(rb.share.update)

# Compare the initial and tuned shares fits
compare(rb.share, rb.share.update, show.rho.functions = FALSE)



# estimate model with total expenditure of allies from each pact
system.time(
  rb.expend <- rlmer(ln.milex ~ prob.det.expend + cond.expend + uncond.expend + comp.expend  + avg.dem.prop + lag.ln.milex +
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





### Consultation variable: Was Hypothesis 3, but no longer in the paper to simplify presentation of the results

########

# Use consultation variable 
m3.cons <- plm(ln.milex ~ consul.only.pres + offense.pres + defense.pres + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP +
                ls.threatenv + cold.war + total.ally.expend, 
              data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
              effect = c("twoways"))

summary(m3.cons)
plot(density(m3.cons$residuals))



# Use consultation only as a share of total treaties 
m4.cons <- plm(ln.milex ~ consul.only.share + offense.share + defense.share + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP +
                ls.threatenv + cold.war + total.ally.expend, 
              data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0),
              effect = c("twoways"))

summary(m4.cons)
plot(density(m4.cons$residuals))




### robust regression

# Use consultation variable 
m3r.cons <- rlm(ln.milex ~ consul.only.pres + offense.pres + defense.pres + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP +
                  ls.threatenv + total.ally.expend, 
                data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0))

summary(m3r.cons)
plot(m3r.cons$residuals, m3r.cons$w)

plotreg(m3r.cons, omit.coef = "Intercept")


# Use consultation only as a share of total treaties 
m4r.cons <- rlm(ln.milex ~ consul.only.share + offense.share + defense.share + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP +
                  ls.threatenv + total.ally.expend, 
                data = state.char.full, subset = (majpower == 0 & avg.num.mem != 0))

summary(m4r.cons)
plot(m4r.cons$residuals, m4r.cons$w)

plotreg(m4r.cons, omit.coef = "Intercept")




