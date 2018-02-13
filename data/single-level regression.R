# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(dplyr)
library(texreg)
library(MASS)



# Set working directory to current folder 
setwd(here::here())
getwd()

# Environment is determined by use of projects and/or using this file in conjunction with
# the file dataset construction and summary.R 


# Regression is based on data from the state.char.full dataset, which 
# contains state-level variables, including summaries of alliance characteristics

# Create a couple new variables
state.char.full <- state.char.full %>%
  mutate(
    treaty.count = treaty.count - 1,
    prob.det.share = prob.det.total / treaty.count,
    comp.share = (uncond.comp.total + cond.comp.total) / treaty.count,
    uncond.det.share = uncond.det.total / treaty.count, 
    comp.pres = ifelse((uncond.comp.pres == 1 | cond.comp.pres == 1), 1 , 0),
    ln.rival.mil = log(rival.mil),
    consul.only.share = consul.only.total / treaty.count,
    offense.pres = ifelse((offense.total >= 1), 1 , 0),
    offense.share = offense.total / treaty.count, 
    defense.pres = ifelse((defense.total >= 1), 1 , 0),
    defense.share = defense.total / treaty.count
  )



# Create a dataset of the major powers only: this is the COW coding
char.major <- filter(state.char.full, majpower == 1)
View(char.major)

# create a dataset of state with no alliances 
no.ally <- filter(state.char.full, avg.num.mem == 0)
View(no.ally)

# Sample is minor powers only 

# Start with a simple linear model 
m1.reg <- lm(ln.milex ~ prob.det.pres + uncond.det.pres + avg.dem.prop + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP +
               ls.threatenv + comp.pres ,
             data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m1.reg)
plot(density(m1.reg$residuals))
plot(m1.reg)

plotreg(m1.reg, omit.coef = "Intercept")


# Use shares instead
m2.reg <- lm(ln.milex ~ prob.det.share + uncond.det.share + avg.dem.prop + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP +
               ls.threatenv + comp.share, 
             data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m2.reg)
plot(density(m2.reg$residuals))
plot(m2.reg)

plotreg(m2.reg, omit.coef = "Intercept")



# Use consultation variable 
m3.cons <- lm(ln.milex ~ consul.only.pres + offense.pres + defense.pres + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP +
                ls.threatenv, 
              data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m3.cons)
plot(density(m3.cons$residuals))
plot(m3.cons)

plotreg(m3.cons, omit.coef = "Intercept")


# Use consultation only as a share of total treaties 
m4.cons <- lm(ln.milex ~ consul.only.share + offense.share + defense.share + avg.dem.prop + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP +
                ls.threatenv, 
              data = state.char.full, majpower == 0  & avg.num.mem != 0)

summary(m4.cons)
plot(density(m4.cons$residuals))
plot(m4.cons)

plotreg(m4.cons, omit.coef = "Intercept")




# Robust Regression 
###### 
# Residuals in the early models have some really heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient


# Start with a binary indicator of probabilistic deterrent pacts 
m1r.reg <- rlm(ln.milex ~ prob.det.pres + uncond.det.pres + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP +
                 ls.threatenv + comp.pres ,
               data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)

plotreg(m1r.reg, omit.coef = "Intercept")


# Use shares instead
m2r.reg <- rlm(ln.milex ~ prob.det.share + uncond.det.share + avg.dem.prop + lag.ln.milex +
                 atwar + civilwar + polity + ln.GDP +
                 ls.threatenv + comp.share, 
               data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m2r.reg)
plot(m2r.reg$residuals, m2r.reg$w)

plotreg(m2r.reg, omit.coef = "Intercept")



# Use consultation variable 
m3r.cons <- rlm(ln.milex ~ consul.only.pres + offense.pres + defense.pres + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP +
                  ls.threatenv, 
                data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m3r.cons)
plot(m3r.cons$residuals, m3r.cons$w)

plotreg(m3r.cons, omit.coef = "Intercept")


# Use consultation only as a share of total treaties 
m4r.cons <- rlm(ln.milex ~ consul.only.share + offense.share + defense.share + avg.dem.prop + lag.ln.milex +
                  atwar + civilwar + polity + ln.GDP +
                  ls.threatenv, 
                data = state.char.full, majpower == 0 & avg.num.mem != 0)

summary(m4r.cons)
plot(m4r.cons$residuals, m4r.cons$w)

plotreg(m4.cons, omit.coef = "Intercept")





