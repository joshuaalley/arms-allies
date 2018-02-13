# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Load packages
library(here)
library(dplyr)
library(texreg)



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

# Sample is minor powers only 

# Start with a simple linear model 
m1.reg <- lm(ln.milex ~ prob.det.pres + uncond.det.pres + defense.dem + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP +
               ls.threatenv + comp.pres ,
             data = state.char.full, majpower == 0)

summary(m1.reg)
plot(density(m1.reg$residuals))
plot(m1.reg)

plotreg(m1.reg, omit.coef = "Intercept")


# Use shares instead
m2.reg <- lm(ln.milex ~ prob.det.share + uncond.det.share + defense.dem + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP +
               ls.threatenv + comp.share, 
               data = state.char.full, majpower == 0)

summary(m2.reg)
plot(density(m2.reg$residuals))
plot(m2.reg)

plotreg(m2.reg, omit.coef = "Intercept")



# Use consultation variable 
m3.cons <- lm(ln.milex ~ consul.only.pres + offense.pres + defense.pres + defense.dem + lag.ln.milex +
               atwar + civilwar + polity + ln.GDP +
               ls.threatenv, 
               data = state.char.full, majpower == 0)

summary(m3.cons)
plot(density(m3.cons$residuals))
plot(m3.cons)

plotreg(m3.cons, omit.coef = "Intercept")


# Use consultation only as a share of total treaties 
m4.cons <- lm(ln.milex ~ consul.only.share + offense.share + defense.share + defense.dem + lag.ln.milex +
                atwar + civilwar + polity + ln.GDP +
                ls.threatenv, 
                data = state.char.full, majpower == 0)

summary(m4.cons)
plot(density(m4.cons$residuals))
plot(m4.cons)

plotreg(m4.cons, omit.coef = "Intercept")








