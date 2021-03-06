# Joshua Alley
# Texas A&M University
# State-year regression models of alliance conditions and arms


# Environment is determined by use of projects and/or using this file in conjunction with
# the file dataset construction and summary.R 

# The full dataset can provide the basis of a state-year characteristics dataset 
# with some summary variables for the alliance portfolio
# Can then merge this with the state characteristics dataset to create a dataset for 
# single-level regressions
state.char.full <- state.ally.year %>%
  group_by(ccode, year) %>%
  mutate(deep.alliance = ifelse(latent.depth.mean > median(latent.depth.mean), 1, 0)) %>%
  summarize(
    treaty.count = n(),
    total.ally.expend = sum(ally.spend[defense == 1 | offense == 1], na.rm = TRUE),
    #avg.treaty.contrib = mean(alliance.contrib[defense == 1 | offense == 1], na.rm = TRUE),
    
    uncond.milsup.pres = max(uncond.milsup, na.rm = TRUE),
    uncond.milsup.expend = sum(ally.spend[uncond.milsup == 1], na.rm = TRUE),
    
    deep.alliance = max(deep.alliance, na.rm = TRUE),
    
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
table(state.char.full$deep.alliance)

# Transform DV
state.char.full$ihs.growth.milex <- asinh(state.char.full$growth.milex)


# Start with a simple linear regression
m1.all <- lm(ihs.growth.milex ~ avg.depth +
               econagg.pres + uncond.milsup.pres +
            atwar + civilwar.part + polity + gdp.growth + majpower +
            lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
          data = state.char.full
          )
summary(m1.all)
qqnorm(m1.all$residuals)
qqline(m1.all$residuals)


# Add state and year fixed effects 
m1.reg.fe <- plm(ihs.growth.milex ~ avg.depth +
                   econagg.pres + uncond.milsup.pres +
                   atwar + civilwar.part + polity + gdp.growth + majpower +
                   lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                 index = c("ccode", "year"),
                 effect = "individual", # unrestricted error covariance
                 data = state.char.full,
                 model = "within")

summary(m1.reg.fe)


# Add state and year fixed effects: dummy IV 
m1.reg.fe.dum <- plm(ihs.growth.milex ~ deep.alliance +
                   econagg.pres + uncond.milsup.pres +
                   atwar + civilwar.part + polity + gdp.growth + majpower +
                   lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                 index = c("ccode", "year"),
                 effect = "individual", # unrestricted error covariance
                 data = state.char.full,
                 model = "within")

summary(m1.reg.fe.dum)



# Robust Regression 
###### 
# Residuals in the above have extremely heavy tails. Robust regression weights observations as 
# a function of their residual, ensuring least squares is still efficient
m1r.reg <- rlm(ihs.growth.milex ~ avg.depth +
                 econagg.pres + uncond.milsup.pres +
                 atwar + civilwar.part + polity + gdp.growth + majpower +
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
               data = state.char.full)

summary(m1r.reg)
plot(m1r.reg$residuals, m1r.reg$w)


# subset by major and minor powers
# Major powers
rreg.maj <- rlm(ihs.growth.milex ~ avg.depth +
                    econagg.pres + uncond.milsup.pres + 
                     atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 1))

summary(rreg.maj)
plot(rreg.maj$residuals, rreg.maj$w)

# minor powers
rreg.min <- rlm(ihs.growth.milex ~ avg.depth +
                  econagg.pres + uncond.milsup.pres +
                     atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 0))

summary(rreg.min)
plot(rreg.min$residuals, rreg.min$w)


# minor powers
rreg.min.dum <- rlm(ihs.growth.milex ~ deep.alliance +
                  econagg.pres + uncond.milsup.pres +
                  atwar + civilwar.part + polity + gdp.growth + ln.ally.expend +
                  lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                data = state.char.full, subset = (majpower == 0))

summary(rreg.min.dum)
plot(rreg.min$residuals, rreg.min$w)



# Interact major power indicator with depth
rreg.int <- rlm(ihs.growth.milex ~ avg.depth + as.factor(majpower) + avg.depth:as.factor(majpower) + 
                  econagg.pres + uncond.milsup.pres +
                 atwar + civilwar.part + polity + gdp.growth + 
                 lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
               data = state.char.full)

summary(rreg.int)
plot(rreg.int$residuals, rreg.int$w)

# Calculate marginal effects
margins(rreg.int)
cplot(rreg.int, x = "majpower", dx = "avg.depth", what = "effect",
      main = "Average Marginal Effect of Treaty Depth For Major and Non-Major Powers",
      factor.lty = 0L, rug = FALSE, xvals = c(0, 1))
abline(h = 0)




### Another option is to consider how total allied capability is modified by presence of economic agreement
# and to look at whether that conditional relationship shifts between major and non-major powers


# Robust regression: major
rreg.ex.maj <- rlm(growth.milex ~ avg.depth + ln.ally.expend + avg.depth:ln.ally.expend +
                      econagg.pres + uncond.milsup.pres +
                      atwar + civilwar.part + polity + gdp.growth + 
                      lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                    data = state.char.full, subset = (majpower == 1))

summary(rreg.ex.maj)

# Calculate marginal effects
margins(rreg.ex.maj)
cplot(rreg.ex.maj, x = "deep.alliance", dx = "ln.ally.expend", what = "effect",
      main = "Average Marginal Effect of Allied Military Capability")
abline(h = 0)


# Robust regression: minor
rreg.ex.min <- rlm(ihs.growth.milex ~ avg.depth + ln.ally.expend + avg.depth:ln.ally.expend +
                     econagg.pres + uncond.milsup.pres +
                     atwar + civilwar.part + polity + gdp.growth + 
                     lsthreat + cold.war + avg.num.mem + avg.dem.prop,
                   data = state.char.full, subset = (majpower == 0))

summary(rreg.ex.min)

# Calculate marginal effects
margins(rreg.ex.min)
cplot(rreg.ex.min, x = "avg.depth", dx = "ln.ally.expend", what = "effect",
      main = "Average Marginal Effect of Allied Military Capability")
abline(h = 0)




# Average Treaty contribution as another proxy for size
# filter out cases with no alliances
inter.data.rel <- filter(state.char.full, treaty.pres == 1)
inter.data.rel <- as.data.frame(inter.data.rel)



# Robust regression: non-major power alliance members and average depth
m1.all <- rlm(ihs.growth.milex ~ avg.depth + 
                    econagg.pres + uncond.milsup.pres +
                     atwar + civilwar.part + polity + gdp.growth +
                     lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
                   data = inter.data.rel, subset = (majpower == 0)
)
summary(m1.all)




# Robust regression: deep alliance dummy
m1.all.dum <- rlm(ihs.growth.milex ~ deep.alliance + 
                econagg.pres + uncond.milsup.pres +
                atwar + civilwar.part + polity + gdp.growth +
                lsthreat + cold.war + avg.num.mem + ln.ally.expend + avg.dem.prop,
              data = inter.data.rel, subset = (majpower == 0)
)
summary(m1.all.dum)


eba.all <- eba(data = inter.data.rel, y = "ihs.growth.milex", 
               doubtful = c("deep.alliance", "econagg.pres", 
                            "atwar", "civilwar.part", "polity", "gdp.growth", "ln.ally.expend",
                            "lsthreat", "cold.war", "avg.num.mem", "avg.dem.prop"), 
               focus = c("deep.alliance", "econagg.pres"),
               k = 0:9, reg.fun = lm)
print(eba.all)
hist(eba.all, variables = c("deep.alliance", "econagg.pres"),
     main = c(deep.alliance = "Deep Alliance Dummy", econagg.pres = "Presence of Economic Concessions"),
     normal.show = TRUE
)



# Plot key coefficients 
coef.avg <- multiplot(rreg.min, m1.all, m1.reg.fe, 
            names = c("Non-Major Powers",
                    "Non-Major Powers in Alliances", 
                    "FE: Non-Major Powers"),
            coefficients = c("avg.depth"),
            by = "Model",
            xlab = "Value",
            color = "black",
            zeroColor = "black",
            zeroType = 1,
            zeroLWD = 1,
            ylab = "Sample",
            title = "Average Alliance Depth",
          lwdInner = 2,
          lwdOuter =  2,
          pointSize = 4,
          horizontal = FALSE
      ) + theme_bw()
coef.avg

# Same plot with dummy 
coef.dum <- multiplot(rreg.min.dum, m1.all.dum, m1.reg.fe.dum, 
            names = c("Non-Major Powers",
                    "Non-Major Powers in Alliances", 
                    "FE: Non-Major Powers"),
            coefficients = c("deep.alliance"),
            by = "Model",
            xlab = "Value",
            color = "black",
            zeroColor = "black",
            zeroType = 1,
            zeroLWD = 1,
            ylab = "Sample",
            title = "Deep Alliance Dummy",
            lwdInner = 2,
            lwdOuter =  2,
            pointSize = 4,
            horizontal = FALSE
        ) + theme_bw()
coef.dum


# plot both results together 
grid.arrange(coef.avg, coef.dum,
             nrow = 2)
results.singlelev <- arrangeGrob(coef.avg, coef.dum,
                               nrow = 2)
ggsave("appendix/single-level-mplot.png", results.singlelev, height = 6, width = 8) #save file






# Remove all these regressions from environment
rm(list = c("rreg.maj", "rreg.min",
            "m1.reg.fe", "rreg.int"))

