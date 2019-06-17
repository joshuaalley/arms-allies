# Joshua Alley
# Texas A&M University
# Test whether Lower Spending from an alliance treaty decreases incidence of conflict


# Idea is this: credible treaties lead to lower defense spending,
# thereby reducing the risk of conflict. 


# Need the following objects from the arms-allies workspace
# state.vars dataset
# dataset of aggregate impact of alliance participation

# Outcome is a count of MID involvement in a given year. 

# Load packages
library(pscl) # need this for zero-inflated models


# Create dataset
# merge state.vars and aggregate impact of alliance participation
state.vars.all <- left_join(state.vars, agg.all.imp)

# summarize key IV: aggregate impact of alliances on military spending
summary(state.vars.all$agg.all.imp)
ggplot(state.vars.all, aes(x = agg.all.imp)) + geom_histogram()


# Dummy indicator of a negative aggregate effect
state.vars.all$neg.all.imp <- ifelse(state.vars.all$agg.all.imp < 0, 1, 0)
summary(state.vars.all$neg.all.imp)

# Should the count model use zero-inflated poisson or negative binomial? 
# Check for overdispersion of outcome
mean(state.vars.all$disputes)
var(state.vars.all$disputes)


# Fit zero-inflated poisson 
mid.zip <- zeroinfl(disputes ~ agg.all.imp + ln.gdp + polity + 
                      rival.milex + cold.war + majpower |
                      ln.gdp + polity + rival.milex,
                    data = state.vars.all)
summary(mid.zip)


# fit zero-inflated negative binomial
mid.zinb <- zeroinfl(disputes ~ agg.all.imp + ln.gdp + polity + 
                      rival.milex + cold.war + majpower |
                      ln.gdp + polity + rival.milex,
                    data = state.vars.all, dist = "negbin")
summary(mid.zinb)

AIC(mid.zip, mid.zinb)


# Replace aggregate impact with dummy of negative alliance impact
# fit zero-inflated negative binomial
mid.zinb.dum <- zeroinfl(disputes ~ neg.all.imp + ln.gdp + polity + 
                       rival.milex + cold.war + majpower |
                       ln.gdp + polity + rival.milex,
                     data = state.vars.all, dist = "negbin")
summary(mid.zinb.dum)


# Fit in non-major power sample
mid.zinb.nonmaj <- zeroinfl(disputes ~ neg.all.imp + ln.gdp + polity + 
                           rival.milex + cold.war |
                           ln.gdp + polity + rival.milex,
                         data = state.vars.all, subset = (majpower == 0),
                          dist = "negbin")
summary(mid.zinb.nonmaj)

