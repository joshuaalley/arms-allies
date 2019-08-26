# Joshua Alley
# Texas A&M University
# Summarizing and measuring Key alliance characteristics


# Load packages
library(here)
library(zoo)
library(ggplot2)
library(tidyr)
library(countrycode)
library(bfa)
library(coda)
library(dplyr)


# Set working directory to current folder
setwd(here::here())
getwd()

# Load ATOP v4 alliance-level data 
atop <- read.csv("data/atop-alliance-level.csv")
atop.mem.full <- read.csv("data/atop-member-level.csv")



# Conditional alliance commtiments
table(atop$conditio) # condititional
table(atop$offcon)
table(atop$offcon, atop$offense)
table(atop$defcon)
table(atop$defcon, atop$defense)


# overlap between offensive and defensive
table(atop$offense, atop$defense)
table(atop$offcon, atop$defcon)


# Generate counts of conditions attached to each type of commitment
# Use ATOP member-level data because conditions vary across members
atop.mem.full <- atop.mem.full %>%
    mutate(
      off.cond.count = offcoadv + offcoloc + offcocon + offconum + offcodem,
      def.cond.count = defcoadv + defcoloc + defcocon + defconum + defcodem + defconpr,
      neu.cond.count = neucoadv + neucoloc + neucocon + neuconum + neucodem + neuconpr,
      con.cond.count = concoadv + concoloc + concocon + concoreq,
      milsup.cond = off.cond.count + def.cond.count,
      total.cond = off.cond.count + def.cond.count + neu.cond.count + con.cond.count, 
      mil.support = ifelse(offense == 1 | defense == 1, 1, 0)
    )

ggplot(filter(atop.mem.full, offense == 1), aes(x = off.cond.count)) + geom_bar() # offensive conditions
ggplot(filter(atop.mem.full, defense == 1), aes(x = def.cond.count)) + geom_bar() # defensive conditions
ggplot(filter(atop.mem.full, neutral == 1), aes(x = neu.cond.count)) + geom_bar() # neutrality conditions
ggplot(filter(atop.mem.full, consul == 1), aes(x = con.cond.count)) + geom_bar() # consultation conditions

ggplot(filter(atop.mem.full, mil.support == 1), aes(x = milsup.cond)) + geom_bar() # total conditions on military support
ggplot(atop.mem.full, aes(x = total.cond)) + geom_bar() # total conditions on any kind of support

# Precise numbers of offense and defense pacts with number of conditions
table(atop.mem.full$def.cond.count, atop.mem.full$defense)
table(atop.mem.full$off.cond.count, atop.mem.full$offense)


# Generate an unconditional variable for defense and offense
# Generate other dummy indicators to produce a cumulative indicator of alliance scope
atop <- atop %>%
  mutate(
    uncond = ifelse(conditio == 0, 1, 0), # unconditional dummy
    uncond.def = ifelse(defcon == 0 & defense == 1, 1, 0), # unconditional defense dummy
    uncond.off = ifelse(offcon == 0 & offense == 1, 1, 0), # unconditional offense dummy 
    agpro.mult = ifelse(agprois == 5, 1, 0), # promise for additional agreements on multiple issues
    io.form = ifelse(organ1 > 0, 1, 0), # promise to form  an IO
    ecaid.dum = ifelse(ecaid > 0, 1, 0), # dummy indicator of economic aid
    
    milaid.dum = ifelse(milaid > 0, 1, 0), # dummy indicator of military aid
    uncond.milsup = ifelse(conditio == 0 & (offense == 1 | defense == 1), 1, 0), # unconditional military support
    base.dum = ifelse(base > 0, 1, 0),
    milcon.dum = ifelse(milcon > 0, 1, 0),
    
    trade.dum = ifelse(ecaid == 3, 1, 0), # dummy indicator of economic aid
    compag.econ = ifelse(compag == 2, 1, 0),
    compag.mil = ifelse(compag == 1, 1, 0),
    agpro.econ = ifelse(agprois == 2, 1, 0),
    organ1.dum = ifelse(organ1 > 0, 1, 0),
    
    notaiden.dum = ifelse(notaiden > 0, 1, 0),
    interv.dum = ifelse(interv > 0, 1, 0)
 ) %>% 
  rowwise() %>%
  mutate(scope.index = sum(uncond.milsup, milaid.dum, ecaid.dum, io.form, agpro.mult, na.rm = TRUE),
         milcor.index = sum(milcon.dum, milaid.dum, base.dum, intcom, na.rm = TRUE),
         econagg.index = sum(trade.dum, nomicoop, compag.econ, agpro.econ, na.rm = TRUE),
         econagg.dum = ifelse(econagg.index > 0, 1, 0),
         fp.conc.index = sum(noothall, notaiden.dum, thirdcom, divgains, interv.dum, na.rm = TRUE))



# Check variables: 
table(atop$uncond.def, atop$defcon)
table(atop$uncond.off, atop$offcon)
table(atop$offcon, atop$uncond.milsup)
table(atop$defcon, atop$uncond.milsup)
table(atop$uncond, atop$uncond.def) # 123/413 unconditional commitments are defense pacts
table(atop$uncond, atop$uncond.off) # 13/413 unconditional commitments are offense pacts
table(atop$uncond.milsup)
table(atop$scope.index, atop$uncond.milsup)

# Check indices
table(atop$milcor.index)
table(atop$econagg.index)
table(atop$fp.conc.index)
table(atop$milcor.index, atop$econagg.index)
table(atop$fp.conc.index, atop$milcor.index)
table(atop$fp.conc.index, atop$econagg.index)



# Look at scope of commitment
table(atop$uncond) # unconditional
table(atop$milaid) # military aid
table(atop$base) # basing rights
table(atop$ecaid) # economic aid
table(atop$organ1) # international organization
table(atop$organ2) # second IO
table(atop$intcom) # integrated command
table(atop$milcon)
table(atop$agpro.mult) # promise multiple agreements

# aggregate index of scope
table(atop$scope.index)
ggplot(atop, aes(x = scope.index)) + geom_bar()


# Turn dummy indicators into factors in a separate dataset
# atop.milsup <- filter(atop, offense == 1 | defense == 1) 
atop.depth <- select(atop,
                     uncond.milsup, offense, defense, nonagg, neutral, consul, 
                     intcom, compag.mil,  
                   milaid, milcon, base, organ1) 
# atop.depth <- as.matrix(atop.depth)
 atop.depth <- as.data.frame(atop.depth)
for(i in 1:ncol(atop.depth)){
  atop.depth[, i] <- as.ordered(atop.depth[, i])
}



# Create a 1-dimensional IRT model (all dummy inputs)
# latent.depth <- MCMCirtKd(datamatrix = atop.depth, 
#                             dimensions = 1,
#                             burnin = 10000, mcmc = 40000,
#                             thin = 40,
#                             item.constraints = list("uncond.milsup" = list(2,"+")),
#                             store.ability = TRUE,
#                             store.item = TRUE, 
#                             b0 = 0, # prior mean of zero for item parameters
#                             B0 = .5, # prior precision of variances 
#                             verbose = 5000)
# plot(latent.depth)
# summary(latent.depth)


# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.depth, scores = TRUE)
effectiveSize(latent.depth)
diag.geweke  <- geweke.diag(latent.depth)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))


# Use Murray BFA approach
latent.depth <- bfa_mixed(~ intcom + compag.mil + 
                            milaid + milcon + base + organ1 +
                            uncond.milsup + offense + defense + nonagg + neutral + consul, 
                          data = atop.depth, num.factor = 1,
                          keep.scores = TRUE, loading.prior = "gdp", 
                          px = TRUE, imh.iter = 500, imh.burn = 500,
                          nburn = 20000, nsim = 30000, thin = 30, print.status = 2000)

# Little bit of diagnosis
plot(get_coda(latent.depth))

# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.depth, scores = TRUE)
effectiveSize(lcap.sam)
diag.geweke  <- geweke.diag(lcap.sam)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))



# get posterior scores of latent factor: mean and variance
post.score <- get_posterior_scores(latent.depth)
atop$latent.depth.mean <- as.numeric(t(latent.depth$post.scores.mean))
atop$latent.depth.var <- as.numeric(t(latent.depth$post.scores.var))
atop$latent.depth.sd <- sqrt(atop$latent.depth.var)


# Plot two measures by ATOPID
ggplot(atop, aes(x = atopid, y = latent.depth.mean)) + geom_point()
ggplot(atop, aes(x = atopid, y = depth.index)) +
  geom_point(position = position_dodge(2))

# depth by year of formation
ggplot(atop, aes(x = begyr, y = latent.depth.mean)) + geom_point() +
  geom_errorbar(aes(ymin = latent.depth.mean - latent.depth.sd, 
                    ymax = latent.depth.mean + latent.depth.sd,
                    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1))



# Summarize latent strength: treaties with military support only
# weakest is 1870 France-UK offense/neutrality pact- meant to ensure Belgian neutrality
# median is ATOP 1180- UK, Fr and Austria during Crimean war

# 289 treaties with some promise of military support 
atop.milsup <- filter(atop, offense == 1 | defense == 1)
nrow(atop.milsup)

# histogram
ls.hist <- ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_classic() + labs(x = "Mean Latent Depth", y = "Treaties")
ls.hist


# depth against year of formation for treaties with military support
# Add error bars to plot
ls.styear <- ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_errorbar(aes(ymin = latent.depth.mean - latent.depth.sd, 
    ymax = latent.depth.mean + latent.depth.sd,
    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  labs(x = "Start Year", y = "Latent Depth of Treaty") +
  theme_classic()
ls.styear
# Combine plots 
multiplot.ggplot(ls.hist, ls.styear)



# highlight NATO
atop.milsup %>% 
  mutate(NATO = ifelse(atopid == 3180, T, F)) %>% 
  ggplot(aes(x = begyr, y = latent.depth.mean, color = NATO)) +
  geom_point() +
  scale_color_manual(values = c('#595959', 'red'))



# compare three different measures of depth
commitment.depth <- select(atop.milsup, atopid, 
                        latent.depth.mean, scope.index,
                         econagg.dum)
heatmap(as.matrix(commitment.depth[, 2:4]), scale="column")




# Prevalence of restrictions on member autonomy 
# Noteworthy alliance design considerations: restrictions on autonomy
table(atop$asymm) # asymmmetric
table(atop$specthrt) # specific threat
table(atop$contrib) # specifies specific contribution
table(atop$divgains) # specific division of gains
table(atop$base) # basing rights
table(atop$organ1) # international organizations
table(atop$terrres) # make territory or resources available
table(atop$thirdcom) # restrictions on third-party commitments 
table(atop$notaiden) # promise not to aid enemy
table(atop$dipaid) # promise diplomatic aid
table(atop$noothall) # promise not to form competing alliances
table(atop$compag) # presence of companion agreements
table(atop$nomicoop) # non-military cooperation
table(atop$interv) # intervention in domestic affairs
table(atop$agprois) # commitment to negotiate additional treaties
table(atop$intcom) # integrated command (peace and war)
table(atop$subord) # subordination of forces in war
table(atop$medarb) # mediation and arbitration


# Create variables for US and USSR membershipin Cold War treaties
ussr.mem <- apply(atop[, 72:130], 1, function(x) ifelse(x == 365, 1, 0))
ussr.mem <- t(ussr.mem)
atop$ussr.mem <- rowSums(ussr.mem, na.rm = TRUE)

# US
us.mem <- apply(atop[, 72:130], 1, function(x) ifelse(x == 2, 1, 0))
us.mem <- t(us.mem)
atop$us.mem <- rowSums(us.mem, na.rm = TRUE)

# Remove the US and Russian membership matrices from the environment
rm(ussr.mem)
rm(us.mem)

# limit US and USSR to Cold War Treaties
atop$us.mem[atop$begyr < 1945] <- 0
atop$ussr.mem[atop$begyr < 1945 | atop$begyr > 1991] <- 0


# create a superpower membership dummy
atop$super.mem <- ifelse(atop$us.mem == 1 | atop$ussr.mem == 1, 1, 0)



# count number of major powers in an alliance- need to capture symmetric major power treaties
atop.mem.list <- atop %>%
  select(c(begyr, (72:130)))
atop.mem.list$mp.count <- rowSums(
                (atop.mem.list == 2 & atop.mem.list$begyr > 1898) | # US 
                (atop.mem.list == 255 & atop.mem.list$begyr <= 1918)  | # Germany: 1816 to 1918
                (atop.mem.list == 255 & atop.mem.list$begyr >= 1925 & atop.mem.list$begyr <= 1945) | # Germany 1925 to 1945
                (atop.mem.list == 255 & atop.mem.list$begyr >= 1991) | # Germany 1991 to present  
                (atop.mem.list == 200) | # UK
                (atop.mem.list == 365 & atop.mem.list$begyr <= 1917) | # Russia: 1816 to 1917
                (atop.mem.list == 365 & atop.mem.list$begyr >= 1922) | # Russia: 1922 to present
                (atop.mem.list == 220 & atop.mem.list$begyr <= 1940) | # France 1816 to 1940
                (atop.mem.list == 220 & atop.mem.list$begyr >= 1945) | # France 1945 to present 
                (atop.mem.list == 710 & atop.mem.list$begyr >= 1950) | # China
                (atop.mem.list == 300 & atop.mem.list$begyr <= 1918) | # Austria-Hungary
                (atop.mem.list == 325 & atop.mem.list$begyr >= 1860 & atop.mem.list$begyr <= 1943) | # Italy
                (atop.mem.list == 740 & atop.mem.list$begyr <= 1945 & atop.mem.list$begyr >= 1895) | # Japan 1895 to 1940
                (atop.mem.list == 740 & atop.mem.list$begyr >= 1991), 
                na.rm = TRUE
                )

# assign count to ATOP data
atop$mp.count <- atop.mem.list$mp.count
rm(atop.mem.list)
             


# count number of members: non-missing membership variables
atop$num.mem <-  apply(atop[, 72:130], 1, function(x) sum(!is.na(x)))

# Plot alliance depth against size
ggplot(atop, aes(x = num.mem, y = latent.depth.mean)) +
  geom_point()



# Generate a measure of asymmetric capability
atop <- atop %>%
  mutate(
    non.maj.only = 
      ifelse(
        mp.count == 0 & super.mem == 0, 1, 0
      ),
    asymm.cap = 
      ifelse(
        super.mem == 1 | # super powers always generate asymm cap (no alliance only with each other)
          (mp.count > 0 & (mp.count < num.mem)), # major powers with non-major allies
        1, 0
      )
  )



# identify non-aggression only pacts
# Also, recode arms requirements and military aid variables from ATOP into dummy 
# variables that capture conditions where increases in arms spending are likely
atop <- mutate(atop, nonagg.only = ifelse((nonagg == 1 & offense != 1
                                  & defense != 1 & 
                                   consul != 1 & neutral != 1), 1 , 0),
               armred.rc = ifelse(armred == 2, 1, 0),
               milaid.rc = ifelse(milaid >= 2, 1, 0)
               )


# Generate a measure of FP similarity in initial year of alliance 
all.fp.sim <- read.csv("data/all-fp-sim.csv")
all.fp.sim <- all.fp.sim[complete.cases(all.fp.sim), ] # missing data on all measures of FP similarity 

# Get first observation for each ATOPID
all.fp.sim <- all.fp.sim %>%
  group_by(atopid) %>%
  mutate(
    yr1 = min(year)
  ) 

all.fpsim.first <- filter(all.fp.sim, year == yr1) %>% 
  group_by() %>% 
  select(-c(year, yr1))  


# Add measures of FP similiarity in first year observed
atop <- left_join(atop, all.fpsim.first)
atop.milsup <- filter(atop, offense == 1 | defense == 1)

# Look at correlation between FP similarity and depth

# Treaties with military support
cor.test(atop.milsup$mean.kap.sc, atop.milsup$latent.depth.mean)
cor.test(atop.milsup$low.kap.sc, atop.milsup$latent.depth.mean)

ggplot(atop.milsup, aes(x = mean.kap.sc, y = latent.depth.mean)) + 
  geom_point() + theme_classic()

# t-test w/ FP disagreement: higher FP agreement w/ economic agreements 
t.test(atop.milsup$mean.kap.sc ~ atop.milsup$econagg.dum)
t.test(atop.milsup$low.kap.sc ~ atop.milsup$econagg.dum)


# Subset for economic agreements
atop.econ <- filter(atop.milsup, econagg.dum == 1)
atop.econ.maj <- filter(atop.econ, non.maj.only == 0)


# Export to public-goods-test paper
write.csv(atop, 
          "../Dissertation/public-goods-test/data/atop-additions.csv", 
          row.names = F)


