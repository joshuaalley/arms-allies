# Joshua Alley
# Texas A&M University
# Summarizing and measuring Key alliance characteristics


# Load packages
library(here)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(countrycode)
library(bfa)
library(coda)



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
# Generate other dummy indicators to produce a cumulative indicator of alliance strength
atop <- atop %>%
  mutate(
    uncond = ifelse(conditio == 0, 1, 0), # unconditional dummy
    uncond.def = ifelse(defcon == 0 & defense == 1, 1, 0), # unconditional defense dummy
    uncond.off = ifelse(offcon == 0 & offense == 1, 1, 0), # unconditional offense dummy 
    agpro.mult = ifelse(agprois == 5, 1, 0), # promise for additional agreements on multiple issues
    io.form = ifelse(organ1 > 0, 1, 0), # promise to form  an IO
    ecaid.dum = ifelse(ecaid > 0, 1, 0), # dummy indicator of economic aid
    milaid.dum = ifelse(milaid > 0, 1, 0), # dummy indicator of military aid
    uncond.milsup = ifelse(conditio == 0 & (offense == 1 | defense == 1), 1, 0) # unconditional military support
 ) %>% 
  rowwise() %>%
  mutate(str.index = sum(uncond.milsup, milaid.dum, ecaid.dum, io.form, agpro.mult, na.rm = TRUE))



# Check variables: 
table(atop$uncond.def, atop$defcon)
table(atop$uncond.off, atop$offcon)
table(atop$offcon, atop$uncond.milsup)
table(atop$defcon, atop$uncond.milsup)
table(atop$uncond, atop$uncond.def) # 123/413 unconditional commitments are defense pacts
table(atop$uncond, atop$uncond.off) # 13/413 unconditional commitments are offense pacts
table(atop$uncond.milsup)
table(atop$str.index, atop$uncond.milsup)


# Look at strength of commitment
table(atop$uncond) # unconditional
table(atop$milaid) # military aid
table(atop$base) # basing rights
table(atop$ecaid) # economic aid
table(atop$organ1) # international organization
table(atop$organ2) # second IO
table(atop$intcom) # integrated command
table(atop$agpro.mult) # promise multiple agreements

# aggregate index of strength
table(atop$str.index)
ggplot(atop, aes(x = str.index)) + geom_bar()


# Turn dummy indicators into factors in a separate dataset
atop.str <- select(atop, uncond.milsup, offense, defense, nonagg,
                   neutral, consul, intcom, agpro.mult, 
                   milaid, base, organ1, ecaid, noothall) 
atop.str <- as.data.frame(atop.str)
for(i in 1:ncol(atop.str)){
  atop.str[, i] <- as.ordered(atop.str[, i])
}

# Use Murray BFA approach
latent.strength <- bfa_mixed(~ uncond.milsup + offense + defense + 
                                neutral + consul + nonagg +
                                milaid + base + ecaid + organ1 + 
                                intcom + agpro.mult + noothall, 
                          data = atop.str, num.factor = 1,
                         keep.scores = TRUE, loading.prior = "gdp", 
                         px = TRUE, imh.iter = 500, imh.burn = 500,
                         nburn = 10000, nsim = 20000, thin = 20, print.status = 2000)

# Little bit of diagnosis
plot(get_coda(latent.strength))

# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.strength, scores = TRUE)
effectiveSize(lcap.sam)
diag.geweke  <- geweke.diag(lcap.sam)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))


# get posterior scores of latent factor: mean and variance
post.score <- get_posterior_scores(latent.strength)
atop$latent.str.mean <- as.numeric(t(latent.strength$post.scores.mean))
atop$latent.str.var <- as.numeric(t(latent.strength$post.scores.var))
atop$latent.str.sd <- sqrt(atop$latent.str.var)


# Plot two measures by ATOPID
ggplot(atop, aes(x = atopid, y = latent.str.mean)) + geom_point()
ggplot(atop, aes(x = atopid, y = str.index)) +
  geom_point(position = position_dodge(2))

# Histogram of latent strength- all ATOP alliances 
ggplot(atop, aes(x = latent.str.mean)) + geom_histogram() +
  theme_classic() + labs(x = "Mean Latent Strength", y = "Treaties")

# Strength by year of formation
ggplot(atop, aes(x = begyr, y = latent.str.mean)) + geom_point() +
  geom_errorbar(aes(ymin = latent.str.mean - latent.str.sd, 
                    ymax = latent.str.mean + latent.str.sd,
                    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1))



# Summarize latent strength: treaties with military support only
atop.milsup <- filter(atop, offense == 1 | defense == 1) 
summary(atop.milsup$latent.str.mean)
# weakest is 1870 France-UK offense/neutrality pact- meant to ensure Belgian neutrality
# median is ATOP 1180- UK, Fr and Austria during Crimean war

# 289 treaties with some promise of military support 
nrow(atop.milsup)

# histogram
ls.hist <- ggplot(atop.milsup, aes(x = latent.str.mean)) + geom_histogram() +
  theme_classic() + labs(x = "Mean Latent Strength", y = "Treaties")
ls.hist


# Strength against year of formation for treaties with military support
# Add error bars to plot
ls.styear <- ggplot(atop.milsup, aes(x = begyr, y = latent.str.mean)) +
  geom_errorbar(aes(ymin = latent.str.mean - latent.str.sd, 
    ymax = latent.str.mean + latent.str.sd,
    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  labs(x = "Start Year", y = "Latent Strength of Treaty") +
  theme_classic()
ls.styear
# Combine plots 
multiplot.ggplot(ls.hist, ls.styear)



# highlight NATO
atop %>% 
  mutate(NATO = ifelse(atopid == 3180, T, F)) %>% 
  ggplot(aes(x = begyr, y = latent.str.mean, color = NATO)) +
  geom_point() +
  scale_color_manual(values = c('#595959', 'red'))



# compare three different measures of strength
commitment.str <- select(atop, atopid, 
                         uncond.milsup, str.index,
                         latent.str.mean)
heatmap(as.matrix(commitment.str[, 2:4]), scale="column")




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
atop$ussr.mem[atop$begyr < 1945] <- 0

# count number of members: non-missing membership variables
atop$num.mem <-  apply(atop[, 72:130], 1, function(x) sum(!is.na(x)))

# Plot alliance strength against size
ggplot(atop, aes(x = latent.str.mean, y = num.mem)) +
  geom_point()

# identify non-aggression only pacts
# Also, recode arms requirements and military aid variables from ATOP into dummy 
# variables that capture conditions where increases in arms spending are likely
atop <- mutate(atop, nonagg.only = ifelse((nonagg == 1 & offense != 1
                                  & defense != 1 & 
                                   consul != 1 & neutral != 1), 1 , 0),
               armred.rc = ifelse(armred == 2, 1, 0),
               milaid.rc = ifelse(milaid >= 2, 1, 0)
               )


# Export to public-goods-test paper
write.csv(atop, 
          "C:/Users/jkalley14/Dropbox/Research/Dissertation/public-goods-test/data/atop-additions.csv", 
          row.names = F)


