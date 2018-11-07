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

