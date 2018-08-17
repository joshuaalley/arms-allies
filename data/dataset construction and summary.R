# Joshua Alley
# Texas A&M University
# Constructing Dataset for paper on arms-alliances tradeoff

# To test the prediction that compellent alliances lead to increased spending, 
# need to expand sample into pre-WWII period

# Load packages
library(here)
library(reshape)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(countrycode)



# Set working directory to current folder
setwd(here::here())
getwd()


### This First section creates data on state-membership in alliances,
#   and incorporates both ATOP and the Benson's alliance classifications. 
########
# Load Benson's 2011 Data
d.benson <- read.csv("data/alliance-types-benson.csv")
d.benson <- cbind.data.frame(d.benson[, 1:2], d.benson[,8:15 ])

# Compile all the phases into a single observation
# NB: Benson's phases do not match ATOPs exactly. This makes combining his coding with other ATOP coding very difficult. 
d.benson$atopid <- trunc(d.benson$atopid)

d.benson <- d.benson %>% 
  group_by(atopid) %>% 
  summarize(
    phases = n(),
    startyear = min(startyear, na.rm = TRUE),
    uncond_comp = max(uncond_comp, na.rm = TRUE), 
    cond_comp = max(cond_comp, na.rm = TRUE), 
    uncond_det = max(uncond_det, na.rm = TRUE),
    cond_det = max(cond_det, na.rm = TRUE),
    prob_det = max(prob_det, na.rm = TRUE),
    pure_cond_det = max(pure_cond_det, na.rm = TRUE),
    discret_milsupport = max(discret_milsupport, na.rm = TRUE),
    discret_intervene = max(discret_intervene, na.rm = TRUE)
  )


# Load the ATOP data on alliance-level characteristics
atop <- read.csv("data/atop-alliance-level.csv")

# Create variables for US and USSR membership
russ.mem <- apply(atop[, 71:120], 1, function(x) ifelse(x == 365, 1, 0))
russ.mem <- t(russ.mem)
atop$russ.mem <- rowSums(russ.mem, na.rm = TRUE)

# US
us.mem <- apply(atop[, 71:120], 1, function(x) ifelse(x == 2, 1, 0))
us.mem <- t(us.mem)
atop$us.mem <- rowSums(us.mem, na.rm = TRUE)

# Remove the US and Russian membership matrices from the environment
rm(russ.mem)
rm(us.mem)





# Create a datasets with essential ATOP variables
atop.key <- select(atop, atopid, 
                   offense, defense, neutral, consul, nonagg,           
                   bilat, wartime, conditio,
                   armred, organ1, milaid, us.mem, russ.mem)

summary(atop.key$atopid)


# merge with Benson data
alliance.char.full <- left_join(atop.key, d.benson)


# Load Chiba et al Replication data 
chiba.etal <- read.csv("data/chiba-etal2015.csv")
chiba.etal <- select(chiba.etal, atopid, dem_prop, onlyconsul, mem_num)
colnames(chiba.etal) <- c("atopid", "dem.prop", "onlyconsul", "num.mem")

# Drop atopids greater than 6000, which are all missing
chiba.etal <- subset(chiba.etal, chiba.etal$atopid < 6000)


# add Chiba et al data
alliance.char.full <- left_join(alliance.char.full, chiba.etal)

# remove non-aggression only pacts
alliance.char.full <- mutate(alliance.char.full, nonagg.only = ifelse((nonagg == 1 & offense != 1 & defense != 1 & consul != 1 & neutral != 1), 1 , 0))

alliance.char <- filter(alliance.char.full, nonagg.only != 1)
alliance.char <- select(alliance.char, -nonagg.only)

# Create an indicator of compellent alliances and another for alliances with none of Benson's conditions
# Further indicators of mixed alliances and general indicators of conditional/unconditional pacts
# Also, recode arms requirements and military aid variables from ATOP into dummy 
# variables that capture conditions where increases in arms spending are likely
alliance.char <- mutate(alliance.char,
                        compellent = ifelse((uncond_comp == 1 | cond_comp == 1), 1 , 0),
                        none = ifelse(prob_det == 0 & uncond_det == 0 & cond_det == 0 & compellent == 0, 1, 0),
                        number.types = prob_det + uncond_det + cond_det + compellent,
                        mixed = ifelse(number.types > 1, 1, 0),
                        conditional = ifelse(cond_det == 1 | cond_comp == 1 | pure_cond_det == 1, 1, 0),
                        unconditional = ifelse(uncond_comp == 1 | uncond_det == 1, 1, 0),
                        armred.rc = ifelse(armred == 2, 1, 0),
                        milaid.rc = ifelse(milaid >= 2, 1, 0))

# Remove surplus variables 
alliance.char <- select(alliance.char, - c(armred, milaid))

# Check overlap between consultation only, neutrality and none variable 
table(alliance.char$none, alliance.char$onlyconsul)
table(alliance.char$none, alliance.char$neutral)
sum(alliance.char$compellent)
table(alliance.char$uncond_comp, alliance.char$uncond_det)




####
# load the ATOP alliance-member data (This provides the basis for the alliance member matrix)
atop.mem <- read.csv("data/atop-member-level.csv")


# Create a datasets with observation identifiers
atop.mem <- cbind.data.frame(atop.mem$atopid, atop.mem$member, atop.mem$yrent, atop.mem$yrexit)

colnames(atop.mem) <- c("atopid", "ccode", "startyear", "endyear")

summary(atop.mem$atopid)


# Merge the two alliance data types using ATOP ID and starting year
alliance.comp <- left_join(atop.mem, alliance.char, by = "atopid")


# Create a frequency variable to expand data to country-alliance-year data form
# Alliances that are still operational have 0 for an end year- replace that with 2003
# Don't care about truncation here, just need to know if alliance is operational
alliance.comp$endyear[alliance.comp$endyear == 0] <- 2003


alliance.comp$freq <- alliance.comp$endyear - alliance.comp$startyear
# Alliances that end in the same year have a value of 0, given those a value of 1
# and add one year to the other alliance years
alliance.comp$freq <- alliance.comp$freq +  1



# Expand the dataset, copying each observation according to the frequency variable
alliance.comp.expand <- untable(alliance.comp, alliance.comp$freq)

# Create a year variable by using the number of exanded observations
# group data by country and ATOP alliance and count rows 
alliance.comp.expand <- alliance.comp.expand %>%
  group_by(atopid, ccode, startyear, endyear) %>%
  mutate(count = row_number() - 1)


alliance.comp.expand$year = alliance.comp.expand$startyear + alliance.comp.expand$count


# Sort by country code to fill in missing data from these alliances
# Missing data is the result of a start-date merger issue
alliance.comp.expand <- alliance.comp.expand[
  order(alliance.comp.expand$ccode, alliance.comp.expand$atopid, alliance.comp.expand$year), ]

# Again, fill in missing alliance information with info from same alliance
alliance.comp.expand <- na.locf(alliance.comp.expand)
alliance.comp.expand <- alliance.comp.expand[complete.cases(alliance.comp.expand), ]

# Make sure no duplicate observations from merging process
alliance.comp.expand <- unique(alliance.comp.expand)

# Filter data 
alliance.comp.expand <- filter(alliance.comp.expand, year <= 2003)


### The alliance.comp.expand dataframe has alliance-year data for all ATOP alliances
# Countries appear in multiple years, so states are multiple members within alliances
# This next section of code brings in country-level data








#######
### This section creates a state-year dataset from 1815 to 2003. 
 

# Import correlates of war national capabilities data. 
cinc.data <- read.csv("data/NMC_5_0.csv")
head(cinc.data)
cinc.data <- select(cinc.data, -c(version))


# Merge in polity data
polity.data <- read.csv("data/polity4v2015.csv")
head(polity.data)
polity.data <- select(polity.data, ccode, year, democ, autoc, polity)
sum(is.na(polity.data$polity))


# Merge with a left join
state.vars <- left_join(cinc.data, polity.data)
head(state.vars)
sum(is.na(state.vars$polity))

# Generate a democracy variable if polity score > 5
state.vars$democracy <- ifelse(state.vars$polity > 5, 1, 0)


# Load Maddison Project GDP data
maddison.gdp <- read.csv("data/maddison-data.csv")
maddison.gdp <- select(maddison.gdp, country, countrycode, year, cgdppc, pop)

# Population is in thousands- create an indicator of population to create 
# a crude measure of GDP
maddison.gdp$population <- maddison.gdp$pop*1000
maddison.gdp$gdp <- maddison.gdp$population * maddison.gdp$cgdppc
maddison.gdp$ln.gdp <- log(maddison.gdp$gdp)

# Create cow codes from the ISO codes
maddison.gdp$ccode <- countrycode(maddison.gdp$country, origin = "country.name",
                                  destination = "cown")

# Fix Serbia/Yugoslavia country code problem
# First remove extrapolated Yugolav values
maddison.gdp <- filter(maddison.gdp, !(countrycode == "YUG" & year >= 1992))
# then remove serbia values prior to 1992
maddison.gdp <- filter(maddison.gdp, !(countrycode == "SRB" & year <= 1992))
# last, give serbia country code 345
maddison.gdp$ccode[maddison.gdp$countrycode == "SRB"] <- 345
  
# Left join state characteristics and gdp data
gdp.data <- select(maddison.gdp, ccode, year, cgdppc, ln.gdp)

state.vars <- left_join(state.vars, gdp.data)



# Add data on participation in inter-state war
cow.interstate <- read.csv("data/Inter-StateWarData_v4.0.csv")

# Duration using start and end years
cow.interstate$war.dur <- (cow.interstate$EndYear1 - cow.interstate$StartYear1) + 1

# Expand the dataset, copying each observation according to the frequency variable
cow.interstate <- untable(cow.interstate, cow.interstate$war.dur)

# Create a year variable by using the number of exanded observations
# group data by country and ATOP alliance and count rows 
cow.interstate <- cow.interstate %>%
  group_by(WarNum, ccode, StartYear1, EndYear1) %>%
  mutate(
    count = row_number() - 1,
    year = count + StartYear1,
    atwar = 1)

# Select key vars, check years variables, and merge
interstate.war <- cow.interstate %>%
      group_by(ccode, year) %>% 
      select(ccode, year, atwar)

summary(interstate.war$year)

# Merge conflict data with other state variables
state.vars <- left_join(state.vars, interstate.war)
# Replace missing var values with zero (only merged 1s)
state.vars$atwar[is.na(state.vars$atwar)] <- 0




# Add data on civil war: Using COW because UCDP only covers 1946-present
# There is no succint way to capture the state facing a rebel group: COW includes external interveners 
cow.civilwar <- read.csv("data/Intra-StateWarData_v4.1.csv")

# Pull ccode for states on side B into a single vector that will become the ccode variable
for(i in 1:nrow(cow.civilwar)){
cow.civilwar$CcodeA[i][cow.civilwar$CcodeA[i] == -8 & cow.civilwar$CcodeB[i] != -8] <- cow.civilwar$CcodeB[i]  
}
# Remove observations with no states involved
cow.civilwar <- filter(cow.civilwar, CcodeA != -8)

# replace missing end years with last year of observed state data
cow.civilwar$EndYear1[cow.civilwar$EndYear1 == -7] <- 2012
# fix a small data issue
cow.civilwar$EndYear1[cow.civilwar$WarName == "Yellow Cliff Revolt"] <- 1866

# Similar process to constructing interstate war variable 
# Create a war duration variable
cow.civilwar$war.dur <- (cow.civilwar$EndYear1 - cow.civilwar$StartYear1) + 1
summary(cow.civilwar$war.dur)

# Expand the dataset, copying each observation by the number of years at war
cow.civilwar <- untable(cow.civilwar, cow.civilwar$war.dur)


# Create a year variable by using the number of exanded observations
# group data by country and ATOP alliance and count rows 
cow.civilwar <- cow.civilwar %>%
  rename(ccode = CcodeA) %>%
  group_by(WarNum, ccode, StartYear1, EndYear1) %>%
  mutate(
    count = row_number() - 1,
    year = count + StartYear1,
    civilwar.part = 1)

# Select key vars, check years variables, and merge
civil.war <- cow.civilwar %>%
  group_by(ccode, year) %>% 
  select(ccode, year, civilwar.part)
civil.war <- data.frame(civil.war)
summary(civil.war$year)
civil.war <- unique(civil.war)

# Merge conflict data with other state variables
state.vars <- left_join(state.vars, civil.war)
# Replace missing var values with zero (only merged 1s)
state.vars$civilwar.part[is.na(state.vars$civilwar.part)] <- 0

summary(state.vars$civilwar.part)



# Add data on external threat 
# Leeds and Savun measure relies on S-score, so runs from 1816 to 2000
ls.threat.data <- read.csv("data/leeds-savun-threat.csv")

state.vars <- left_join(state.vars, ls.threat.data)




