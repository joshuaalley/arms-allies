# Joshua Alley
# Texas A&M University
# Constructing Dataset for paper on arms-alliances tradeoff

# To test the prediction that compellent alliances lead to increased spending, 
# need to expand sample into pre-WWII period

# Load packages
library(here)
library(reshape)
library(arm)
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
 

# The base national capability data starts with Brenton Kenkel's merger of 
# CINC and Polity https://github.com/brentonk/merge-cow-polity 

# Import that csv file: 
state.vars <- read.csv("data/cinc-polity.csv")
head(state.vars)
summary(state.vars$milex)

# Filter out component polity indicators
state.vars <- select(state.vars, -c(xrreg, xrcomp, xropen, xconst, parreg, parcomp, exrec, exconst, polcomp))

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
cow.civilwar.annual <- cow.civilwar %>%
  rename(ccode = CcodeA) %>%
  group_by(WarNum, ccode, StartYear1, EndYear1) %>%
  mutate(
    count = row_number() - 1,
    year = count + StartYear1,
    civilwar.part = 1)

# Select key vars, check years variables, and merge
civil.war <- cow.civilwar.annual %>%
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




#  Code contextual variables
state.vars <- state.vars %>%
          mutate(
            ww1 = ifelse((year >= 1914 & year <= 1918), 1, 0), # world war 1
            ww2 = ifelse((year >= 1939 & year <= 1945), 1, 0), # world war 2
            cold.war = ifelse((year >= 1947 & year <= 1990), 1, 0) # cold war
          )



# Code whether a state is a major power by Correlates of War criteria

# relevant country codes and years
# US- 2 from 1899
# Germany- 255 | 260, 1816  to 1918, 1925 through 1945, and 1991 to present
# UK- 200 
# Russia- 365 1816 to 1917, 1922 to present
# France- 220 1816 to 1940, and 1945 to present
# China- 710 from 1950
# Austria-Hungary- 300 1816 to 1918
# Italy- 325 1860 to 1943
# Japan- 740 1895 through 1945 and 1991 to 

state.vars <- state.vars %>%
              mutate(majpower = ifelse( 
                        (ccode == 2 & year > 1898) | # US 
                        (ccode == 255 & year <= 1918)  | # Germany: 1816 to 1918
                        (ccode == 255 & year >= 1925 & year <= 1945) | # Germany 1925 to 1945
                        (ccode == 255 & year >= 1991) | # Germany 1991 to present  
                        (ccode == 200) | # UK
                        (ccode == 365 & year <= 1917) | # Russia: 1816 to 1917
                        (ccode == 365 & year >= 1922) | # Russia: 1992 to present
                        (ccode == 220 & year <= 1940) | # France 1816 to 1940
                        (ccode == 220 & year >= 1945) | # France 1945 to present 
                        (ccode == 710 & year >= 1950) | # China
                        (ccode == 300 & year <= 1918) | # Austria-Hungary
                        (ccode == 325 & year >= 1860 & year <= 1943) | # Italy
                        (ccode == 740 & year <= 1945 & year >= 1895) | # Japan 1895 to 1940
                        (ccode == 740 & year >= 1991), # Japan 1991 to present
                        1, 0) 
                    )

major.powers <- filter(state.vars, majpower == 1)

# Add log-transformed military expenditure variable
summary(state.vars$milex)
state.vars <- mutate(state.vars,
                     ln.milex = log(milex + 1)
                     )
summary(state.vars$ln.milex)
ggplot(state.vars, aes(x = ln.milex)) + geom_density()


### This Section combines state characteristics and alliance data
#####
# Needed for a state-year measure of allied capability and to create the matrix of state membership in alliances

# Merge the state characteristics and alliance data
atop.cow.year <- right_join(alliance.comp.expand, state.vars)

# Change order of variables for ease in viewing
atop.cow.year <- atop.cow.year %>% 
  select(atopid, ccode, year, everything())

# Ensure no duplicate observations are present after merging- not an issue here
atop.cow.year <- unique(atop.cow.year)

# Sort data 
atop.cow.year[order(atop.cow.year$ccode, atop.cow.year$year, atop.cow.year$atopid), ]


# Some missing alliance characteristics data for states that are not members of an ATOP alliance
# Replace ATOP indicator with zero if missing
atop.cow.year$atopid[is.na(atop.cow.year$atopid)] <- 0

# If no ATOP alliance, fill all other alliance characteristic variables with a zero.
atop.cow.year[4:38][is.na(atop.cow.year[, 4:38] & atop.cow.year$atopid == 0)] <- 0


# restrict sample to minor powers
atop.cow.year <- filter(atop.cow.year, majpower == 0)


# Create a dataset of state-year alliance membership:
atop.cow.year <- group_by(atop.cow.year, atopid, ccode, year)
state.mem <- atop.cow.year %>% select(atopid, ccode, year)
state.mem <-  mutate(state.mem, member = 1)
state.mem <- distinct(state.mem, atopid, ccode, year, .keep_all = TRUE)

# This matrix has a binary indicator of which alliances states are a member of in a given year
state.mem <- spread(state.mem, key = atopid, value = member, fill = 0)

# Remove the zero or no alliance category
state.mem <- subset(state.mem, select = -(3))



# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year <- atop.cow.year %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  summarize(
    avg.democ = mean(polity, na.rm = TRUE),
    total.cap = sum(cinc, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    num.mem = n()
  )

alliance.year[order(alliance.year$atopid, alliance.year$year), ]

# With na.rm = TRUE, all missing values have a sum of zero.
# I filter out all of these alliance-year observations
alliance.year <- filter(alliance.year, total.expend != 0)

ggplot(alliance.year, aes(x = total.expend)) + geom_density()


# Create a membership matrix with the spending of all 
# other alliance members in place of 1s 
state.mem.cap <- atop.cow.year %>% 
  select(atopid, ccode, year, ln.milex) %>% 
  left_join(alliance.year) %>%
  mutate(ally.spend = total.expend - ln.milex) %>%
  distinct(state.mem.cap, atopid, ccode, year, .keep_all = TRUE) %>%
  select(ccode, atopid, year, ally.spend)

# Drop missing values of the expenditure variable
# Necessary because spread will fill all missing values with zero,
# not just absent combinations as in the above membership matrix
state.mem.cap <- state.mem.cap[complete.cases(state.mem.cap$ally.spend), ]

# rescale the ally expenditures variable by two standard deviations
state.mem.cap$ally.spend <- rescale(state.mem.cap$ally.spend)

# filter to ensure alliances match: 
state.mem.cap <- filter(state.mem.cap, atopid %in% alliance.char$atopid)


# This dataframe  contains the spending for the alliances states are a member of in a given year
state.mem.cap <- spread(state.mem.cap, key = atopid, value = ally.spend, fill = 0)




