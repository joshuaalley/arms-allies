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


### This First section creates data on state membership in alliances
# Uses ATOP data from alliance-measures script
########

# select key variables
alliance.char <- select(atop, atopid,
                    begyr, endyr,
                    uncond.milsup, scope.index, latent.scope.mean,
                    offense, defense, consul, neutral, nonagg, base,
                    armred.rc, organ1, milaid.rc, us.mem, ussr.mem,
                    num.mem, nonagg.only, wartime, asymm, asymm.cap, low.kap.sc)

# Expand alliance characteristics data to make it alliance characteristic-year data
# Don't care about truncation here, just need to know if alliance is operational
alliance.char$endyr[alliance.char$endyr == 0] <- 2016


alliance.char$freq <- alliance.char$endyr - alliance.char$begyr
# Alliances that end in the same year have a value of 0, given those a value of 1
# and add one year to the other alliance years
alliance.char$freq <- alliance.char$freq +  1



# Expand the dataset, copying each observation according to the frequency variable
alliance.char.expand <- untable(alliance.char, alliance.char$freq)

# Create a year variable by using the number of exanded observations
# group data by country and ATOP alliance and count rows 
alliance.char.expand <- alliance.char.expand %>%
  group_by(atopid) %>%
  mutate(count = row_number() - 1)


alliance.char.expand$year = alliance.char.expand$begyr + alliance.char.expand$count




####
# Create a datasets with observation identifiers: selecting key varaibles from 
# atop member-level dataset
atop.mem <- select(atop.mem.full, atopid, member, yrent, yrexit) 
colnames(atop.mem)[2] <- "ccode"



# Expand atop-member-level data to make it alliance member -year data
# Don't care about truncation here, just need to know if alliance is operational
atop.mem$yrexit[atop.mem$yrexit == 0] <- 2016


atop.mem$freq <- atop.mem$yrexit - atop.mem$yrent
# Alliances that end in the same year have a value of 0, given those a value of 1
# and add one year to the other alliance years
atop.mem$freq <- atop.mem$freq +  1



# Expand the dataset, copying each observation according to the frequency variable
atop.mem.expand <- untable(atop.mem, atop.mem$freq)

# Create a year variable by using the number of exanded observations
# group data by country, ATOP alliance and year of entry and count rows 
# Year of entry addresses cases like when Egypt is let back into the Arab League in 
atop.mem.expand <- atop.mem.expand %>%
  group_by(atopid, ccode, yrent) %>%
  mutate(count = row_number() - 1)


atop.mem.expand$year = atop.mem.expand$yrent + atop.mem.expand$count





# Merge the two alliance data types using ATOP ID and year
alliance.comp <- left_join(atop.mem.expand, alliance.char.expand, by = c("atopid", "year"))
alliance.comp <- unique(alliance.comp)
 


# Fill in missing alliance information (ATOP characteristics) with zeros
alliance.comp[is.na(alliance.comp)] <- 0



### The alliance.comp dataframe has alliance-year data for all ATOP alliances
# Countries appear in multiple years, so states are multiple members within alliances
# This next section of code brings in country-level data








#######
### This section creates a state-year dataset from 1815 to 2012. 
 

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

# Problems with Maddison Data
# 1. Austria-Hungary is just Austria: Maddison code 305 for COW code 300 (AUH)
maddison.gdp$ccode[maddison.gdp$country == "Austria" & maddison.gdp$year <= 1918] <- 300
# 2. Has both Russia and Former USSR. Code (SUN): Drop Russian Fed before 1989 and former USSR afterwards
maddison.gdp <- filter(maddison.gdp, !(countrycode == "SUN" & year > 1989)) # Remove USSR
maddison.gdp <- filter(maddison.gdp, !(countrycode == "RUS" & year <= 1989)) # Remove RUS FED
# 3. Doesn't split Germany into East and West
# 4. No Somalia or Eritrea data
# 5. Nothing on Yemen Arab republic or Yemen People's Republic. 
# 6. Nothing for Bhutan


# Fix Serbia/Yugoslavia country code problem
# First remove extrapolated Yugolav values
maddison.gdp <- filter(maddison.gdp, !(countrycode == "YUG" & year >= 1992))
# then remove serbia values prior to 1992
maddison.gdp <- filter(maddison.gdp, !(countrycode == "SRB" & year <= 1992))
# last, give serbia country code 345
maddison.gdp$ccode[maddison.gdp$countrycode == "SRB"] <- 345
  
# Left join state characteristics and gdp data
gdp.data <- select(maddison.gdp, ccode, year, cgdppc, ln.gdp, gdp)

state.vars <- left_join(state.vars, gdp.data)


# To solve missing GDP data from state splits in post-45 era, get GDP data from DiGiuseppe and Poast. 
state.vars <- left_join(state.vars, select(dg.poast, ccode, year, LNRGDP)) 
state.vars$ln.gdp[is.na(state.vars$ln.gdp)] <- state.vars$LNRGDP[is.na(state.vars$ln.gdp)]
state.vars <- select(state.vars, -c(LNRGDP))


# Add data on participation in inter-state war
interstate.war <- read.csv("data/interstate-war-rev.csv")


# Select key vars
interstate.war <- interstate.war %>%
      mutate(atwar = 1) %>%
      select(init_ccode, target_ccode, year, init_war_id, atwar) %>%
      gather(initiator, ccode, c(init_ccode, target_ccode)) %>%
      mutate(initiator = ifelse(initiator == "init_ccode", 1, 0)) %>%
      group_by(ccode, year) %>%
      summarize(
        atwar = max(atwar),
        initiator = max(initiator) # whether state initiated any conflict in a given year
      )


# Merge conflict data with other state variables
state.vars <- left_join(state.vars, interstate.war)
# Replace missing var values with zero (only merged 1s)
state.vars$atwar[is.na(state.vars$atwar)] <- 0
state.vars$initiator[is.na(state.vars$initiator)] <- 0




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
table(state.vars$atwar, state.vars$civilwar.part)


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
                        (ccode == 365 & year >= 1922) | # Russia: 1922 to present
                        (ccode == 220 & year <= 1940) | # France 1816 to 1940
                        (ccode == 220 & year >= 1945) | # France 1945 to present 
                        (ccode == 710 & year >= 1950) | # China
                        (ccode == 300 & year <= 1918) | # Austria-Hungary
                        (ccode == 325 & year >= 1860 & year <= 1943) | # Italy
                        (ccode == 740 & year <= 1945 & year >= 1895) | # Japan 1895 to 1940
                        (ccode == 740 & year >= 1991), # Japan 1991 to present
                        1, 0),
                     super.power = ifelse(
                       (ccode == 2 & year >= 1945) | # US 1945 to present
                      (ccode == 365 & year >= 1945 & year <= 1991), # Russia: 1992 to present
                     1, 0)
                    )
# Create a new variable marking superpower status
state.vars$status <- 0 # all non-major powers
state.vars$status[state.vars$majpower == 1 & state.vars$super.power == 0] <- 1 # major powers not superpowers
state.vars$status[state.vars$super.power == 1] <- 2 # id the superpowers 



# Add data on annual MIDs participation
# Load Gibler, Miller and Little's recoding of the MID data: directed-dyad data
gml.mid.part <- read.csv("data/gml-ddy-disputes-2.03.csv")

# Group data, collapse and get total disputes by year
gml.mid.part <- gml.mid.part %>%
                mutate(mid = 1) %>%
                select(ccode1, ccode2, year, dispnum, sidea2, mid) %>% 
                gather(side, ccode, c(ccode1, ccode2)) %>%
                select(-side)

gml.mid.part <- unique(gml.mid.part) 
gml.mid.part <- gml.mid.part %>%
                group_by(ccode, year) %>%
                summarize(
                mid.pres = 1,
                disputes = n()
                 )

# merge with state variables 
state.vars <- left_join(state.vars, gml.mid.part)
# Replace missing with zeros
state.vars$mid.pres[is.na(state.vars$mid.pres)] <- 0
state.vars$disputes[is.na(state.vars$disputes)] <- 0







# Remove pacific micro-states (comprehensive missing data, not random)
state.vars <- filter(state.vars, ccode <= 920) # New Zealand is ccode 920.


# Add log-transformed military expenditure variable and its lag. 
summary(state.vars$milex)
ggplot(state.vars, aes(x = milex)) + geom_density()
state.vars <- mutate(state.vars,
                     ln.milex = log(milex + 1)
)
summary(state.vars$ln.milex)
ggplot(state.vars, aes(x = ln.milex)) + geom_density()

# check for duplicate group indicators 
state.vars <- unique(state.vars)
duplicates <- state.vars[which(duplicated(state.vars[,c('ccode', 'year')])==T),] # this should be empty
rm(duplicates)


# Create a lagged expenditures variable within each panel 
# Then take differences and calculate growth
state.vars <- state.vars %>%
  group_by(ccode) %>% 
  mutate(lag.ln.milex = lag(ln.milex),
         lag.milex = lag(milex),
         change.milex = milex - lag.milex,
         change.ln.milex = ln.milex - lag.ln.milex,
         growth.milex = change.milex / lag.milex
  ) %>%
  group_by()

# Check growth 
summary(state.vars$growth.milex)
ggplot(state.vars, aes(x = growth.milex)) + geom_density()

# Trim the exceedingly large values, including infinite
state.vars$growth.milex[state.vars$growth.milex > 140] <- 140

# Concerns, given noise in COW measure: 
# cases where growth is -1 or close. Implies elimination of military budget
# cases with infinite change- 0 to some spending: newly indep states and fluctuations in budget
# Other large positive growth rates are less concerning- part of wartime increases
# Place both these types of observations in their respective state TS to check them
# Measurement error model would be essential. SIPRI 1949-2016 from Zielinski et al would be less noisy


# What does inverse hyperbolic sine transformation look like? 
ggplot(state.vars, aes(x = asinh(growth.milex))) + geom_density()
ggplot(state.vars, aes(x = asinh(growth.milex))) + geom_histogram(bins = 45)

# Create a growth in GDP variable
state.vars <- state.vars %>%
  group_by(ccode) %>% 
  mutate(lag.ln.gdp = lag(ln.gdp),
         lag.gdp = lag(gdp),
         change.gdp = (gdp - lag.gdp),
         gdp.growth = change.gdp / lag.gdp
  ) %>%
  group_by()
summary(state.vars$gdp.growth)
ggplot(state.vars, aes(x = gdp.growth)) + geom_density()

# Add data on rival military spending. 
# Coded rivalry in directed dyad data
td.rivalry <- read.csv("data/thompson-dreyer-rivalry.csv")

# Merge in data on military spending to second state in rivalry. 
td.rivalry <- td.rivalry %>%  
  rename(ccode = ccode2) %>%
  group_by(ccode1, ccode) %>%
  left_join(select(state.vars, ccode, year, ln.milex)) 

td.rivalry.annual <- td.rivalry %>%
  filter(rivalry == 1) %>% 
  group_by(ccode1, year) %>%
  summarize(
    total.rivals = sum(rivalry, na.rm = TRUE),
    rival.milex = sum(ln.milex, na.rm = TRUE),
    avg.rival.milex = rival.milex / total.rivals
  ) %>%
  group_by() 
colnames(td.rivalry.annual)[1] <- "ccode"


# Merge rival spending data in state variables 
state.vars <- left_join(state.vars, td.rivalry.annual)  
# fill missing rivalry data  with 0s
state.vars$total.rivals[is.na(state.vars$total.rivals)] <- 0
state.vars$rival.milex[is.na(state.vars$rival.milex)] <- 0
state.vars$avg.rival.milex[is.na(state.vars$avg.rival.milex)] <- 0


# export data to test of public goods theory
write.csv(state.vars, 
          "C:/Users/jkalley14/Dropbox/Research/Dissertation/public-goods-test/data/state-vars.csv", 
          row.names = F)



## TODO(JOSH)
# Get a state variables dataset with key stuff, and run multiple imputation






### This Section combines state characteristics and alliance data
#####
# Needed for a state-year measure of allied capability and to create the matrix of state membership in alliances

# Merge the state characteristics and alliance data
atop.cow.year <- right_join(alliance.comp, state.vars)

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
atop.cow.year[4:26][is.na(atop.cow.year[, 4:26] & atop.cow.year$atopid == 0)] <- 0

# export data to test of public goods theory
write.csv(atop.cow.year, 
          "C:/Users/jkalley14/Dropbox/Research/Dissertation/public-goods-test/data/atop-cow-year.csv", 
          row.names = F)


# Create a dataset of state-year alliance membership in pacts with military suport:
atop.cow.year <- group_by(atop.cow.year, atopid, ccode, year)
state.mem <- atop.cow.year %>% 
              filter(defense == 1 | offense == 1) %>%
              select(atopid, ccode, year)

state.mem <-  mutate(state.mem, member = 1)
state.mem <- distinct(state.mem, atopid, ccode, year, .keep_all = TRUE)

# This matrix has a binary indicator of which alliances states are a member of in a given year
state.mem <- spread(state.mem, key = atopid, value = member, fill = 0)




# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year <- atop.cow.year %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  summarize(
    avg.democ = mean(polity2, na.rm = TRUE),
    total.cap = sum(cinc, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    num.mem = n()
  )

alliance.year[order(alliance.year$atopid, alliance.year$year),] 


# Merge mean democracy into alliance characteristics data
alliance.year <- alliance.year %>% 
                  group_by(atopid) %>%
                  mutate(
                    begyr = min(year)
                  )

alliance.democ <- filter(alliance.year, begyr == year) %>% 
                 select(c(atopid, avg.democ)) 

# merge with alliance characteristics data
alliance.char <- left_join(alliance.char, alliance.democ, by = "atopid")
summary(alliance.char$avg.democ)
alliance.char$avg.democ[is.nan(alliance.char$avg.democ)] <- 0 # alliances where some members are not in system
ggplot(alliance.char, aes(x = avg.democ)) + geom_histogram()


# Create a membership matrix with the spending of all 
# other alliance members in place of 1s 


# With na.rm = TRUE, all missing values have a sum of zero.
# I filter out all of these alliance-year observations
alliance.year <- filter(alliance.year, total.expend != 0)

ggplot(alliance.year, aes(x = total.expend)) + geom_density()

# Replace missing NA values with 0 in atop.cow.year data
# This means mutate below won't produce missing values when state military spending is missing. 
atop.cow.year$ln.milex[is.na(atop.cow.year$ln.milex)] <- 0

# Create the dataset
state.mem.cap <- atop.cow.year %>% 
  filter(defense == 1 | offense == 1) %>%
  select(atopid, ccode, year, ln.milex) %>% 
  left_join(alliance.year) %>%
  mutate(alliance.contrib = ln.milex / total.expend) %>%
   mutate( ally.spend = total.expend - ln.milex) %>%
  distinct(atopid, ccode, year, .keep_all = TRUE) %>%
  select(ccode, atopid, year, ally.spend, avg.democ, alliance.contrib) 

# Replace missing values with zero if atopid = 0 (no alliance)
state.mem.cap$ally.spend[is.na(state.mem.cap$ally.spend) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$alliance.contrib[is.na(state.mem.cap$alliance.contrib) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$avg.democ[is.na(state.mem.cap$avg.democ) & state.mem.cap$atopid == 0] <- 0


# export data to test of public goods theory
write.csv(state.mem.cap, 
          "/../Research/Dissertation/public-goods-test/data/state-mem-cap.csv", 
          row.names = F)


# Add allied spending to state-alliance-year data: single level regressions
state.ally.year <- left_join(atop.cow.year, state.mem.cap)

# Write to Olson and Zeckhauser reanalysis paper
write.csv(state.ally.year, 
          "/../Research/Dissertation/public-goods-test/data/alliance-state-year.csv", 
          row.names = F)




# Drop missing values of the expenditure variable
# Necessary because spread will fill all missing values with zero,
# not just absent combinations as in the above membership matrix
summary(state.mem.cap$ally.spend)
state.mem.cap <- state.mem.cap[complete.cases(state.mem.cap$ally.spend), ]

# filter to ensure alliances match: 
state.mem.cap <- filter(state.mem.cap, atopid %in% alliance.char$atopid)


# rescale the ally expenditures variable by two standard deviations
state.mem.cap$ally.spend <- rescale(state.mem.cap$ally.spend)

# Create another membership matrix with contribution to the alliance
state.mem.contrib <- select(state.mem.cap, atopid, ccode, year, alliance.contrib) %>%
                  spread(key = atopid, 
                            value = alliance.contrib, fill = 0)

# This dataframe  contains the spending for the alliances states are a member of in a given year
state.mem.cap <- select(state.mem.cap, atopid, ccode, year, ally.spend) %>%
  spread(key = atopid, value = ally.spend, fill = 0)




# Combine State and alliance data 
########



# Define a state-year level dataset with no missing observations
reg.state.data <- state.vars %>%
  select(ccode, year, growth.milex, 
         atwar, civilwar.part, rival.milex, gdp.growth, polity2, 
         cold.war, disputes, majpower) 

# Add state membership in alliances to this data
reg.state.data <-  left_join(reg.state.data, state.mem.cap) 


# Replace missing alliance values with zero 
reg.state.data[, 12: ncol(reg.state.data)][is.na(reg.state.data[, 12: ncol(reg.state.data)])] <- 0

# Remove observations with missing values
reg.state.comp <- reg.state.data[complete.cases(reg.state.data), ]


# Rescale the state-level regressors
reg.state.comp[, 4:10] <- lapply(reg.state.comp[, 4:10], 
                                 function(x) rescale(x, binary.inputs = "0/1"))


# Create separate datasets for major and non-major powers
# major powers
reg.state.comp.maj <- filter(reg.state.comp, majpower == 1)
# Create a matrix of major power membership in alliances (Z in STAN model)
state.mem.maj <- as.matrix(reg.state.comp.maj[, 12: ncol(reg.state.comp.maj)])
# remove alliances with no major power participation
state.mem.maj <- state.mem.maj[, colSums(state.mem.maj != 0) > 0]

# non-major powers
reg.state.comp.min <- filter(reg.state.comp, majpower == 0)
# Create a matrix of npn-major membership in alliances (Z in STAN model)
state.mem.min <- as.matrix(reg.state.comp.min[, 12: ncol(reg.state.comp.min)])
# remove alliances with no non-major power participation
state.mem.min <- state.mem.min[, colSums(state.mem.min != 0) > 0]




# Check the range and distribution of the DV
summary(reg.state.comp$growth.milex)
sd(reg.state.comp$growth.milex)
ggplot(reg.state.comp, aes(growth.milex)) + geom_density()



# Create a matrix of state membership in alliances (Z in STAN model)
state.mem.mat <- select(reg.state.comp, c(majpower, 12: ncol(reg.state.comp)))
# Create a major power index variable 
state.mem.mat$mp.id <- state.mem.mat %>% group_indices(majpower)
state.mem.mat <- select(state.mem.mat, - c(majpower))
# Create a row index 
state.mem.mat$obs <- as.numeric(row.names(state.mem.mat))

# reorder variables in dataframe
state.mem.mat <- select(state.mem.mat, obs, mp.id, everything())



# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% group_indices(ccode)
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp %>% group_indices(year)
# Create a major power index variable 
reg.state.comp$mp.id <- reg.state.comp %>% group_indices(majpower)





# Create the matrix of alliance-level variables
# Make the alliance characteristics data match the membership matrix
reg.all.data <- filter(alliance.char, atopid %in% colnames(state.mem.mat)) %>%
  select(atopid, latent.scope.mean, num.mem, low.kap.sc,
         avg.democ, wartime, asymm, asymm.cap)


# Remove alliances w/ missing FPsim- one member not in system
reg.all.data <- reg.all.data[complete.cases(reg.all.data), ] 
state.mem.mat <- state.mem.mat[, colnames(state.mem.mat) %in% reg.all.data$atopid]

