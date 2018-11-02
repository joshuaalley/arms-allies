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


### This First section creates data on state membership in alliances,
#   and incorporates both ATOP and the Benson's alliance classifications. 
########
# Load my combination of Benson's 2012 data and ATOP v4
alliance.char.full <- read.csv("data/alliance-types-benson.csv")


# Create variables for US and USSR membership
russ.mem <- apply(alliance.char.full[, 73:129], 1, function(x) ifelse(x == 365, 1, 0))
russ.mem <- t(russ.mem)
alliance.char.full$russ.mem <- rowSums(russ.mem, na.rm = TRUE)

# US
us.mem <- apply(alliance.char.full[, 73:129], 1, function(x) ifelse(x == 2, 1, 0))
us.mem <- t(us.mem)
alliance.char.full$us.mem <- rowSums(us.mem, na.rm = TRUE)

# Remove the US and Russian membership matrices from the environment
rm(russ.mem)
rm(us.mem)


# count number of members: non-missing membership variables
alliance.char.full$num.mem <-  apply(alliance.char.full[, 73:129], 1, function(x) sum(!is.na(x)))


# identify non-aggression only pacts
alliance.char.full <- mutate(alliance.char.full, nonagg.only = ifelse((nonagg == 1 & 
                                                offense != 1 & defense != 1 & 
                                                consul != 1 & neutral != 1), 1 , 0))


# Create an indicator of compellent alliances and another for alliances with none of Benson's conditions
# Further indicators of mixed alliances and general indicators of conditional/unconditional pacts
# Also, recode arms requirements and military aid variables from ATOP into dummy 
# variables that capture conditions where increases in arms spending are likely
alliance.char.full <- mutate(alliance.char.full,
                        compellent = ifelse((uncond_comp == 1 | cond_comp == 1), 1 , 0),
                        none = ifelse(prob_det == 0 & uncond_det == 0 & cond_det == 0 & compellent == 0, 1, 0),
                        number.types = prob_det + uncond_det + cond_det + compellent,
                        mixed = ifelse(number.types > 1, 1, 0),
                        conditional = ifelse(cond_det == 1 | cond_comp == 1 | pure_cond_det == 1, 1, 0),
                        unconditional = ifelse(uncond_comp == 1 | uncond_det == 1, 1, 0),
                        armred.rc = ifelse(armred == 2, 1, 0),
                        milaid.rc = ifelse(milaid >= 2, 1, 0)
                        )

# Remove surplus variables 
alliance.char.full <- select(alliance.char.full, - c(armred, milaid))

# Check overlap between consultation only, neutrality and none variable 
table(alliance.char.full$none, alliance.char.full$consul)
table(alliance.char.full$none, alliance.char.full$neutral)
sum(alliance.char.full$compellent)
table(alliance.char.full$uncond_comp, alliance.char.full$uncond_det)

# truncate atopidphase to get regular atopid
alliance.char.full$atopid <- trunc(alliance.char.full$atopidphase)

# select key variables
alliance.char <- select(alliance.char.full, atopid, atopidphase,
                    begyr, endyr,
                    uncond_comp, cond_comp, 
                    uncond_det, cond_det, 
                    prob_det, pure_cond_det,
                    discret_intervene, discret_milsupport,
                    bilat, wartime, conditio,
                    armred.rc, organ1, milaid.rc, us.mem, russ.mem,
                    num.mem, none, nonagg.only, number.types)
# Recode ATOP phaseid: take away the decimal point. 
alliance.char$atopidphase.rc <- alliance.char$atopidphase*10


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
  group_by(atopidphase) %>%
  mutate(count = row_number() - 1)


alliance.char.expand$year = alliance.char.expand$begyr + alliance.char.expand$count




####
# load the ATOP alliance-member data (This provides the basis for the alliance member matrix)
atop.mem <- read.csv("data/atop-member-level.csv")
atop.mem$atopidphase.rc <- (atop.mem$atopid*10) + atop.mem$phase 

# Create a datasets with observation identifiers
atop.mem <- select(atop.mem, atopid, atopidphase.rc, member, yrent, yrexit)

colnames(atop.mem) <- c("atopid", "atopidphase.rc", "ccode", "yrent", "yrexit")

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
  group_by(atopidphase.rc, ccode, yrent) %>%
  mutate(count = row_number() - 1)


atop.mem.expand$year = atop.mem.expand$yrent + atop.mem.expand$count





# Merge the two alliance data types using ATOP ID and year
alliance.comp <- left_join(atop.mem.expand, alliance.char.expand, by = c("atopid", "year"))
alliance.comp <- unique(alliance.comp)
 


# Remove non-aggression pacts only
summary(alliance.comp$nonagg.only)
alliance.comp <- filter(alliance.comp, nonagg.only != 1)
alliance.comp <- select(alliance.comp, -nonagg.only)



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
gdp.data <- select(maddison.gdp, ccode, year, cgdppc, ln.gdp)

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



# Add data on annual MIDs participation
# Load Gibler, Miller and Little's recoding of the MID data: directed-dyad data
gml.mid.part <- read.csv("data/gml-ddy-disputes-2.03.csv")

# Group data, collapse and get total disputes by year
gml.mid.part <- gml.mid.part %>%
                mutate(mid = 1) %>%
                select(ccode1, ccode2, year, dispnum, mid) %>% 
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
state.vars <- state.vars %>%
              group_by(ccode) %>% 
              mutate(lag.ln.milex = lag(ln.milex),
                     lag.milex = lag(milex)
              ) %>%
              group_by()

# create a differenced expenditure variable 
state.vars$change.milex <- state.vars$milex - state.vars$lag.milex
state.vars$change.ln.milex <- state.vars$ln.milex - state.vars$lag.ln.milex



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


# Remove pacific micro-states (comprehensive missing data, not random)
state.vars <- filter(state.vars, ccode <= 920) # New Zealand is ccode 920. 


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
atop.cow.year[4:33][is.na(atop.cow.year[, 4:33] & atop.cow.year$atopid == 0)] <- 0

# Export to another folder for alliances, capability and conflict paper
write.csv(atop.cow.year, 
          "C:/Users/jkalley14/Dropbox/Research/alliances-conflict-dyads/Benson 2011/atop-cow-year.csv", 
          row.names = F)

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

# Replace missing NA values with 0 in atop.cow.year data
# This means mutate below won't produce missing values when state military spending is missing. 
atop.cow.year$ln.milex[is.na(atop.cow.year$ln.milex)] <- 0

# Create the dataset
state.mem.cap <- atop.cow.year %>% 
  select(atopid, ccode, year, ln.milex) %>% 
  left_join(alliance.year) %>%
  mutate(ally.spend = total.expend - ln.milex) %>%
  distinct(atopid, ccode, year, .keep_all = TRUE) %>%
  select(ccode, atopid, year, ally.spend, avg.democ)

# Replace missing values with zero if atopid = 0 (no alliance)
state.mem.cap$ally.spend[is.na(state.mem.cap$ally.spend) & state.mem.cap$atopid == 0] <- 0


# Add allied spending to state-alliance-year data: single level regressions
state.ally.year <- left_join(atop.cow.year, state.mem.cap)

# Drop missing values of the expenditure variable
# Necessary because spread will fill all missing values with zero,
# not just absent combinations as in the above membership matrix
summary(state.mem.cap$ally.spend)
state.mem.cap <- state.mem.cap[complete.cases(state.mem.cap$ally.spend), ]

# filter to ensure alliances match: 
state.mem.cap <- filter(state.mem.cap, atopid %in% alliance.char$atopid)




# rescale the ally expenditures variable by two standard deviations
state.mem.cap$ally.spend <- rescale(state.mem.cap$ally.spend)


# This dataframe  contains the spending for the alliances states are a member of in a given year
state.mem.cap <- spread(state.mem.cap, key = atopid, value = ally.spend, fill = 0)




