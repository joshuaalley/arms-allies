# Joshua Alley
# Texas A&M University
# Constructing Dataset for Prelim Paper on arms-alliances tradeoff



# Set working directory to current folder
setwd(here::here())
getwd()


# Load packages
library(reshape)
library(dplyr)
library(zoo)


# Load Benson's 2011 Data
d.benson <- read.csv("data/alliance-types-benson.csv")
d.benson <- cbind.data.frame(d.benson[, 1:2], d.benson[,8:15 ])


# Load the ATOP data on alliance-level characteristics
atop <- read.csv("data/atop-alliancephase-level.csv")

# load the ATOP alliance-member data
atop.mem <- read.csv("data/atop-member-level.csv")


# Create a datasets with essential ATOP variables
atop.mem.key <- cbind.data.frame(atop.mem$atopid, atop.mem$member, atop.mem$yrent, atop.mem$yrexit,
                      atop.mem$offense, atop.mem$defense, atop.mem$neutral, atop.mem$consul, atop.mem$nonagg,           
                      atop.mem$bilat, atop.mem$wartime, atop.mem$conditio,
                      atop.mem$defcon, atop.mem$offcon, atop.mem$neucon, atop.mem$concon,
                      atop.mem$armred)

colnames(atop.mem.key) <- c("atopid", "ccode", "startyear", "endyear",
                            "offense", "defense", "neutral", "consul", "nonagg",
                            "bilat", "wartime", "conditional",
                            "defcon", "offcon", "neucon", "concon",
                            "armsreq")

summary(atop.mem.key$atopid)


# Add Chiba et al data to ATOP member alliance data
# Load Chiba et al Replication data 
chiba.etal <- read.csv("data/chiba-etal2015.csv")
chiba.etal <- cbind.data.frame(chiba.etal[, 1], chiba.etal[, 6], chiba.etal[, 3], chiba.etal[, 10])
colnames(chiba.etal) <- c("atopid", "dem.prop", "onlyconsul", "num.mem")

# Drop atopids greater than 6000, which are all missing
chiba.etal <- subset(chiba.etal, chiba.etal$atopid < 6000)


# merge chiba et al data with ATOP member data
atop.mem.key <- merge(atop.mem.key, chiba.etal, all.x = TRUE)


# Merge ATOP member and Benson data to create a dataset with alliance members
# and key conditions of a given ATOP alliance
# This is necessary because Benson's replication data only provides ATOP ids 
# and his dyadic data doesn't have the raw alliance types

# Pull the ATOP ID variable out as a dataframe to merge Benson's data on
atop.id <- cbind.data.frame(atop$atopidphase, atop$begyr)
colnames(atop.id) <- c("atopid", "startyear")


# Merge Benson's data with the regular ATOP indicator
# Use of ALL provides max chance of getting data in a merger
atop.ben <- merge(atop.id, d.benson, by = c("atopid", "startyear"), all = TRUE)

# truncate atopid phaseid variable to facilitate merging
 atop.ben$atopid <- trunc(atop.ben$atopid)
 
# Fill in missing data on alliance characteristics based on ATOP ID
atop.ben <- atop.ben[order(atop.ben$atopid, atop.ben$startyear, atop.ben$uncond_comp), ]
atop.ben[3:10] <- na.locf(atop.ben[3:10])

# remove duplicates
 unique(atop.ben)
 atop.ben.unique <- unique(atop.ben)
 
 # Some rows with missing data shows up as unique
 atop.ben.unique <- atop.ben.unique[complete.cases(atop.ben.unique), ] 


# Merge the two alliance data types using ATOP ID and starting year
alliance.comp <- merge(atop.mem.key, atop.ben.unique, all.x = TRUE)


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

# Check the missing data issue by ATOPID
# Major chunk of missing data for some alliances, including ATOPIDs = 
# 1160, 1165, 1260, 1355, 2015, 2375, 2550, 3075, 3130, 3015, 3180, 3205


# Sort by country code to fill in missing data from these alliances
# Missing data is the result of a start-date merger issue
alliance.comp.expand <- alliance.comp.expand[
  order(alliance.comp.expand$ccode, alliance.comp.expand$atopid, alliance.comp.expand$year), ]

# Again, fill in missing alliance information with info from same alliance
alliance.comp.expand <- na.locf(alliance.comp.expand)
alliance.comp.expand <- alliance.comp.expand[complete.cases(alliance.comp.expand), ]

# Make sure no duplicate observations from merging process
alliance.comp.expand <- unique(alliance.comp.expand)

### The alliance.comp.expand dataframe has alliance-year data for all ATOP alliances
# Countries appear in multiple years, so states are cross-classified within alliances
# This next section of code brings in country-level data
# For the country-level data, use DiGiuseppe and Poast's 2016 dataset
# This also has the advantage of ensuring our samples are comparable


# Start by subsettting the alliance data
alliance.1950 <- subset(alliance.comp.expand, 
                        alliance.comp.expand$year >= 1949 & alliance.comp.expand$year <= 2001)

summary(alliance.1950$year)

alliance.1950 <- alliance.1950[order(alliance.1950$ccode, alliance.1950$year, alliance.1950$atopid), ]



# Load Digiuseppe and Poast's data
dg.poast <- read.csv("data/dg-poast2016.csv")

# Pull out key variables from DiGiuseppe and Poast Data
state.char <- cbind.data.frame(dg.poast$ccode, dg.poast$year,
                                 dg.poast$majpow1, dg.poast$cap_1, dg.poast$rivalcap, dg.poast$rivalmil,
                                 dg.poast$rivalry, dg.poast$defense_dem, dg.poast$defense_nodem,
                                 dg.poast$nato, dg.poast$atwar, dg.poast$civilwar, dg.poast$DEMOC, 
                                 dg.poast$LNRGDP, dg.poast$LMILEX, dg.poast$LMILEX1,
                                 dg.poast$atopally_capsum, dg.poast$atopally_milsum, dg.poast$threatenv)

colnames(state.char) <- c("ccode", "year", 
                            "majpower", "CINC", "rival.cap", "rival.mil",
                            "rivalry", "defense.dem", "defense.nodem",
                            "nato", "atwar", "civilwar", "polity",
                            "ln.GDP", "ln.milex", "lag.ln.milex",
                            "totalcap.atop.ally", "totalmilex.atop.ally", "ls.threatenv")

summary(state.char$year)
state.char <- subset(state.char, state.char$year <= 2001)

# Merge the state characteristics and alliance data
full.data <- right_join(alliance.1950, state.char)

# Change order of variables for ease in viewing
full.data <- full.data[c(1, 3, 30, 2, 4, 5:29, 31:47)]

# Ensure no duplicate observations are present after merging- not an issue here
full.data <- unique(full.data)

# Sort data 
full.data[order(full.data$ccode, full.data$year, full.data$atopid), ]


# Some missing alliance characteristics data for states that are not members of an ATOP alliance
# Replace ATOP indicator with zero if missing
full.data$atopid[is.na(full.data$atopid)] <- 0

# If no ATOP alliance, fill all other alliance characteristic variables with a zero.
full.data[6:28][is.na(full.data[, 6:28] & full.data$atopid == 0)] <- 0

# States with no alliances in a year are given an alliance ID of zero, grouping them all together



