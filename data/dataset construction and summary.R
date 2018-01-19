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

# Create a year variable
alliance.comp.expand$year <- alliance.comp.expand$startyear + 
  floor((as.numeric(rownames(alliance.comp.expand)) %% 1)*10)

# Check the missing data issue by ATOPID

# Major chunk of missing data for some alliances, including ATOPIDs = 
# 1160, 1165, 1260, 1355, 2015, 2375, 2550, 3075, 3130, 3015, 3180, 3205


# Sort by country code to fill in missing data from these alliances
# Missing data is the result of a start-date merger issue
alliance.comp.expand <- alliance.comp.expand[
  order(alliance.comp.expand$atopid, alliance.comp.expand$year), ]

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
                        alliance.comp.expand$year >= 1950 & alliance.comp.expand$year <= 2001)

alliance.1950 <- alliance.1950[order(alliance.1950$ccode, alliance.1950$year, alliance.1950$atopid), ]

# Pull out key variables from DiGiuseppe and Poast Data
