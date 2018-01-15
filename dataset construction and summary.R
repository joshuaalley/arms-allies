# Joshua Alley
# Texas A&M University
# Constructing Dataset for Prelim Paper on arms-alliances tradeoff



# Set working directory to current folder
setwd(here::here())
getwd()


# Load packages


# Load Benson's 2011 Data
d.benson <- read.csv("data/alliance-typology-benson.csv")


# Load the ATOP data
atop <- read.csv("data/atop-alliance-level.csv")

# Merge ATOP and Benson data
alliance.comp <- merge(d.benson, atop, by = "atopid")
