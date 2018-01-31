# Joshua Alley
# Texas A&M University
# Constructing Dataset for Prelim Paper on arms-alliances tradeoff


# Load packages
library(here)
library(reshape)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)


# Set working directory to current folder
setwd(here::here())
getwd()



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
# Countries appear in multiple years, so states are multiple members within alliances
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

# Create a differences in military expenditure variable
# Start by taking the military expenditures data and reversing the log transformation
state.char$milex <- exp(state.char$ln.milex)
state.char$lag.milex <- exp(state.char$lag.ln.milex)
# create the differences 
state.char$change.milex <- state.char$milex - state.char$lag.milex
state.char$change.ln.milex <- state.char$ln.milex - state.char$lag.ln.milex

# Merge the state characteristics and alliance data
full.data <- right_join(alliance.1950, state.char)

# Change order of variables for ease in viewing
full.data <- full.data %>% 
  select(atopid, ccode, year, everything())

# Ensure no duplicate observations are present after merging- not an issue here
full.data <- unique(full.data)

# Sort data 
full.data[order(full.data$ccode, full.data$year, full.data$atopid), ]


# Some missing alliance characteristics data for states that are not members of an ATOP alliance
# Replace ATOP indicator with zero if missing
full.data$atopid[is.na(full.data$atopid)] <- 0

# If no ATOP alliance, fill all other alliance characteristic variables with a zero.
full.data[6:29][is.na(full.data[, 6:29] & full.data$atopid == 0)] <- 0

# States with no alliances in a year are given an alliance ID of zero, grouping them all together



# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year <- full.data %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  summarize(
    avg.democ = mean(polity, na.rm = TRUE),
    total.cap = sum(CINC, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    count = n(),
    consul.only = max(onlyconsul, na.rm = TRUE),
    prob.det = max(prob_det, na.rm = TRUE),
    num.mem = mean(num.mem, na.rm = TRUE),
    offense = max(offense, na.rm = TRUE),
    defense = max(defense, na.rm = TRUE),
    neutral = max(neutral, na.rm = TRUE),
    nonagg = max(nonagg, na.rm = TRUE),
    consul = max(consul, na.rm = TRUE),
    wartime = max(wartime, na.rm = TRUE),
    uncond_comp = max(uncond_comp, na.rm = TRUE),
    cond_comp = max(cond_comp, na.rm = TRUE),
    uncond_det = max(uncond_det, na.rm = TRUE),
    cond_det = max(cond_det, na.rm = TRUE),
    discret_milsupport = max(discret_milsupport, na.rm = TRUE),
    discret_intervene = max(discret_intervene, na.rm = TRUE)
  )

alliance.year[order(alliance.year$atopid, alliance.year$year), ]


# merge the avg democracy, total capability, and total military expenditures variables into the full data
alliance.totals <- alliance.year[, 1:5]
full.data <- left_join(full.data, alliance.totals)

# Fill in totals with zero for state-years with no alliance
full.data[48:50][is.na(full.data[, 48:50] & full.data$atopid == 0)] <- 0


### Add a couple more variables to the full data
# binary indicator of compellent alliances
full.data$compellent <- ifelse(full.data$uncond_comp == 1 | full.data$cond_comp == 1, 1, 0)

# Binary indicator of deterrent alliances
full.data$deterrent <- ifelse(full.data$cond_det == 1 | full.data$uncond_det == 1 |
                              full.data$pure_cond_det == 1 | full.data$prob_det == 1,
                              1, 0)
# binary indicator of conditional alliances in the benson typology
full.data$ben.cond <- ifelse(full.data$cond_det == 1 | full.data$cond_comp == 1, 1, 0)

# Mark each unique state-year observation in the data with its own indicator 
full.data <- full.data %>% group_by(ccode, year)
full.data$state.year.id <- full.data %>% group_indices(ccode, year)

# Remove observation with missing ccode
full.data <- full.data[complete.cases(full.data$ccode), ]


# The full dataset can also provide the basis of a state-year characteristics dataset 
# with some summary variables for the alliance portfolio
# Can then merge this with the state characteristics dataset
state.ally.year <- full.data %>%
  group_by(ccode, year) %>%
  summarize(
    treaty.count = n(),
    prob.det.pres = max(prob_det, na.rm = TRUE),
    prob.det.total = sum(prob_det, na.rm = TRUE),
    consul.only.pres = max(onlyconsul, na.rm = TRUE),
    consul.only.total = sum(onlyconsul, na.rm = TRUE),
    bilat.total = sum(bilat, na.rm = TRUE),
    uncond.comp.pres = max(uncond_comp, na.rm = TRUE),
    uncond.comp.total = sum(uncond_comp, na.rm = TRUE),
    cond.comp.pres = max(cond_comp, na.rm = TRUE),
    cond.comp.total = sum(cond_comp, na.rm = TRUE),
    uncond.det.pres = max(uncond_det, na.rm = TRUE),
    uncond.det.total = sum(uncond_det, na.rm = TRUE),
    cond.det.pres = max(cond_det, na.rm = TRUE),
    cond.det.total = sum(cond_det, na.rm = TRUE),
    avg.dem.prop = mean(dem.prop, na.rm = TRUE),
    arms.req = max(armsreq, na.rm = TRUE),
    avg.num.mem = mean(num.mem, na.rm = TRUE),
    defense.total = sum(defense, na.rm = TRUE),
    offense.total = sum(offense, na.rm = TRUE)
  )

state.char.full <- left_join(state.char, state.ally.year)



# Create a dataset of state-year alliance membership:
state.mem <- full.data %>% select(atopid, ccode, year)
state.mem <-  mutate(state.mem, member = 1)
state.mem <- distinct(state.mem, .keep_all = TRUE)

# This matrix has a binary indicator of which alliances states are a member of in a given year
state.mem <- spread(state.mem, key = atopid, value = member, fill = 0)

# Remove the zero or no alliance category
state.mem <- subset(state.mem, select = -(3))



######
# This section summarizes the data using descriptive statistics and plots

# Start by grouping the full dataset by country and alliance
full.data <- full.data %>%
  group_by(atopid, ccode, year)



# DV: ln(military expenditure)
summary(full.data$ln.milex)
ggplot(full.data, aes(ln.milex)) + geom_density()
ggplot(state.char, aes(ln.milex)) + geom_density()

ggplot(state.char, aes(milex)) + geom_density()

# Which states have the smallest military expenditures? 
# At least one has democratic alliances
# Data for states with negative log military expenditures: Iceland and Solomon Islands
small.spend <- filter(state.char, state.char$ln.milex < 0)
small.spend <- filter(state.char, state.char$ln.milex < 1)


# IV1: Probabilistic Alliances
summary(full.data$prob_det)
summary(alliance.1950$prob_det)
# Differences in expenditure
t.test(full.data$ln.milex ~ full.data$prob_det, alternative = "less")
t.test(full.data$change.milex ~ full.data$prob_det, alternative = "less")
# Split data and plot
full.data %>% 
  select(ln.milex, prob_det) %>%
ggplot(mapping = aes(x = prob_det, y = ln.milex)) +
  geom_boxplot(mapping = aes(group = prob_det))

# IV2: Consultation only
summary(full.data$onlyconsul)
summary(alliance.1950$onlyconsul)
# Differences in expenditure
t.test(full.data$ln.milex ~ full.data$onlyconsul, alternative = "less")
t.test(full.data$change.milex ~ full.data$onlyconsul, alternative = "less")
# Split data and plot
full.data %>% 
  select(ln.milex, onlyconsul) %>%
ggplot(mapping = aes(x = onlyconsul, y = ln.milex)) +
  geom_boxplot(mapping = aes(group = onlyconsul))



### Summarize Benson's alliance typology in greater detail
# Discretion to intervene and provide military support are subsets of probabilistic deterrent treaties
# However, there are few observations where both are present
table(full.data$prob_det, full.data$discret_intervene)
table(full.data$prob_det, full.data$discret_milsupport)
table(full.data$discret_intervene, full.data$discret_milsupport)

# Overlap between probabilistic deterrent treaties and consultation only pacts
# Consultation on top, probabilistic deterrent on sides
table(full.data$prob_det, full.data$consul)




# Check alliance aggregates: the sums throw some funky values
# total capability
summary(full.data$total.cap)
ggplot(full.data) + geom_histogram(mapping = aes(x = total.cap))
# Total expenditures
summary(full.data$total.expend)
ggplot(full.data) + geom_histogram(mapping = aes(x = total.expend))
# Average democracy
summary(full.data$avg.democ)
ggplot(full.data) + geom_histogram(mapping = aes(x = avg.democ))


#### scatterplot of association between GDP and expenditure
# within the state characteristics data
ggplot(state.char) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex))
ggplot(state.char, mapping = aes(x = ln.GDP, y = ln.milex)) + 
  geom_point() + 
  geom_smooth()

# Defense pacts with democracies
ggplot(state.char) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, color = defense.dem))

ggplot(state.char, aes(ln.GDP, ln.milex, group = defense.dem, colour = defense.dem)) +
  geom_point() + 
  stat_smooth(method="lm")
# Major powers 
ggplot(state.char) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, color = majpower))


# Show the same association with full data:
# Compare probabilistic deterrent alliances
summary(full.data$prob_det)
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = prob_det))

ggplot(full.data, aes(ln.GDP, ln.milex, group =  prob_det, colour = prob_det)) +
  geom_point() + 
  stat_smooth(method="lm")

# Only consultation alliances
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = onlyconsul))

ggplot(full.data, aes(ln.GDP, ln.milex, group =  onlyconsul, colour = onlyconsul)) +
  geom_point() + 
  stat_smooth(method="lm")

# Proportion of democracies in an alliance
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = dem.prop))

# Average polity score within an alliance
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = avg.democ))

# Total capability of an alliance
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = total.cap))

# Conditional on offensive or defensive pacts
# Defensive
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = defense))
ggplot(full.data, aes(ln.GDP, ln.milex, group =  defense, colour = defense)) +
  geom_point() + 
  stat_smooth(method="lm")
# Offensive 
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = offense))
ggplot(full.data, aes(ln.GDP, ln.milex, group =  offense, colour = offense)) +
  geom_point() + 
  stat_smooth(method="lm")


# Bilateral treaties
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = bilat))
ggplot(full.data, aes(ln.GDP, ln.milex, group =  bilat, colour = bilat)) +
  geom_point() + 
  stat_smooth(method="lm")


# Discretion over military support
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = discret_milsupport))
ggplot(full.data, aes(ln.GDP, ln.milex, group =  discret_milsupport, colour = discret_milsupport)) +
  geom_point() + 
  stat_smooth(method="lm")


# Discretion over intervention
ggplot(full.data) + geom_point(mapping = aes(x = ln.GDP, y = ln.milex, 
                                             color = discret_intervene))
ggplot(full.data, aes(ln.GDP, ln.milex, group =  discret_intervene, colour = discret_intervene)) +
  geom_point() + 
  stat_smooth(method="lm")

### Spending over time
ggplot(full.data) + geom_point(mapping = aes(x = year, y = ln.milex))

# Probabilistic alliances over time
ggplot(full.data) + 
  geom_point(mapping = aes(x = year, y = ln.milex,
                           color = prob_det), position = "jitter") +
  ggtitle("Probabilistic Alliances and Spending")


# Consultation pacts over time
ggplot(full.data) + 
  geom_point(mapping = aes(x = year, y = ln.milex,
                           color = onlyconsul), position = "jitter") +
  ggtitle("Consultation Alliances and Spending")



# Smoothed spending and regression lines over time
ggplot(state.char) + geom_smooth(mapping = aes(x = year, y = ln.milex))
ggplot(state.char, mapping = aes(x = year, y = ln.milex)) +
                  geom_point() +
                  geom_smooth() + ggtitle("Spending over time: State-Year")

# smoothed lines won't be as informative because states with more alliances will have the same point appear multiple times
# Plot those regardless
ggplot(full.data) + geom_smooth(mapping = aes(x = year, y = ln.milex))
ggplot(full.data, mapping = aes(x = year, y = ln.milex)) +
  geom_point() +
  geom_smooth() + ggtitle("Spending over time: Full Sample")


# Regression conditional on probabilistic deterrent
ggplot(full.data, aes(year, ln.milex, group =  prob_det, colour = prob_det)) +
  geom_point() + 
  stat_smooth(method="lm")


# Regression conditional on Consultation pacts
ggplot(full.data, aes(year, ln.milex, group =  onlyconsul, colour = onlyconsul)) +
  geom_point() + 
  stat_smooth(method="lm")



### Look at number of members and spending variables
ggplot(full.data, aes(num.mem, ln.milex)) + geom_point()

# Conditional regression by probabilistic deterrenct
ggplot(full.data, aes(num.mem, ln.milex, group =  prob_det, colour = prob_det)) +
  geom_point() + 
  stat_smooth(method="lm")

# Conditional regression by consultation only treaties
ggplot(full.data, aes(num.mem, ln.milex, group =  onlyconsul, colour = onlyconsul)) +
  geom_point() + 
  stat_smooth(method="lm")



#### Plot changes in military spending

# Change in spending
summary(full.data$change.milex)
ggplot(full.data, aes(change.milex)) + geom_density()
ggplot(state.char, aes(change.milex)) + geom_density()

# Changes in the natural log of spending- still some crazy outliers, but not as extreme
summary(full.data$change.ln.milex)
ggplot(full.data, aes(change.ln.milex)) + geom_density()
ggplot(state.char, aes(change.ln.milex)) + geom_density()


# Plot changes against year
ggplot(full.data, aes(year, change.ln.milex)) + geom_point()
# Color conditional on probabilistic deterrent pacts
ggplot(full.data, aes(year, change.ln.milex, group =  prob_det, colour = prob_det)) +
  geom_point()
# Color conditional on consultation only
ggplot(full.data, aes(year, change.ln.milex, group =  onlyconsul, colour = onlyconsul)) +
  geom_point()


