# Joshua Alley
# Texas A&M University
# Constructing Dataset for Prelim Paper on arms-alliances tradeoff
# Covers military spending from 1945 to 2001 

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

# Add a Cold War variable 
state.char$cold.war <- ifelse(state.char$year >= 1949 & state.char$year <= 1990, 1, 0)

# Add some proxies for entrapment risk
entrapment.vars <- read.csv("data/conflict-risk-data.csv")
# Turn leader-level data intro state-level data
entrapment.vars <- group_by(entrapment.vars, ccode, year) %>%
            summarize(
              rebel = max(rebel, na.rm = FALSE),
              borders = max(borders, na.rm = FALSE),
              disputes = max(disputes, na.rm = FALSE)
            )

state.char <- left_join(state.char, entrapment.vars)

# Create a differences in military expenditure variable
# Start by taking the military expenditures data and reversing the log transformation
state.char$milex <- exp(state.char$ln.milex)
state.char$lag.milex <- exp(state.char$lag.ln.milex)
# create the differences 
state.char$change.milex <- state.char$milex - state.char$lag.milex
state.char$change.ln.milex <- state.char$ln.milex - state.char$lag.ln.milex


# Look at differences between a rival expenditures and a state's expenditures, plus those of its allies
state.char$ally.milex.gap <- state.char$totalmilex.atop.ally - state.char$rival.mil
summary(state.char$ally.milex.gap)
ggplot(state.char, aes(ally.milex.gap)) + geom_histogram(bins = 60)


### 
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
full.data[6:36][is.na(full.data[, 6:36] & full.data$atopid == 0)] <- 0

# States with no alliances in a year are given an alliance ID of zero, grouping them all together



# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year <- full.data %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  summarize(
    avg.democ = mean(polity, na.rm = TRUE),
    total.cap = sum(CINC, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    num.mem = n()
  )

alliance.year[order(alliance.year$atopid, alliance.year$year), ]

# With na.rm = TRUE, all missing values have a sum of zero.
# I filter out all of these alliance-year observations
alliance.year <- filter(alliance.year, total.expend != 0)

ggplot(alliance.year, aes(x = total.expend)) + geom_density()



# merge the avg democracy, total capability, and total military expenditures variables into the full data
full.data <- left_join(full.data, alliance.year)

# Fill in totals with zero for state-years with no alliance
full.data[65: 67][is.na(full.data[, 65: 67] & full.data$atopid == 0)] <- 0

# Fill in alliance characteristics with zero if no alliance is present
full.data[27: 36][is.na(full.data[, 27: 36] & full.data$atopid == 0)] <- 0

### Add a couple more variables to the full data
# binary indicator of compellent alliances
full.data$compellent <- ifelse(full.data$uncond_comp == 1 | full.data$cond_comp == 1, 1, 0)

# Binary indicator of deterrent alliances
full.data$deterrent <- ifelse(full.data$cond_det == 1 | full.data$uncond_det == 1 |
                              full.data$pure_cond_det == 1 | full.data$prob_det == 1,
                              1, 0)
# binary indicator of conditional alliances in the benson typology
full.data$ben.cond <- ifelse(full.data$cond_det == 1 | full.data$cond_comp == 1, 1, 0)


# Remove observation with missing ccode
full.data <- full.data[complete.cases(full.data$ccode), ]


# Create variables for new alliance membership- for single-level model
full.data <- full.data %>% 
  group_by(ccode, year) %>% mutate(
    alliance.duration = year - startyear, 
    new.prob.det5 = ifelse(alliance.duration <= 5 & prob_det == 1, 1, 0),
    new.conditional5 = ifelse(alliance.duration <= 5 & conditional == 1, 1, 0),
    new.unconditional5 = ifelse(alliance.duration <= 5 & unconditional == 1, 1, 0), 
    new.compellent5 = ifelse(alliance.duration <= 5 & compellent == 1, 1, 0),
    
    new.prob.det10 = ifelse(alliance.duration <= 10 & prob_det == 1, 1, 0),
    new.conditional10 = ifelse(alliance.duration <= 10 & conditional == 1, 1, 0),
    new.unconditional10 = ifelse(alliance.duration <= 10 & unconditional == 1, 1, 0), 
    new.compellent10 = ifelse(alliance.duration <= 10 & compellent == 1, 1, 0)
)


# remove non-aggression pacts
full.data <- mutate(full.data, nonagg.only = ifelse((nonagg == 1 & offense != 1 & defense != 1 & consul != 1 & neutral != 1), 1 , 0))
summary(full.data$nonagg.only)

full.data.rnonagg <- filter(full.data, nonagg.only != 1)
# restrict sample to minor powers
full.data.rnonagg <- filter(full.data.rnonagg, majpower == 0)


# Create a dataset of state-year alliance membership:
full.data.rnonagg <- group_by(full.data.rnonagg, atopid, ccode, year)
state.mem <- full.data.rnonagg %>% select(atopid, ccode, year)
state.mem <-  mutate(state.mem, member = 1)
state.mem <- distinct(state.mem, atopid, ccode, year, .keep_all = TRUE)

# This matrix has a binary indicator of which alliances states are a member of in a given year
state.mem <- spread(state.mem, key = atopid, value = member, fill = 0)

# Remove the zero or no alliance category
state.mem <- subset(state.mem, select = -(3))


# Using total capability of an alliance, create a dataset of state-year alliance membership:
full.data.rnonagg <- group_by(full.data, atopid, ccode, year)

state.mem.cap <- full.data.rnonagg %>% 
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
library(arm)
state.mem.cap$ally.spend <- rescale(state.mem.cap$ally.spend)

# filter to ensure alliances match: 
state.mem.cap <- filter(state.mem.cap, atopid %in% alliance.char$atopid)


# This dataframe  contains the spending for the alliances states are a member of in a given year
state.mem.cap <- spread(state.mem.cap, key = atopid, value = ally.spend, fill = 0)

















######
# This section summarizes the data using descriptive statistics and plots

# DV: ln(military expenditure)
ggplot(state.char, aes(milex)) + geom_density()
ggplot(state.char, aes(ln.milex)) + geom_density()

# Fancy Plot
ggplot(state.char, aes(ln.milex)) + geom_histogram(bins = 45) +
  ggtitle("Histogram of Natural Log of Military Spending") +
  labs(x = "Natural Log of Military Spending") +
  theme_classic()

# Minor powers only 
state.char %>% 
  filter(majpower == 0) %>%
  ggplot(mapping = aes(milex)) + geom_density()

state.char %>% 
  filter(majpower == 0) %>%
  ggplot(mapping = aes(ln.milex)) + geom_density()


# Split data and plot: probabilistic
state.char.full %>% 
  select(change.ln.milex, prob.det.pres) %>%
  ggplot(mapping = aes(x = prob.det.pres, y = change.ln.milex)) +
  geom_boxplot(mapping = aes(group = prob.det.pres))

# Split data and plot: unconditional
state.char.full %>% 
  select(change.ln.milex, uncond.det.pres) %>%
  ggplot(mapping = aes(x = uncond.det.pres, y = change.ln.milex)) +
  geom_boxplot(mapping = aes(group = uncond.det.pres))





# Which states have the smallest military expenditures? 
# At least one has democratic alliances
# Data for states with negative log military expenditures: Iceland and Solomon Islands
small.spend <- filter(state.char, state.char$ln.milex < 0)
small.spend <- filter(state.char, state.char$ln.milex < 1)




### Summarize Benson's alliance typology in greater detail
# Discretion to intervene and provide military support are subsets of probabilistic deterrent treaties
# However, there are few observations where both are present
table(full.data$prob_det, full.data$discret_intervene)
table(full.data$prob_det, full.data$discret_milsupport)
table(full.data$discret_intervene, full.data$discret_milsupport)

# Overlap between probabilistic deterrent treaties and consultation only pacts
# Consultation on top, probabilistic deterrent on sides
table(full.data$prob_det, full.data$onlyconsul)

# Overlap between benson's typology and democratic defense pacts
table(full.data$prob_det, full.data$defense.dem)
table(full.data$cond_det, full.data$defense.dem)
table(full.data$uncond_det, full.data$defense.dem)




# Check alliance aggregates: the sums throw some funky values from the OCSE 
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




# Smoothed spending and regression lines over time
ggplot(state.char) + geom_smooth(mapping = aes(x = year, y = ln.milex))
ggplot(state.char, mapping = aes(x = year, y = ln.milex)) +
                  geom_point() +
                  geom_smooth() + ggtitle("Spending over time: State-Year")





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


# Changes in spending for milex

# Changes in the natural log of spending- still some crazy outliers, but not as extreme
summary(full.data$change.ln.milex)
ggplot(full.data, aes(change.ln.milex)) + geom_density()
ggplot(state.char, aes(change.ln.milex)) + geom_density()

# Changes in expenditure for minor powers only
state.char %>% 
  filter(majpower == 0) %>%
    ggplot(mapping = aes(change.milex)) + geom_density()

state.char %>% 
  filter(majpower == 0) %>%
    ggplot(mapping = aes(change.ln.milex)) + geom_density()



# Plot changes against year
ggplot(full.data, aes(year, change.ln.milex)) + geom_point()
# Color conditional on probabilistic deterrent pacts
ggplot(full.data, aes(year, change.ln.milex, group =  prob_det, colour = prob_det)) +
  geom_point()
# Color conditional on consultation only
ggplot(full.data, aes(year, change.ln.milex, group =  onlyconsul, colour = onlyconsul)) +
  geom_point()


