# Joshua Alley
# Texas A&M University
# Constructing Dataset for Prelim Paper on arms-alliances tradeoff
# Covers military spending from 1945 to 2001 


### The alliance.comp.expand dataframe has alliance-year data for all ATOP alliances
# Countries appear in multiple years, so states are multiple members within alliances
# Bring in country-level data, use DiGiuseppe and Poast's 2016 dataset


# Start by subsettting the alliance data
alliance.1950 <- filter(atop.mem.expand, year >= 1949)

summary(alliance.1950$year)

alliance.1950 <- alliance.1950[order(alliance.1950$ccode, alliance.1950$year, alliance.1950$atopid), ]

# focus on military alliances only
alliance.1950 <- filter(alliance.1950, atopid %in% atop.milsup$atopid)



### Load in SIPRI data
sipri.data <- read.csv("data/sipri-milex-data.csv")

sipri.data <- gather(sipri.data, key = "year", value = "sipri.milex", -Country)

# remove the x from the year values
sipri.data$year = as.numeric(gsub("\\X", "", sipri.data$year))

# add country codes to the sipri data
sipri.data$ccode <- countrycode(sipri.data$Country, 'country.name', 'cown')

# fix some of the missing values and other ccode problems
sipri.data$ccode[sipri.data$Country == "Central African Rep."] <- 482
sipri.data$ccode[sipri.data$Country == "eSwatini"] <- 572 # former swaziland
sipri.data$ccode[sipri.data$Country == "German DR"] <- 265
sipri.data$ccode[sipri.data$Country == "Serbia" & sipri.data$year >= 1996] <- 345 # matches Yugoslavia coding

# filter out duplicate ccode values 
sipri.data <- filter(sipri.data, !(Country == "Serbia" & year < 1996)) # remove pre-1996 Serbia observations
sipri.data <- filter(sipri.data, !(Country == "Russia" & year < 1993)) # remove pre-1996 Serbia observations


# Create some transformed values of sipri spending
ggplot(sipri.data, aes(x = sipri.milex)) + geom_histogram(bins = 45)
sipri.data$ln.sipri.milex <- log(sipri.data$sipri.milex + 1)
ggplot(sipri.data, aes(x = ln.sipri.milex)) + geom_histogram(bins = 45)

# Create lags 
sipri.data <- sipri.data %>%
  group_by(ccode) %>% 
  mutate(lag.ln.sipri.milex = lag(ln.sipri.milex),
         lag.sipri.milex = lag(sipri.milex),
         change.sipri.milex = sipri.milex - lag.sipri.milex,
         change.ln.sipri.milex = ln.sipri.milex - lag.ln.sipri.milex,
         growth.sipri.milex = change.sipri.milex / lag.sipri.milex
  ) %>%
  group_by()

# Loaded SIPRI, but missing Eastern Euro Cold War data is a problem

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
# cut missing data- using Nordhaus et al data (through 2001)
state.char <- filter(state.char, year <= 2001)

# Add a Cold War variable 
state.char$cold.war <- ifelse(state.char$year >= 1949 & state.char$year <= 1991, 1, 0)

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

ggplot(state.char, aes(change.ln.milex)) + geom_histogram(bins = 60)
ggplot(state.char, aes(asinh(change.ln.milex))) + geom_histogram(bins = 60)

# Create a growth variable
state.char$growth.milex <- state.char$change.milex / state.char$lag.milex

ggplot(state.char, aes(growth.milex)) + geom_histogram(bins = 60)
ggplot(state.char, aes(asinh(growth.milex))) + geom_histogram(bins = 60)

# Look at differences between a rival expenditures and a state's expenditures, plus those of its allies
state.char$ally.milex.gap <- state.char$totalmilex.atop.ally - state.char$rival.mil
summary(state.char$ally.milex.gap)
ggplot(state.char, aes(ally.milex.gap)) + geom_histogram(bins = 60)


# GDP growth 
state.char <- state.char %>%
              group_by(ccode) %>%
              mutate(
                lag.ln.gdp = lag(ln.GDP),
                change.ln.gdp = ln.GDP - lag.ln.gdp,
                growth.gdp = change.ln.gdp / lag.ln.gdp
              )
summary(state.char$growth.gdp)
summary(state.char$change.ln.gdp)
summary(state.char$lag.ln.gdp)

# Combine state char data and SIPRI
state.char <- left_join(state.char, sipri.data)


### 
# Merge the state characteristics and alliance data
full.data <- right_join(alliance.1950, state.char)

# Change order of variables for ease in viewing
full.data <- full.data %>% 
  select(atopid, ccode, year, everything())

# Ensure no duplicate observations are present after merging- not an issue here
full.data <- unique(full.data)

# Sort data 
full.data <- full.data[order(full.data$ccode, full.data$year, full.data$atopid), ]


# Some missing alliance characteristics data for states that are not members of an ATOP alliance
# Replace ATOP indicator with zero if missing
full.data$atopid[is.na(full.data$atopid)] <- 0


# States with no alliances in a year are given an alliance ID of zero, grouping them all together


# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year.1950 <- full.data %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  summarize(
    avg.democ = mean(polity, na.rm = TRUE),
    total.cap = sum(CINC, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    total.expend.sipri = sum(ln.sipri.milex, na.rm = TRUE), 
    num.mem = n(),
    .groups = "keep"
  )

alliance.year.1950 <- alliance.year.1950[order(alliance.year.1950$atopid, 
                                               alliance.year.1950$year), ]

# With na.rm = TRUE, all missing values have a sum of zero.
# I filter out all of these alliance-year observations
alliance.year.1950 <- filter(alliance.year.1950, total.expend != 0)

ggplot(alliance.year.1950, aes(x = total.expend)) + geom_density()



# merge the avg democracy, total capability, and total military expenditures variables into the full data
full.data <- left_join(full.data, alliance.year.1950)

# Fill in totals with zero for state-years with no alliance
full.data[46: 50][is.na(full.data[, 46: 50] & full.data$atopid == 0)] <- 0



# Remove observation with missing ccode
full.data <- full.data[complete.cases(full.data$ccode), ]



# Using total capability of an alliance, create a dataset of state-year alliance membership:
full.data <- group_by(full.data, atopid, ccode, year)

state.mem.post45 <- full.data %>% 
                select(atopid, ccode, year, ln.milex) %>% 
                  left_join(select(alliance.year.1950, 
                                   atopid, year, total.expend)) %>%
              mutate(ally.spend = total.expend - ln.milex) %>%
              distinct(atopid, ccode, year, .keep_all = TRUE) %>%
                 select(ccode, atopid, year, ally.spend)

# Drop missing values of the expenditure variable
# Necessary because spread will fill all missing values with zero,
# not just absent combinations as in the above membership matrix
state.mem.post45 <- state.mem.post45[complete.cases(state.mem.post45$ally.spend), ]

# normalize allied spending by year
state.mem.post45 <- state.mem.post45 %>% 
  group_by(year) %>%
  mutate(
    ally.spend.norm = ally.spend / max(ally.spend)
  ) %>%
  group_by()


# filter to ensure alliances match: 
state.mem.post45 <- filter(state.mem.post45, 
                           atopid %in% alliance.char$atopid) %>%
                    select(ccode, year, # avoids duplicates by ally.spend
                           atopid, ally.spend.norm)
  


# This dataframe  contains the spending for the alliances states are a member of in a given year
state.mem.post45 <- spread(state.mem.post45, key = atopid, value = ally.spend.norm, fill = 0)




### combine alliance and state data to format data for STAN 

# Define a state-year level dataset with no missing observations
post45.state <- state.char %>%
  select(ccode, year, change.ln.milex,
         atwar, civilwar, rival.mil, growth.gdp, polity, 
         cold.war, majpower) 

# Add state membership in alliances to this data
post45.state <-  left_join(post45.state, state.mem.post45) 


# Replace missing alliance values with zero 
post45.state[, 11: ncol(post45.state)][is.na(post45.state[, 11: ncol(post45.state)])] <- 0

# Remove observations with missing values
post45.comp <- post45.state[complete.cases(post45.state), ] %>%
  filter(ccode != 950)


# Rescale the state-level regressors
post45.comp[, 4:9] <- lapply(post45.comp[, 4:9], 
                                 function(x) rescale(x, binary.inputs = "0/1"))


# Create separate small and large membership matrices
# Create a matrix of major power membership in alliances (Z in STAN model)
post45.mem.mat <- as.matrix(post45.comp[, 12: ncol(post45.comp)])

# Get cap-year matrices for this data to split large and small matrices
cap.year.post45 <- left_join(select(post45.comp, ccode, year),
                          cap.year)
cap.year.post45 <- cap.year.post45[, colnames(cap.year.post45) %in% 
                                     colnames(post45.mem.mat)]

# get state-mem marix for large
post45.mem.lg = post45.mem.mat*cap.year.post45
# state-mem matrix for small states
sum(cap.year.post45) # total large 1s
cap.year.post45.sm <- mutate_all(cap.year.post45,  ~ifelse(. == 1, 0, 1))
sum(cap.year.post45.sm) # total small 1s (includes 0s) 
nrow(cap.year.post45.sm)*ncol(cap.year.post45.sm) - 
  sum(cap.year.post45) # difference equal to above: checks w/ missing
post45.mem.sm = state.mem.full*cap.year.post45.sm

# remove 0 cols (no small members or missing data on allies)
post45.mem.sm <-  post45.mem.sm[, colSums(post45.mem.sm) != 0]
post45.mem.lg <-  post45.mem.lg[, colSums(post45.mem.lg) != 0]



### transform data into matrices for STAN: focus on non-major powers 
# State-level characeristics
post45.reg.mat <- as.matrix(post45.comp[, 4:8])

# check correlations among state-level regressors
cor(post45.reg.mat, method = "pearson")


# Create the matrix of alliance-level variables for major and non-major power groups
# non-major powers
# Make the alliance characteristics data match the membership matrix
post45.all.sm <- filter(alliance.char, atopid %in% colnames(post45.mem.sm)) %>%
  select(atopid, latent.depth.mean, uncond.milsup, econagg.dum, 
         fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, mean.threat, 
         us.mem, ussr.mem) %>%
  na.omit

post45.mem.sm <- post45.mem.sm[, colnames(post45.mem.sm) %in% 
                                   post45.all.sm$atopid]

post45.all.sm <- select(post45.all.sm, -c(atopid))

# define non-major power alliance matrix
cons <- rep(1, nrow(post45.all.sm))
post45.all.mat.sm <- as.matrix(cbind(cons, post45.all.sm))


# large state alliance regression
# Make the alliance characteristics data match the membership matrix
post45.all.lg <- filter(alliance.char, atopid %in% colnames(post45.mem.lg)) %>%
  select(atopid, latent.depth.mean, uncond.milsup, econagg.dum, 
         fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, mean.threat,
         us.mem, ussr.mem) %>%
  na.omit

post45.mem.lg <- post45.mem.lg[, colnames(post45.mem.lg) %in% 
                                 post45.all.lg$atopid]

post45.all.lg <- select(post45.all.lg, -c(atopid))

# define non-major power alliance matrix
cons <- rep(1, nrow(post45.all.lg))
post45.all.mat.lg <- as.matrix(cbind(cons, post45.all.lg))


# create a state index variable
post45.comp$state.id <- post45.comp %>% 
                      group_by(ccode) %>%
                      group_indices(ccode)
# Create a year index variable 
post45.comp$year.id <- post45.comp %>% 
                       group_by(year) %>%
                       group_indices(year)

