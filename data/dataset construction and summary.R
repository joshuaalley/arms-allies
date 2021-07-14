# Joshua Alley
# Texas A&M University
# Constructing Dataset for paper on arms-alliances tradeoff



### This First section creates data on state membership in alliances
# Uses ATOP data from alliance-measures script
########

# select key variables from ATOP alliances with some military support
alliance.char <- select(atop.milsup, atopid,
                    begyr, endyr,
                    uncond.milsup, scope.index, latent.depth.mean,
                    offense, defense, consul, neutral, nonagg, base,
                    armred.rc, organ1, milaid.rc, us.mem, ussr.mem, mp.count, 
                    ecaid, trade.dum, milcor.index, 
                    econagg.dum, fp.conc.index, econagg.index, 
                    nonagg.only, wartime, asymm, asymm.cap, non.maj.only,
                    low.kap.sc, milinst, super.mem)

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


# atop.mem.final removes phased year entry and exit
atop.mem.final <- ungroup(atop.mem.expand) %>%
             select(atopid, ccode, year) %>% 
             distinct(.keep_all = TRUE)



# Merge the two alliance data types using ATOP ID and year
alliance.comp <- left_join(atop.mem.final, alliance.char.expand, 
                           by = c("atopid", "year"))
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
dg.poast <- read.csv("data/dg-poast2016.csv")
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
        initiator = max(initiator), # whether state initiated any conflict in a given year
      .groups = "keep"
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
                disputes = n(),
                .groups = "keep"
                 )

# merge with state variables 
state.vars <- left_join(state.vars, gml.mid.part)
# Replace missing with zeros
state.vars$mid.pres[is.na(state.vars$mid.pres)] <- 0
state.vars$disputes[is.na(state.vars$disputes)] <- 0







# Remove pacific micro-states (comprehensive missing data)
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


# What does inverse hyperbolic sine transformation look like? 
ggplot(state.vars, aes(x = asinh(growth.milex))) + geom_density()
ggplot(state.vars, aes(x = asinh(growth.milex))) + geom_histogram(bins = 45)

# Concerns, given noise in COW measure: 
# cases where growth is -1 or close. Implies elimination of military budget
# cases with infinite change- 0 to some spending: newly indep states and fluctuations in budget
# But SIPRI is missing most Warsaw Pact members in the Cold War, which is a problem.


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
    avg.rival.milex = rival.milex / total.rivals,
    .groups = "keep"
  ) %>%
  ungroup() 
colnames(td.rivalry.annual)[1] <- "ccode"


# Merge rival spending data in state variables 
state.vars <- left_join(state.vars, td.rivalry.annual)  
# fill missing rivalry data  with 0s
state.vars$total.rivals[is.na(state.vars$total.rivals)] <- 0
state.vars$rival.milex[is.na(state.vars$rival.milex)] <- 0
state.vars$avg.rival.milex[is.na(state.vars$avg.rival.milex)] <- 0


# Cut state variables down to years with comprehensive coverage of controls
state.vars <- filter(state.vars, year <= 2007)






### This Section combines state characteristics and alliance data
#####
# Needed for a state-year measure of allied capability and to create the matrix of state membership in alliances


# Merge the state characteristics and alliance data
atop.cow.year <- right_join(alliance.comp, state.vars)

# Change order of variables for ease in viewing
atop.cow.year <- atop.cow.year %>% 
  select(atopid, ccode, year, everything())

# Ensure no duplicate observations are present after merging- not an issue here
atop.cow.year <- distinct(atop.cow.year,
                          .keep_all = TRUE)

# Sort data 
atop.cow.year[order(atop.cow.year$ccode, atop.cow.year$year, atop.cow.year$atopid), ]


# Some missing alliance characteristics data for states that are not members of an ATOP alliance
# Replace ATOP indicator with zero if missing
atop.cow.year$atopid[is.na(atop.cow.year$atopid)] <- 0

# If no ATOP alliance, fill all other alliance characteristic variables with a zero.
atop.cow.year[4:36][is.na(atop.cow.year[, 4:36] & atop.cow.year$atopid == 0)] <- 0


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
alliance.state.year <- atop.cow.year %>%
  group_by(atopid, year) %>%
  mutate(
    num.mem = n(),
    cinc.share = cinc / sum(cinc, na.rm = TRUE),
    cinc.dist = cinc.share - (1 / num.mem), # gap between share and even dist of cap
    # dummy indicator of most capable state
    most.cap = ifelse(cinc == max(cinc, na.rm = TRUE) | # most capability OR
                    (mp.count > 1 & mp.count == num.mem & majpower == 1) | # major power alliances OR
                    (cinc.share >= .4 & num.mem == 2) |  # symmetric non-maj OR
                    (majpower == 1 & asymm.cap == 1 & # Other major powers in asymm ML
                      super.mem == 0 & num.mem > 2), # w/o superpowers, however
                      1, 0),
    
    maxcap.democ = ifelse(most.cap == 1, polity2, 0),
    democ.weight = polity2 * cinc.share,
    democ = ifelse(polity2 > 5, 1, 0),
    contrib.cinc.per = cinc.share*100
      ) 

# summarize CINC shares
summary(alliance.state.year$cinc.share, 
        subset = alliance.state.year$atopid != 0)
filter(alliance.state.year, atopid != 0) %>%
   ggplot(aes(x = cinc.share)) + geom_histogram()


# scatterplot by size
filter(alliance.state.year, atopid != 0 & num.mem > 0) %>%
ggplot(aes(x = num.mem, y = cinc.share)) +  
  stat_bin_hex(colour="white", na.rm=TRUE) + # bin points- overplotted
  scale_fill_gradientn(colours=c("#999999","#333333"), 
                       name = "Frequency", 
                       na.value=NA)
# multilateral
filter(alliance.state.year, atopid != 0 &
         num.mem > 2) %>%
  ggplot(aes(x = cinc.share)) + geom_histogram()
# bilateral
filter(alliance.state.year, atopid != 0 &
         num.mem == 2) %>%
  ggplot(aes(x = cinc.share)) + geom_histogram()


# create an indicator of state capability for model where 
# lambdas are grouped by most capable state
cap.year <- alliance.state.year %>%
             group_by(ccode, atopid) %>%
            select(ccode, year, atopid, most.cap) 

cap.year <- distinct(cap.year, atopid, ccode, year, .keep_all = TRUE)

# This matrix has a binary indicator of which alliances states are a member of in a given year
cap.year <- spread(cap.year, key = atopid, value = most.cap, fill = 0)


# alliance-year data- summarize state characteristics
alliance.year <- alliance.state.year %>% 
  filter(atopid > 0) %>%
  summarize(
    avg.democ = mean(polity2, na.rm = TRUE),
    max.democ = max(polity2, na.rm = TRUE),
    min.democ = min(polity2, na.rm = TRUE),
    maxcap.democ.max = max(maxcap.democ, na.rm = TRUE),
    maxcap.democ.min = min(maxcap.democ, na.rm = TRUE),
    joint.democ = ifelse(min.democ > 5, 1, 0), 
    democ.count = sum(democ, na.rm = TRUE),
    
    total.cap = sum(cinc, na.rm = TRUE),
    total.expend = sum(milex, na.rm = TRUE),
    total.ln.expend = sum(ln.milex, na.rm = TRUE),
    total.gdp = sum(gdp, na.rm = TRUE),
    num.mem = n(),
  
    dem.prop = democ.count / num.mem,
    avg.democ.weight = mean(democ.weight, na.rm = TRUE),
    max.democ.weight = max(democ.weight, na.rm = TRUE),
    min.democ.weight = min(democ.weight, na.rm = TRUE),
    
    max.threat = max(lsthreat, na.rm = TRUE),
    min.threat = min(lsthreat, na.rm = TRUE),
    mean.threat = mean(lsthreat, na.rm = TRUE),
    .groups = "keep"
  )

alliance.year[order(alliance.year$atopid, alliance.year$year),] 


# Merge mean democracy into alliance characteristics data
# this is democracy at time of formation
alliance.year <- alliance.year %>% 
                  group_by(atopid) %>%
                  mutate(
                    begyr = min(year)
                  )

alliance.democ <- filter(alliance.year, begyr == year) %>% 
                 select(c(atopid, dem.prop, joint.democ, avg.democ, max.democ, min.democ, 
                          avg.democ.weight, max.democ.weight, min.democ.weight,
                          max.threat, min.threat, mean.threat, num.mem, 
                          maxcap.democ.min, maxcap.democ.max))

# merge with alliance characteristics data
alliance.char <- left_join(alliance.char, alliance.democ, by = "atopid")
summary(alliance.char$avg.democ)
alliance.char$avg.democ[is.nan(alliance.char$avg.democ)] <- 0 # alliances where some members are not in system
ggplot(alliance.char, aes(x = avg.democ)) + geom_histogram()

# Look at correlation between democractic membership and depth
cor.test(alliance.char$avg.democ, alliance.char$latent.depth.mean)
ggplot(alliance.char, aes(x = avg.democ, y = latent.depth.mean)) + 
  geom_point() + theme_classic()

# and t-test for uncond milsup 
t.test(alliance.char$avg.democ ~ alliance.char$uncond.milsup)


# Quick check: are avg threat and avg depth correlated?
summary(alliance.char$mean.threat)
cor.test(alliance.char$latent.depth.mean, alliance.char$mean.threat)
ggplot(alliance.char, aes(x = mean.threat, y = latent.depth.mean)) +
  geom_point() 


# max threat driven more by major powers
# min threat is too conservative


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
  select(atopid, ccode, year, cinc, milex, ln.milex, gdp) %>% 
  left_join(alliance.year) %>%
  mutate(milex.contrib = milex / total.expend,
         milex.contrib.ln = ln.milex / total.ln.expend,
         ally.spend = total.ln.expend - ln.milex,
         ally.cap = total.cap - cinc,
         contrib.gdp = gdp / total.gdp) %>%
  distinct(atopid, ccode, year, .keep_all = TRUE) %>%
  select(ccode, atopid, year, ally.spend, ally.cap, avg.democ, milex.contrib, contrib.gdp, num.mem) 

# Replace missing values with zero if atopid = 0 (no alliance)
state.mem.cap$ally.spend[is.na(state.mem.cap$ally.spend) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$ally.cap[is.na(state.mem.cap$ally.cap) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$milex.contrib[is.na(state.mem.cap$milex.contrib) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$avg.democ[is.na(state.mem.cap$avg.democ) & state.mem.cap$atopid == 0] <- 0
state.mem.cap$contrib.gdp[is.na(state.mem.cap$contrib.gdp) & state.mem.cap$atopid == 0] <- 0


# Add allied spending to state-alliance-year data: single level regressions
state.ally.year <- left_join(atop.cow.year, state.mem.cap)




# Drop missing values of the expenditure variable
# Necessary because spread will fill all missing values with zero,
# not just absent combinations as in the above membership matrix
summary(state.mem.cap$ally.spend)
state.mem.cap <- state.mem.cap[complete.cases(state.mem.cap$ally.spend), ]
ggplot(state.mem.cap, aes(x = ally.spend)) + geom_histogram()


# Summarize by capability
summary(state.mem.cap$ally.cap)
state.mem.cap <- state.mem.cap[complete.cases(state.mem.cap$ally.cap), ]
ggplot(state.mem.cap, aes(x = ally.cap)) + geom_histogram()

# filter to ensure alliances match: 
state.mem.cap <- filter(state.mem.cap, atopid %in% alliance.char$atopid)


# rescale the ally expenditures variable 
# keeps them on a similar scale to regressors in ML model
state.mem.cap$ally.spend.rescale <- rescaler(state.mem.cap$ally.spend, type = "range")
state.mem.cap$ally.spend.rescale[state.mem.cap$ally.spend.rescale == 0] <- 0.01

summary(state.mem.cap$ally.spend.rescale)
ggplot(state.mem.cap, aes(x = ally.spend.rescale)) + geom_histogram()


# rescale the ally expenditures variable by 2sd  
# keeps them on a similar scale to regressors in ML model
state.mem.cap$ally.spend.rescale2sd <- rescale(state.mem.cap$ally.spend)

summary(state.mem.cap$ally.spend.rescale2sd)
ggplot(state.mem.cap, aes(x = ally.spend.rescale2sd)) + geom_histogram()

# Normalized spending by year
state.mem.cap <- state.mem.cap %>% 
                  group_by(year) %>%
                   mutate(
                     ally.spend.norm = ally.spend / max(ally.spend, na.rm = TRUE)
                   ) %>%
                  ungroup()
                  
summary(state.mem.cap$ally.spend.norm)
ggplot(state.mem.cap, aes(x = ally.spend.norm)) + geom_histogram()


# change in alliance capability over time within alliances
state.mem.cap %>%
   filter(atopid != 0) %>%
    group_by(atopid, year) %>%
  mutate(
    change.ally.norm = ally.spend.norm - lag(ally.spend.norm)
  ) %>%
  filter(change.ally.norm != 0) %>%
  ggplot(aes(x = change.ally.norm)) + geom_histogram() 


# summarize the distribution  
state.mem.cap %>%
  filter(atopid != 0) %>%
  group_by(atopid, year) %>%
  mutate(
    change.ally.norm = ally.spend.norm - lag(ally.spend.norm)
  ) %>%
  filter(change.ally.norm != 0) %>%
    ungroup() %>%
  summarize(
   sum.change.norm = summary(change.ally.norm)
  )

# calculate variation in alliance-cap
# over years
state.cap.var <- select(state.mem.cap, ccode, atopid, year, 
                            ally.spend.norm) %>%
                       group_by(ccode, atopid) %>% # over time var within alliance
                       summarize(
                         sd.cap = sd(ally.spend.norm, na.rm = TRUE),
                         .groups = "keep"
                       ) %>%
                     filter(sd.cap > 0) # remove state-alliances w/ no cap
summary(state.cap.var$sd.cap)
ggplot(state.cap.var, aes(x = sd.cap)) + geom_histogram()


# alliance cap var
# within alliances across states and time 
alliance.cap.var <- select(state.mem.cap, ccode, atopid, year, 
                        ally.spend.norm) %>%
  group_by(atopid) %>%
  summarize(
    sd.cap = sd(ally.spend.norm, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  filter(sd.cap > 0)
summary(alliance.cap.var$sd.cap)
ggplot(alliance.cap.var, aes(x = sd.cap)) + geom_histogram()

# by year
# within alliances across states by year
year.cap.var <- select(state.mem.cap, ccode, atopid, year, 
                           ally.spend.norm) %>%
  group_by(atopid, year) %>%
  summarize(
    sd.cap = sd(ally.spend.norm, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  filter(sd.cap > 0)
summary(year.cap.var$sd.cap)
ggplot(year.cap.var, aes(x = sd.cap)) + geom_histogram()


# This dataframe contains the normalized capability (by year) for the alliances states are a member of in a given year
state.mem.spread.norm <- select(state.mem.cap, atopid, ccode, year, ally.spend.norm) %>%
  spread(key = atopid, value = ally.spend.norm, fill = 0)


# Create several alternative membership matrices
# Create another membership matrix with contribution to the alliance
state.mem.contrib <- select(state.mem.cap, atopid, ccode, year, milex.contrib) %>%
                  spread(key = atopid, 
                            value = milex.contrib, fill = 0)

# This dataframe contains the total cinc scores for the alliances states are a member of in a given year
state.mem.spread.cinc <- select(state.mem.cap, atopid, ccode, year, ally.cap) %>%
  spread(key = atopid, value = ally.cap, fill = 0)

# This dataframe contains rescaled allied spending for the alliances states are a member of in a given year
state.mem.spread.rescale <- select(state.mem.cap, atopid, ccode, year, ally.spend.rescale) %>%
  spread(key = atopid, value = ally.spend.rescale, fill = 0)


# This dataframe contains rescaled allied spending (s2d) for the alliances states are a member of in a given year
state.mem.spread.rescale2sd <- select(state.mem.cap, atopid, ccode, year, ally.spend.rescale2sd) %>%
  spread(key = atopid, value = ally.spend.rescale2sd, fill = 0)




# Combine State and alliance data 
########


# Define a state-year level dataset with no missing observations
reg.state.comp <- state.vars %>%
  select(ccode, year, growth.milex, 
         atwar, civilwar.part, rival.milex, gdp.growth, polity2, 
         cold.war, disputes, majpower) %>%
       drop_na() %>%
     filter(year >= 1919)

# Add state membership in alliances to this data
reg.state.comp <-  left_join(reg.state.comp, state.mem.spread.norm) 


# Replace missing alliance values with zero 
reg.state.comp[, 12: ncol(reg.state.comp)][
  is.na(reg.state.comp[, 12: ncol(reg.state.comp)])] <- 0


# Rescale the state-level regressors
reg.state.comp[, 4:10] <- lapply(reg.state.comp[, 4:10], 
                                 function(x) rescale(x, binary.inputs = "0/1"))


# Check the range and distribution of the DV
summary(reg.state.comp$growth.milex)
sd(reg.state.comp$growth.milex)
ggplot(reg.state.comp, aes(growth.milex)) + geom_density()



# create a state index variable
reg.state.comp$state.id <- reg.state.comp %>% 
  group_by(ccode) %>% group_indices()
# Create a year index variable 
reg.state.comp$year.id <- reg.state.comp%>% 
  group_by(year) %>% group_indices()


### transform data into matrices for STAN
# State-level characeristics
reg.state.mat <- as.matrix(reg.state.comp[, 4:10])

# check correlations among state-level regressors
cor(reg.state.mat, method = "pearson")


# General set of alliance-level regressors
reg.all.data <- select(alliance.char, atopid, latent.depth.mean, uncond.milsup, econagg.dum, fp.conc.index, num.mem, low.kap.sc, 
         avg.democ, wartime, asymm, mean.threat)


# quick tables of summary statistics
stargazer(reg.all.data, summary = TRUE)
stargazer(reg.state.mat, summary = TRUE)
