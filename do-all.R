# Joshua Alley
# do all scripts in proper order

# set up environment
source("data/setup-script.R", echo = TRUE)
# clean data
source("data/alliance-measures.R", echo = TRUE)
source("data/dataset construction and summary.R", echo = TRUE)

# analysis
source("data/analysis-lmbcap.R", echo = TRUE)
source("data/simulated-effects.R", echo = TRUE)

# appendix
source("data/alt-measures.R", echo = TRUE)
source("data/single-level regression.R", echo = TRUE)
source("data/analysis-neutral.R", echo = TRUE)
source("data/leeds-anac-replication/depth-cred-reanalysis.R", echo = TRUE)
source("data/simulation-check.R", echo = TRUE)
