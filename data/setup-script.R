# Joshua Alley
# Texas A&M University
# Load Packages, manage conflicts and and define key functions 
# Run this at start of every session

# Load packages
library(here)
library(conflicted)
library(arm)
library(reshape)
library(countrycode)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(rstan)
library(bayesplot)
library(shinystan)
library(reshape2)
library(party)
library(xtable)
library(coefplot)
library(ggcarly)
library(ggridges)
library(MASS)
library(plm)
library(margins)
library(interflex)
library(ExtremeBounds)
library(bfa)
library(coda)
library(GJRM)
library(loo)


# Look at conflicts 
conflict_scout()

# Set preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")
conflict_prefer("extract", "rstan")
conflict_prefer("monitor", "rstan")
conflict_prefer("traceplot", "rstan")
conflict_prefer("display", "xtable")
conflict_prefer("expand", "tidyr")
conflict_prefer("melt", "reshape2")
conflict_prefer("scale_discrete_manual", "ggplot2")
conflict_prefer("chol2inv", "Matrix")
conflict_prefer("between", "dplyr")
conflict_prefer("coefplot", "coefplot")
conflict_prefer("coefplot.default", "coefplot")
conflict_prefer("colsplit", "reshape2")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("recast", "reshape2")
conflict_prefer("refit", "modeltools")
conflict_prefer("lmList", "nlme")
conflict_prefer("collapse", "dplyr")
conflict_prefer("loo", "loo")


# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Set seed
set.seed(12)


# Define several functions that are used repeatedly

# Plot posterior probabilities that a given coefficient is positive
positive.check <- function(x){
  mean(x > 0)
}


# Define multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/ 
multiplot.ggplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
