# Joshua Alley
# Texas A&M University
# R Script to Produce Figures for Presentation of Alliance Design and the Arms/Alliances Tradeoff


# Environment is determined by use of projects and/or running this file in conjunction with
# the script dataset construction and summary.R and analysis.R


# Load packages if using as stand-alone file
library(dplyr)
library(ggplot2)
library(ggcarly)

# Set working directory to current folder 
setwd(here::here())
getwd()



# Plot results
pres.mplot <- multiplot(rreg.min, rreg.maj, m1r.reg,
          names = c("Non-Major Powers", "Major Powers", "Full Sample"),
          coefficients = "uncond.milsup.pres",
          by = "Model",
          xlab = "Value",
          color = "#d95f02",
          zeroColor = "black",
          zeroType = 1,
          zeroLWD = 2,
         ylab = "Sample",
         title = "",
         lwdInner = 4,
         lwdOuter =  2,
           pointSize = 8,
        horizontal = FALSE
) 
pres.mplot + theme_classic() + theme(axis.text=element_text(size=20),
                                     axis.title=element_text(size=24,face="bold"))
ggsave("presentation/robust-reg-coef.png", height = 6, width = 8) 



# Dynamic simulation of impact of minor powers
scen1 <- data.frame(uncond.milsup.pres = 1,
                    cond.milsup.pres = 0,
                    atwar = 0,
                    civilwar.part = 0,
                    polity = mean(state.char.full$polity, na.rm = TRUE),
                    ln.gdp = mean(state.char.full$ln.gdp, na.rm = TRUE),
                    ln.ally.expend = mean(state.char.full$ln.ally.expend, na.rm = TRUE),
                    lsthreat = mean(state.char.full$lsthreat, na.rm = TRUE),
                    cold.war = 0,
                    avg.num.mem = mean(state.char.full$avg.num.mem, na.rm = TRUE),
                    avg.dem.prop = mean(state.char.full$avg.dem.prop, na.rm = TRUE)
)

# No unconditional- conditional instead
scen2 <- data.frame(uncond.milsup.pres = 0, 
                    cond.milsup.pres = 1,
                    atwar = 0,
                    civilwar.part = 0,
                    polity = mean(state.char.full$polity, na.rm = TRUE),
                    ln.gdp = mean(state.char.full$ln.gdp, na.rm = TRUE),
                    ln.ally.expend = mean(state.char.full$ln.ally.expend, na.rm = TRUE),
                    lsthreat = mean(state.char.full$lsthreat, na.rm = TRUE),
                    cold.war = 0,
                    avg.num.mem = mean(state.char.full$avg.num.mem, na.rm = TRUE),
                    avg.dem.prop = mean(state.char.full$avg.dem.prop, na.rm = TRUE)
)

scen.list <- list(scen1, scen2)

Sim1 <- dynsim(obj = rreg.min, ldv = 'lag.ln.milex',
               scen = scen.list, n = 20)
dynsimGG(Sim1, leg.labels = c("Unconditional Military Support", "Conditional Military Support"), color = "Dark2") +
  theme_carly_presents()
ggsave("presentation/mp-dynsim.png", height = 6, width = 8) 


rm(list = c("m1r.reg", "rreg.maj", "rreg.min"))


# Plot same split from ML model
ggplot(uncond.summary, aes(x = row.names(uncond.summary), y = mean)) +
  geom_errorbar(aes(ymin = `5%`, 
                    ymax = `95%`,
                    width=.05), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) + geom_hline(yintercept = 0) +
  theme_carly_presents() + labs(x = "Sample", y = "Effect of Unconditional Military Support") +
  coord_flip()
ggsave("presentation/ml-model-uncond-coefs.png", height = 6, width = 8) 


# 
#### Plot the Means of the Lambda Parameters
lambda.probs$atopid <- reorder(lambda.probs$atopid, lambda.probs$lambda.mean)

lambda.probs %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = factor(uncond.milsup))) + 
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  geom_col() + coord_flip() +
  labs(x = "Alliance", y = "Posterior Mean of Lambda Parameter")  + 
  theme_carly_presents()



### Violin plots
lambda.probs %>%
ggplot(aes(x = factor(uncond.milsup), y = lambda.mean, fill = factor(uncond.milsup))) +
  geom_violin() +  # add violin
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Unconditional Military Support", y = "Alliance Coef.")  + 
  theme_carly_presents()
ggsave("presentation/lambda-box-presentation.png", height = 6, width = 8) 


 
#### Graphical comparison of coefficients with Bayesplot
# Set the color scheme here
color_scheme_set("red")

# Create a dataframe with beta parameters
beta.pars <- as.data.frame(ml.model.sum$beta)

# Plot posterior density and Fill the negative region
# Uncoditional treaties 
uncond.dens <- ggplot(data = beta.pars, mapping = aes(x = uncond.milsup)) +
  geom_density() + geom_vline(xintercept = 0, size = 1.5) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

d <- ggplot_build(uncond.dens)$data[[1]] # build the density plot for filling

uncond.dens + geom_area(data = subset(d, x < 0), aes(x=x, y=y), fill="#d95f02") +
   annotate("text", x = -.02, y = 40, label = ".934", size = 15, parse = TRUE)  + 
  theme_carly_presents() + labs(x = "Posterior: Unconditional Military Support")
ggsave("presentation/uncond post_prob.png", height = 6, width = 8)



# Plot intervals
mcmc_areas(ml.model.sum$beta, pars = c("uncond.milsup"), prob = .89) +
  ggplot2::labs(
    title = "Posterior distributions of Unconditional Alliance Parameter",
    subtitle = "with median and 90% interval")

# Compare unconditional and proportion of democracies
mcmc_areas(ml.model.sum$beta, pars = c("dem.prop", "uncond.milsup"), prob = .9)+
  ggplot2::labs( 
   title = "Posterior distributions of Democratic Proportion and Unconditional Parameters",
    subtitle = "with medians and 90% intervals")

# Plot alliance-level regression positive posterior probabilities
beta.probs <- cbind.data.frame(beta.probs, c("Alliance Model Constant", "Uncond. Mil. Supp.", "Offense", 
                                             "Number Members","Democratic Membership", 
                                             "Wartime", "IO Form.", "Military Aid", "Asymmetric",
                                             "US Member", "USSR Member"))

colnames(beta.probs) <- c("post.prob", "variable")
beta.probs$variable <- reorder(beta.probs$variable, beta.probs$post.prob)

# Plot
ggplot(beta.probs, aes(x = variable, y = post.prob)) + 
  geom_col() +
  geom_text(aes(label = post.prob), nudge_y = .0675) +
  labs(y = "Posterior Probability of Positive Coefficient") +
  coord_flip()
ggsave("presentation/post-prob-beta.png", height = 6, width = 8)




### Plot the non-zero lambdas
# For non-zero alliances 
lambda.probs %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = factor(uncond.milsup))) + 
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = lambda.mean), nudge_y = 0.01, size = 4) +
  labs(y = "Posterior Mean of Alliance Weight Parameter") +
  coord_flip()
ggsave("presentation/non-zero alliances.png", height = 6, width = 8)


### Add a posterior predictive check
color_scheme_set("viridisC")
ppc_hist(y, yrep.full[1:5, ])
ggsave("presentation/ppc.png", height = 6, width = 8)




#### Figures for Prospectus defense slides

# Distribution of compellent alliance duration
alliance.char %>% filter(uncond_comp == 1) %>%
  ggplot(alliance.char, mapping = aes(x = freq)) + 
  geom_histogram(bins = 20) + 
  labs(x = "Active Alliance Years") +
  theme_carly_presents()


# Distribution of unconditional deterrent alliance duration
alliance.char %>% filter(uncond_det == 1) %>%
  ggplot(alliance.char, mapping = aes(x = freq)) + 
  geom_histogram(bins = 20) + 
  labs(x = "Active Alliance Years") +
  theme_carly_presents()


