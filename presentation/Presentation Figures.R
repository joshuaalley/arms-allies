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




# Create a plot of estiamted effects of treaty participation. 
# Summarize lambda from full model (not fit on subsets)
lambda.summary <- summary(ml.model, pars = c("lambda"), probs = c(0.05, 0.95))$summary
lambda.summary <- cbind.data.frame(as.numeric(colnames(state.mem.mat)), lambda.summary)
colnames(lambda.summary) <- c("atopid", "lambda.mean", "lambda.se.mean",
                             "lambda.sd", "lambda.5", "lambda.95",
                             "lambda.neff", "lambda.rhat")
lambda.summary <- left_join(atop, lambda.summary)


# Plot points with error bars by start year of treaty without having split samples 
ggplot(lambda.summary, aes(x = begyr, y = lambda.mean)) +
  geom_errorbar(aes(ymin = lambda.5, 
                    ymax = lambda.95,
                    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1), size = 2) + geom_hline(yintercept = 0) +
  labs(x = "Start Year", y = "Estimated Impact on Growth in Military Spending") + 
  coord_flip() +
  theme_carly_presents()


# Compare lambdas for alliances with major and minor power members
lambda.mix.full <- lambda.df.min %>%
  select(atopid, begyr, lambda, latent.scope.mean) %>%
  left_join(select(lambda.df.maj, atopid, begyr, lambda, latent.scope.mean), 
            by = c("atopid", "begyr", "latent.scope.mean")) %>% 
  rename(
    lambda.min = lambda.x,
    lambda.maj = lambda.y
  ) %>%
  gather(mp.stat, lambda, c(lambda.min, lambda.maj))

ggplot(lambda.mix.full, aes(x = begyr, y = c(lambda))) +
  geom_point(position = position_dodge(0.1), size = 2) + geom_hline(yintercept = 0) +
  labs(x = "Start Year", y = "Estimated Impact on Growth in Military Spending") + 
  coord_flip() +
  theme_carly_presents()
ggsave("presentation/lambda-est-full.png", height = 6, width = 8)


# Plot histogram of mean latent Scope
ggplot(atop.milsup, aes(x = latent.scope.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Scope", y = "Treaties") 
ggsave("presentation/ls-hist.png", height = 6, width = 8)

# Show UAR
ggplot(atop.milsup, aes(x = latent.scope.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Scope", y = "Treaties") +
  geom_vline(xintercept = 1.9969987829, linetype = "dashed", size = 1)  + 
  geom_text(label="United Arab Rep.", x = 1.9969987829, y = 30, hjust = 1, size = 5)  # UAR
ggsave("presentation/ls-hist-broad.png", height = 6, width = 8)


# Show Weak treaty
ggplot(atop.milsup, aes(x = latent.scope.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Scope", y = "Treaties") +
  geom_vline(xintercept = -0.017, linetype = "dashed", size = 1)  + 
  geom_text(label="UK-France 1870", x = -0.017, y = 35, hjust = -0.01, size = 5) # Ukraine-India neutrality and non-aggression
ggsave("presentation/ls-hist-narrow.png", height = 6, width = 8)


# Show typical treaty
ggplot(atop.milsup, aes(x = latent.scope.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Scope", y = "Treaties") +
  geom_vline(xintercept = 0.90289, linetype = "dashed", size = 1)  + 
  geom_text(label="Crimean War 1854", x = 0.90289, y = 38, hjust = 1, size = 5) # France-Czech consul 1938
ggsave("presentation/ls-hist-median.png", height = 6, width = 8)


# Show typical treaty
ggplot(atop.milsup, aes(x = latent.scope.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Scope", y = "Treaties") +
  geom_vline(xintercept = 0.66128237, linetype = "dashed", size = 1)  + 
  geom_text(label="NATO", x = 0.66128237, y = 38, hjust = 1, size = 5) # NATO
ggsave("presentation/ls-hist-nato.png", height = 6, width = 8)

  
  

# Plot results
ggplot(scope.dens, aes(x = value,  fill = X2)) +
  geom_density(alpha = .75) + geom_vline(xintercept = 0, size = 1.5) + 
  scale_fill_brewer(name = "Sample", palette = "Dark2") +
  theme_carly_presents() +
  annotate("text", x = -.12, y = 12, 
  label = ".963", size = 8, parse = TRUE) + # Note for major
  annotate("text", x = 0.08, y = 12, 
  label = ".935", size = 8, parse = TRUE) # Note for non-major 
  
ggsave("presentation/scope-post.png", height = 6, width = 8)

 
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



### Plot treaty Scope against lambda
# non-major powers
ggplot(lambda.df.min, aes(x = latent.scope.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Scope", y = "Alliance Part. Impact") +
  theme_carly_presents()
ggsave("presentation/ls-lambda-min.png", height = 6, width = 8)

# Major powers 
ggplot(lambda.df.maj, aes(x = latent.scope.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Scope", y = "Alliance Part. Impact") +
  theme_carly_presents()
ggsave("presentation/ls-lambda-maj.png", height = 6, width = 8)



## Impact of alliances
# Start with NATO

# plot for the US
nato.imp.maj.sum %>%
  filter(ccode == 2 & year >= 1949) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  labs(x = "Year", y = "NATO Impact") +
  theme_carly_presents()
ggsave("presentation/nato-imp-us.png", height = 6, width = 8)


# add a plot of impact of US alliance participation on growth in milex
agg.all.maj.sum %>%
  filter(ccode == 2 & year >= 1942) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  xlab("Year") + ylab("Agg. Impact") +
  theme_carly_presents()
ggsave("presentation/us-agg-imp.png", height = 6, width = 8)



# for NATO members, much of perceived free-riding may be the result of EU participation
# not NATO
# Use Belgium as the example 

# The full picture for Belgium
ggplot(bel.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  labs(x = "Aggregate Alliance Impact", y = "Year") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.07, 0.05) 
ggsave("presentation/bel-agg-imp.png", height = 6, width = 8)

# NATO for Belgium
nato.imp.min.sum %>%
  filter(ccode == 211 & year >= 1949) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  labs(y = "NATO Impact", x = "Year") +
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  theme_carly_presents()
ggsave("presentation/bel-nato-imp.png", height = 6, width = 8)

# EU for Belgium
eu.imp.min.sum %>%
  filter(ccode == 211 & year >= 1992) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  labs(y = "EU Impact", x = "Year") +
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  theme_carly_presents()
ggsave("presentation/bel-eu-imp.png", height = 6, width = 8)



### Appendix Slides
illus.plot + theme_carly_presents()
ggsave("presentation/illus-arg.png", height = 6, width = 9)



# Non-zero major power alliances
lambda.probs.maj %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = latent.scope.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  labs(x = "ATOPID", y = "Impact of Alliance",
       fill = "Scope") +
  theme_carly_presents() +
  coord_flip() 
ggsave("presentation/non-zero-maj.png", height = 6, width = 8)


# non-major powers
lambda.probs.min %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = latent.scope.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  labs(x = "ATOPID", y = "Impact of Alliance",
       fill = "Scope") +
  theme_carly_presents() +
  coord_flip() 
ggsave("presentation/non-zero-min.png", height = 6, width = 8)



# Plot of varying slopes results for scope
ggplot(scope.dens.joint, aes(x = value,  fill = X2)) +
  geom_density(alpha = .75) + 
  scale_fill_brewer(name = "Sample", palette = "Dark2") +
  theme_carly_presents() +
  annotate("text", x = -.12, y = 12, 
           label = ".93", size = 8, parse = TRUE) + # Note for major
  annotate("text", x = 0.07, y = 12, 
           label = ".93", size = 8, parse = TRUE) + # Note for non-major
  annotate("text", x = 0.00, y = 2, 
           label = ".08", size = 5, parse = TRUE) + # Note for overlap
  theme_carly_presents()
ggsave("presentation/var-slopes-scope.png", height = 6, width = 8)


# All the coefficients from the varying slopes model
# non-major powers
non.maj.vs <- mcmc_intervals(ml.model.sum$beta[, 1, ], 
               prob = .9) + 
               ggtitle("Non-Major") +
               theme_carly_presents()

maj.vs <- mcmc_intervals(ml.model.sum$beta[, 2, ], 
               prob = .9) + 
               ggtitle("Major") +
               theme_carly_presents()
multiplot.ggplot(non.maj.vs, maj.vs) # need to save manually 


#### Graphical comparison of coefficients with Bayesplot
# Set the color scheme here
color_scheme_set("red")





### Add a posterior predictive check
color_scheme_set("viridisC")
traceplot(ml.model.maj, pars = "beta")
ggsave("presentation/beta-trace-maj.png", height = 6, width = 8)
traceplot(ml.model.min, pars = "beta")
ggsave("presentation/beta-trace-min.png", height = 6, width = 8)




### Single-level regression check 
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
pres.mplot + theme_carly_presents() 
ggsave("presentation/robust-reg-coef.png", height = 6, width = 8) 

rm(list = c("m1r.reg", "rreg.maj", "rreg.min"))



