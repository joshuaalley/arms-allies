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




 
#### Graphical comparison of coefficients with Bayesplot
# Set the color scheme here
color_scheme_set("red")

# Create a dataframe with beta parameters
beta.pars <- as.data.frame(ml.model.sum$beta)

# Plot posterior density and Fill the negative region
# Uncoditional treaties 
uncond.dens <- ggplot(data = beta.pars, mapping = aes(x = uncond.milsup)) +
  + theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))
  geom_density() + geom_vline(xintercept = 0, size = 1.5) 

d <- ggplot_build(uncond.dens)$data[[1]] # build the density plot for filling

uncond.dens + geom_area(data = subset(d, x < 0), aes(x=x, y=y), fill="#d95f02") +
   annotate("text", x = -.02, y = 40, label = ".934", size = 15, parse = TRUE)  + 
  theme_carly_presents() + labs(x = "Posterior: Unconditional Military Support")
ggsave("presentation/uncond post_prob.png", height = 6, width = 8)



# Plot intervals
mcmc_areas(ml.model.sum$beta, pars = c("uncond.milsup"), prob = .9) +
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



