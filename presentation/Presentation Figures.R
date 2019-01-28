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


# Plot histogram of mean latent strength
ggplot(atop, aes(x = latent.str.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Strength", y = "Treaties") 
ggsave("presentation/ls-hist.png", height = 6, width = 8)

# Show UAR
ggplot(atop, aes(x = latent.str.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Strength", y = "Treaties") +
  geom_vline(xintercept = 1.9969987829, linetype = "dashed", size = 1)  + 
  geom_text(label="United Arab Rep.", x = 1.9969987829, y = 65, hjust = 1, size = 5)  # UAR
ggsave("presentation/ls-hist-uar.png", height = 6, width = 8)


# Show Weak treaty
ggplot(atop, aes(x = latent.str.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Strength", y = "Treaties") +
  geom_vline(xintercept = -1.36, linetype = "dashed", size = 1)  + 
  geom_text(label="India Ukraine '92", x = -1.36, y = 65, hjust = 0.3, size = 5) # Ukraine-India neutrality and non-aggression
ggsave("presentation/ls-hist-ukr-ind.png", height = 6, width = 8)


# Show typical treaty
ggplot(atop, aes(x = latent.str.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Strength", y = "Treaties") +
  geom_vline(xintercept = -0.103606287, linetype = "dashed", size = 1)  + 
  geom_text(label="France Czech 1938", x = -0.103606287, y = 65, hjust = 0.37, size = 5) # France-Czech consul 1938
ggsave("presentation/ls-hist-fr-czech.png", height = 6, width = 8)

  
  



# Plot results
ggplot(str.dens, aes(x = value,  fill = X2)) +
  geom_density(alpha = .75) + geom_vline(xintercept = 0, size = 1.5) + 
  scale_fill_brewer(name = "Sample", palette = "Dark2") +
  theme_carly_presents() +
  annotate("text", x = -.12, y = 12, 
  label = ".963", size = 8, parse = TRUE) + # Note for major
  annotate("text", x = 0.08, y = 12, 
  label = ".935", size = 8, parse = TRUE) # Note for non-major 
  
ggsave("presentation/str-post.png", height = 6, width = 8)

 
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



### Plot treaty strength against lambda
# non-major powers
ggplot(lambda.df.min, aes(x = latent.str.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Strength", y = "Effect of Allied Spending") +
  theme_carly_presents()
ggsave("presentation/ls-lambda-min.png", height = 6, width = 8)

# Major powers 
ggplot(lambda.df.maj, aes(x = latent.str.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Strength", y = "Effect of Allied Spending") +
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



