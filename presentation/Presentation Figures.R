# Joshua Alley
# Texas A&M University
# R Script to Produce Figures for Presentation of Alliance Design and the Arms/Alliances Tradeoff


# Environment is determined by use of projects and/or running this file in conjunction with
# the script dataset construction and summary.R and analysis.R


# Load packages if using as stand-alone file
library(dplyr)
library(ggplot2)

# Set working directory to current folder 
setwd(here::here())
getwd()



# 
#### Plot the Means of the Lambda Parameters
lambda.probs$atopid <- reorder(lambda.probs$atopid, lambda.probs$lambda.mean)

lambda.probs %>% 
  filter(alliance.type != "No Aid" & alliance.type != "Probabilistic Deterrent") %>%
  ggplot(aes(x = lambda.mean, fill = alliance.type)) + 
  scale_fill_brewer(palette = "Dark2") +
  geom_col() +
  labs(x = "Alliance", y = "Posterior Mean of Lambda Parameter")

# compare unconditional and nones
lambda.probs %>% 
  filter(alliance.type != "Conditional Deterrent" & alliance.type != "Probabilistic Deterrent") %>%
  ggplot(aes(x = lambda.mean, fill = alliance.type)) + 
  scale_fill_brewer(palette = "Dark2") +
  geom_density() +
  labs(x = "Posterior Mean of Lambda Parameter") 
ggsave("presentation/lambdac none-uncond.png", height = 6, width = 8)


### Violin plots
lambda_df %>%
  filter(alliance.type != "Conditional Deterrent" & alliance.type != "Probabilistic Deterrent") %>%
ggplot(aes(x = alliance.type, y = lambda, fill = alliance.type)) +
  geom_violin() +  # add violin
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Posterior Mean of Lambda Parameter") 
ggsave("presentation/lambda-box-presentation.png", height = 6, width = 8)

# Include all alliance types for an appendix slide
lambda_df %>%
  ggplot(aes(x = alliance.type, y = lambda, fill = alliance.type)) +
  geom_violin() +  # add violin
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Posterior Mean of Lambda Parameter") 
ggsave("presentation/lambda-box-presentation-full.png", height = 6, width = 8)


 
#### Graphical comparison of coefficients with Bayesplot
# Set the color scheme here
color_scheme_set("red")

# Create a dataframe with beta parameters
beta.pars <- as.data.frame(ml.model.sum$beta)

# Plot posterior density and Fill the negative region
# Uncoditional treaties 
uncond.dens <- ggplot(data = beta.pars, mapping = aes(x = unconditional)) +
  geom_density() + geom_vline(xintercept = 0, size = 1.5) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

d <- ggplot_build(uncond.dens)$data[[1]] # build the density plot for filling

uncond.dens + geom_area(data = subset(d, x < 0), aes(x=x, y=y), fill="#d95f02") +
   annotate("text", x = -.06, y = 22, label = ".934", size = 15, parse = TRUE)
ggsave("presentation/uncond post_prob.png", height = 6, width = 8)

# probabilisitc deterrent treaties
probdet.dens <- ggplot(data = beta.pars, mapping = aes(x = prob_det)) +
  geom_density() + geom_vline(xintercept = 0, size = 1.5) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

d.pd <- ggplot_build(probdet.dens)$data[[1]]

probdet.dens + geom_area(data = subset(d.pd, x < 0), aes(x=x, y=y), fill="#d95f02") +
  annotate("text", x = -.06, y = 17, label = ".724", size = 15, parse = TRUE)
ggsave("presentation/probdet post_prob.png", height = 6, width = 8)



# Plot intervals
mcmc_areas(ml.model.sum$beta, pars = c("unconditional"), prob = .89) +
  ggplot2::labs(
    title = "Posterior distributions of Unconditional Alliance Parameter",
    subtitle = "with median and 90% interval")

# Compare Conditional and Unconditional Coefficients
mcmc_areas(ml.model.sum$beta, pars = c("conditional", "unconditional"), prob = .9) +
  ggplot2::labs(
    title = "Posterior distributions of Conditional and Unconditional Parameters",
    subtitle = "with medians and 90% intervals")

# Compare unconditional and proportion of democracies
mcmc_areas(ml.model.sum$beta, pars = c("dem.prop", "unconditional"), prob = .9)+
  ggplot2::labs( 
   title = "Posterior distributions of Democratic Proportion and Unconditional Parameters",
    subtitle = "with medians and 90% intervals")

# Plot alliance-level regression positive posterior probabilities
beta.probs <- cbind.data.frame(beta.probs, c("Alliance Model Constant", "Probabilistic Deterrent", "Conditional", "Unconditional", 
                          "Compellent", "Number of Members", 
                          "Share Dem. Members", "Wartime Alliance", "Insitutionalization", 
                          "Military Aid", "US Member", "Russia Member"))

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
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = alliance.type)) + 
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


### TODO(JOSH):
# 2: Compare US unconditional alliance with Israel to other treaties
# Use %in% to get non-zero alliances and plot full posteriors. 