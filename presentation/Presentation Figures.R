# Joshua Alley
# Texas A&M University
# R Script to Produce Figures for Presentation of Alliance Design and the Arms/Alliances Tradeoff


# Environment is determined by use of projects and/or running this file in conjunction with
# the script dataset construction and summary.R and analysis-joint.R and analysis-split-sample




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
  select(atopid, begyr, lambda, latent.depth.mean) %>%
  left_join(select(lambda.df.maj, atopid, begyr, lambda, latent.depth.mean), 
            by = c("atopid", "begyr", "latent.depth.mean")) %>% 
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


# Plot histogram of mean latent depth
ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") 
ggsave("presentation/ld-hist.png", height = 6, width = 8)

# Show UAR
ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = 2.2, linetype = "dashed", size = 1)  + 
  geom_text(label="UAE-Yemen 1958", x = 2.15, y = 30, hjust = 1, size = 5)  # UAR
ggsave("presentation/ld-hist-deep.png", height = 6, width = 8)


# Show shallow treaty
ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = -0.017, linetype = "dashed", size = 1)  + 
  geom_text(label="UK-France 1870", x = -0.011, y = 35, hjust = -0.01, size = 5) # Ukraine-India neutrality and non-aggression
ggsave("presentation/ld-hist-shallow.png", height = 6, width = 8)


# Show typical treaty
ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = 0.90289, linetype = "dashed", size = 1)  + 
  geom_text(label="Czech-German 1967", x = 0.85, y = 38, hjust = 1, size = 5) # France-Czech consul 1938
ggsave("presentation/ld-hist-median.png", height = 6, width = 8)


# Show NATO
ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = 0.66128237, linetype = "dashed", size = 1)  + 
  geom_text(label="NATO", x = 0.64, y = 38, hjust = 1, size = 5) # NATO
ggsave("presentation/ld-hist-nato.png", height = 6, width = 8)


# plot factor loadings 
ggplot(latent.factors, aes(x = mean, y = var)) + 
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = mean - 2*sd, 
                     xmax = mean + 2*sd),
                 height = .2, size = 1) +
  geom_vline(xintercept = 0) +
  labs(x = "Factor Loading", y = "Variable") +
  theme_carly_presents()
ggsave("presentation/factor-loadings.png", height = 6, width = 8) 


# Plot histogram of mean latent depth in full data
ggplot(atop, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") 
ggsave("presentation/ld-hist-full.png", height = 6, width = 8)
  

# Plot results
depth.post <- as.data.frame(coef.min$beta[, 2])
colnames(depth.post) <- c("latent.depth.mean")

depth.dens <- ggplot(data = depth.post, mapping = aes(x = latent.depth.mean)) +
  geom_density() + geom_vline(xintercept = 0, size = 1.5) + 
  labs(x = "Impact of Latent Depth") +
  theme_carly_presents()

d <- ggplot_build(depth.dens)$data[[1]] # build the density plot for filling

depth.dens + geom_area(data = subset(d, x > 0), aes(x=x, y=y), fill="#d95f02") +
  annotate("text", x = .06, y = 20, label = ".92", size = 12, parse = TRUE)

ggsave("presentation/depth-post.png", height = 6, width = 8)


### Plot intervals for alliance-level regressors
color_scheme_set("blue")
mcmc_intervals(coef.min$beta, 
               prob = .9) +
  theme_carly_presents()
ggsave("presentation/beta-intervals-min.png", height = 6, width = 8)

 
#### Plot the Means of the Lambda Parameters
lambda.probs$atopid <- reorder(lambda.probs$atopid, lambda.probs$lambda.mean)

lambda.probs %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  geom_col() + coord_flip() +
  labs(x = "Alliance", y = "Posterior Mean of Lambda Parameter")  + 
  theme_carly_presents()



### Plot treaty depth against lambda

# blank axis for illustrative purposes
ggplot(lambda.df.min, aes(x = latent.depth.mean, y = lambda)) +
  labs(x = "Latent Treaty Depth", y = "Alliance Part. Impact") +
  theme_carly_presents()
ggsave("presentation/ld-lambda-blank.png", height = 6, width = 8)

# non-major powers
lambda.df.min %>% filter(wartime == 0) %>%
ggplot(aes(x = latent.depth.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Depth", y = "Alliance Part. Impact") +
  theme_carly_presents()
ggsave("presentation/ld-lambda-min.png", height = 6, width = 8)

# Major powers 
ggplot(lambda.df.maj, aes(x = latent.depth.mean, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Depth", y = "Alliance Part. Impact") +
  theme_carly_presents()
ggsave("presentation/ld-lambda-maj.png", height = 6, width = 8)


# Impact of US alliances

lambda.min.sum %>%
  filter(us.mem == 1) %>%
  ggplot( aes(x = lambda.mean, y = atopid)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 size = 1.5) +
  labs(x = "Alliance Impact", y = "ATOPID") +
  geom_vline(xintercept = 0) +
  theme_carly_presents()
ggsave("presentation/lambda-us-min.png", height = 6, width = 8)


# Scatter plot highlighting US treaties 
# blank axis for illustrative purposes
ggplot(lambda.min.sum, aes(x = latent.depth.mean, y = lambda.mean)) +
  xlim(-.02, 2.3) + ylim(-.09, .09) +
  geom_hline(yintercept = median(lambda.min.sum$lambda.mean)) +
  geom_vline(xintercept = median(lambda.min.sum$latent.depth.mean)) +
  labs(x = "Latent Treaty Depth", y = "Alliance Impact") +
  theme_carly_presents()
ggsave("presentation/lambda-depth-us-blank.png", height = 6, width = 8)

ggplot(lambda.min.sum, aes(x = latent.depth.mean, y = lambda.mean, shape = factor(us.mem))) +
  geom_hline(yintercept = median(lambda.min.sum$lambda.mean)) +
  geom_vline(xintercept = median(lambda.min.sum$latent.depth.mean)) +
  geom_point(mapping = aes(color = factor(us.mem)), size = 4.5) +
  scale_color_brewer(palette="Dark2") +
  labs(x = "Latent Depth", y = "Alliance Impact") +
  guides(color = FALSE, shape = FALSE) +
  theme_carly_presents() 
ggsave("presentation/lambda-depth-us.png", height = 6, width = 8)


# Only US Alliances
lambda.min.sum %>%
  filter(us.mem == 1) %>%
  ggplot(aes(x = latent.depth.mean, y = lambda.mean)) +
  geom_hline(yintercept = median(lambda.min.sum$lambda.mean)) +
  geom_vline(xintercept = median(lambda.min.sum$latent.depth.mean)) +
  geom_point(size = 4) +
  labs(x = "Latent Depth", y = "Alliance Impact") +
  theme_carly_presents()
ggsave("presentation/lambda-depth-usonly.png", height = 6, width = 8)



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
illus.plot 
ggsave("presentation/illus-arg.png", height = 6, width = 9)



# Non-zero major power alliances
lambda.probs.maj %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  labs(x = "ATOPID", y = "Impact of Alliance",
       fill = "Depth") +
  theme_carly_presents() +
  coord_flip() 
ggsave("presentation/non-zero-maj.png", height = 6, width = 8)


# non-major powers
lambda.probs.min %>% 
  filter(non.zero == 1) %>% 
  ggplot(mapping = aes(x = atopid, y = lambda.mean, fill = latent.depth.mean)) + 
  geom_col() +
  scale_fill_continuous(type = "viridis") +
  labs(x = "ATOPID", y = "Impact of Alliance",
       fill = "Depth") +
  theme_carly_presents() +
  coord_flip() 
ggsave("presentation/non-zero-min.png", height = 6, width = 8)



# Plot of varying slopes results for depth
ggplot(depth.dens.joint, aes(x = value,  fill = X2)) +
  geom_density(alpha = .75) + 
  scale_fill_brewer(name = "Sample", palette = "Dark2") +
  theme_carly_presents() +
  annotate("text", x = -.09, y = 12, 
           label = ".90", size = 8, parse = TRUE) + # Note for major
  annotate("text", x = 0.07, y = 12, 
           label = ".80", size = 8, parse = TRUE) + # Note for non-major
  theme_carly_presents()
ggsave("presentation/var-slopes-depth.png", height = 6, width = 8)


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


# add coefficients from post 45 regression
mcmc_intervals(coef.post45$beta, 
               prob = .9) +
  theme_carly_presents()
ggsave("presentation/beta-intervals-post45.png", height = 6, width = 8)



### Add a posterior predictive check
color_scheme_set("viridisC")
traceplot(ml.model.maj, pars = "beta")
ggsave("presentation/beta-trace-maj.png", height = 6, width = 8)
traceplot(ml.model.min, pars = "beta")
ggsave("presentation/beta-trace-min.png", height = 6, width = 8)


# Simulation check results
# Start with beta- second-level regression parameters
b1.sim.plot.pres <- mcmc_areas(sim.est.sum$beta, pars = c("beta1"), prob = .9) +
  vline_at(true.beta[1], color = "black", size = 2) + theme_carly_presents()
b1.sim.plot.pres
b2.sim.plot.pres <- mcmc_areas(sim.est.sum$beta, pars = c("beta2"), prob = .9) +
  vline_at(true.beta[2], color = "black", size = 2) + theme_carly_presents()
b2.sim.plot.pres
multiplot.ggplot(b1.sim.plot.pres, b2.sim.plot.pres)




### Single-level regression check 
pres.mplot <- multiplot(rreg.min, rreg.maj, m1r.reg,
                        names = c("Non-Major Powers", "Major Powers", "All States"),
                        coefficients = "avg.depth",
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

# plot for single-level model
cplot(m1.all.irel, x = "avg.treaty.contrib", dx = "avg.depth", what = "effect",
      main = "Conditional Relationship between Depth and Treaty Contribution",
      xlab = "Average Share of Allied Capability", ylab = "Marginal Effect of Average Alliance Depth")
abline(h = 0)

