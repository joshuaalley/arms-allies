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
ggplot(atop.milsup, aes(x = latent.depth.mean)) + 
  geom_histogram(color = "black", fill = "grey") +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") 
ggsave("presentation/ld-hist.png", height = 6, width = 8)

# Show deep treaty: UAE-Yemen 1958 (ATOPID 3345)
# Military Union (organization), central high command, official contact 
ggplot(atop.milsup, aes(x = latent.depth.mean)) + 
  geom_histogram(color = "black", fill = "grey") +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = 1.95, linetype = "dashed", size = 1)  + 
  geom_text(label="UAE-Yemen 1958", x = 1.9, y = 60, hjust = 1, size = 5)  # UAR
ggsave("presentation/ld-hist-deep.png", height = 6, width = 8)


# Show shallow treaty: ATOPID 2135 France Poland 1925 with new measure (-0.8263845)
# Nothing
ggplot(atop.milsup, aes(x = latent.depth.mean)) + 
  geom_histogram(color = "black", fill = "grey") +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = -0.8, linetype = "dashed", size = 1)  + 
  geom_text(label="France-Poland 1925", x = -0.7, y = 60, hjust = -0.1, size = 5) 
ggsave("presentation/ld-hist-shallow.png", height = 6, width = 8)


# Show typical treaty: 
atop.milsup$atopid[atop.milsup$latent.depth.mean == median(atop.milsup$latent.depth.mean)]
# Defense pact with regular intergovernmental meetings 
ggplot(atop.milsup, aes(x = latent.depth.mean)) + 
  geom_histogram(color = "black", fill = "grey") +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = -0.09, linetype = "dashed", size = 1)  + 
  geom_text(label="OAS", x = -0.05, y = 60, hjust = -.1, size = 5) 
ggsave("presentation/ld-hist-median.png", height = 6, width = 8)


# Show NATO
atop.milsup$latent.depth.mean[atop.milsup$atopid == 3180]
ggplot(atop.milsup, aes(x = latent.depth.mean)) + 
  geom_histogram(color = "black", fill = "grey") +
  theme_carly_presents() + labs(x = "Mean Latent Depth", y = "Treaties") +
  geom_vline(xintercept = 0.33, linetype = "dashed", size = 1)  + 
  geom_text(label="NATO", x = 0.4, y = 60, hjust = 1.2, size = 5) # NATO
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



# Plot results
depth.post <- as.data.frame(coef.min$beta[, 2])
colnames(depth.post) <- c("latent.depth.mean")

depth.dens <- ggplot(data = depth.post, mapping = aes(x = latent.depth.mean)) +
  geom_density() + geom_vline(xintercept = 0, size = 1.5) + 
  labs(x = "Impact of Latent Depth") +
  theme_carly_presents()

d <- ggplot_build(depth.dens)$data[[1]] # build the density plot for filling

depth.dens + geom_area(data = subset(d, x < 0), aes(x=x, y=y), fill="#d95f02") +
  annotate("text", x = -.07, y = 15, label = ".96", size = 12, parse = TRUE)

ggsave("presentation/depth-post.png", height = 6, width = 8)


### Plot intervals for alliance-level regressors
color_scheme_set("blue")
mcmc_intervals(coef.min$beta, 
               prob = .9) +
  theme_carly_presents()
ggsave("presentation/beta-intervals-min.png", height = 6, width = 8)


# substantive effect calculation
color_scheme_set("darkgray")
mcmc_areas(lambda.change.depth, pars = "impact.milex", prob = .9) +
  geom_vline(xintercept = 0) +
  ggtitle("Predicted Impact on Military Spending") +
  theme_carly_presents() +
  theme(axis.text.y = element_blank())


impact.milex.nonmaj +
  ggtitle("") +
  theme_carly_presents() 
ggsave("presentation/pred-impact-depth.png", height = 6, width = 8)


### Plot treaty depth against lambda

# blank axis for illustrative purposes
ggplot(lambda.df.min, aes(x = latent.depth.mean, y = lambda)) +
  labs(x = "Latent Treaty Depth", y = "Alliance\n Coefficient") +
  theme_carly_presents()
ggsave("presentation/ld-lambda-blank.png", height = 6, width = 8)

# non-major powers
ggplot(lambda.df.min, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Latent Treaty Depth", y = "Alliance\n Coefficient") +
  theme_carly_presents()
ggsave("presentation/ld-lambda-min.png", height = 6, width = 8)


# Plot with substantive estimates from observed data
ggplot(growth.pred.res, aes(x = latent.depth.mean, y = mean.pred)) +
  geom_hline(yintercept = 0) +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("#999999","#333333"), 
                       name = "Frequency", 
                       na.value=NA) +
  labs(x = "Latent Treaty Depth",
       y = "Predicted\n % Change") +
  theme_carly_presents()
ggsave("presentation/ld-growth-min.png", height = 6, width = 8)


# Major powers 
ggplot(lambda.df.maj, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
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
  xlim(-.85, 2.3) + ylim(-.12, .21) +
  geom_hline(yintercept = median(lambda.min.sum$lambda.mean)) +
  geom_vline(xintercept = median(lambda.min.sum$latent.depth.mean)) +
  labs(x = "Latent Depth", y = "Alliance Impact") +
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

# Plot with substantive estimates from observed data:
# US alliances only
growth.pred.res %>% 
  filter(us.mem == 1) %>%
  ggplot(aes(x = latent.depth.mean, y = mean.pred)) +
  geom_hline(yintercept = 0) +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("#999999","#333333"), 
                       name = "Frequency", 
                       na.value=NA) +
  labs(x = "Latent Treaty Depth",
       y = "Predicted\n % Change") +
  theme_carly_presents()


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



# Plot depth and uncertainty against start year
ls.styear + theme_carly_presents()
ggsave("presentation/ld-start-year.png", height = 6, width = 8)



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
ggplot(depth.dens.joint, aes(x = value,  fill = Var2)) +
  geom_density(alpha = .75) + 
  scale_fill_brewer(name = "Sample", palette = "Dark2") +
  theme_carly_presents() 
ggsave("presentation/var-slopes-depth.png", height = 6, width = 8)


# All the coefficients from the varying slopes model
color_scheme_set("blue")
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



### models with alternative depth measures
# Leeds and Anac Milinst

# comparison
ggplot(atop.milsup, aes(x = as.ordered(milinst), y = latent.depth.mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .5) +
  labs(x = "Military Institutionalization",
       y = "Latent\n Depth") +
  theme_carly_presents()
ggsave("presentation/milinst-comp.png", height = 5, width = 8)

# show lambda res
ggplot(lambda.df.milinst, aes(x = as.factor(milinst), y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .5) +
  theme_carly_presents() +
  labs(x = "Military Instituionalization", y = "Alliance\n Coefficient") 
ggsave("presentation/milinst-lambda.png", height = 6, width = 8)

# benson and clinton
bc.score.comp + 
  labs(y = "Rescaled Benson and\n Clinton",
       x = "Rescaled BFA Measure") +
  theme_carly_presents()
ggsave("presentation/bc-score-comp.png", height = 6, width = 8)

factors.comp + 
  scale_color_brewer(palette="Dark2") +
  labs(y = " ") + ggtitle("") +
  theme_carly_presents()
ggsave("presentation/bc-factor-comp.png", height = 6, width = 8)


### Single-level regression check 
# Average depth
coef.avg + theme_carly_presents() 
ggsave("presentation/single-level-average.png", height = 6, width = 8) 

# Dummy for deep alliance presence
coef.dum + theme_carly_presents() 
ggsave("presentation/single-level-dummy.png", height = 6, width = 8) 




