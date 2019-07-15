# Joshua Alley
# Texas A&M University
# Comparisons of other alliance characteristics 


# Examine Trends in lambda parameters across different characteristics. 


### Violin plot of wartime alliances 
# major powers
ggplot(lambda.df.maj, aes(x = as.factor(wartime), y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem)) + theme_classic()


# non-major powers
ggplot(lambda.df.min, aes(x = as.factor(wartime), y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem)) + theme_classic()



### violin plots for asymmetric obligations
# major powers
ggplot(lambda.df.maj, aes(x = as.factor(asymm), y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem)) + theme_classic()


# non-major powers
ggplot(lambda.df.min, aes(x = as.factor(asymm), y = lambda)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = num.mem)) + theme_classic()



### look for trends in FP similarity
# major powers
ggplot(lambda.df.maj, aes(x = low.kap.sc, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "FP Similarity", y = "Effect of Alliance Part") +
  ggtitle("Major Powers")


# non-major powers
ggplot(lambda.df.min, aes(x = low.kap.sc, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "FP Similarity", y = "Effect of Alliance Part") +
  ggtitle("Non-Major Powers")



### look for trends in democratic proportion
# major powers
ggplot(lambda.df.maj, aes(x = avg.democ, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Average Democracy", y = "Effect of Alliance Part") +
  ggtitle("Major Powers")


# non-major powers
ggplot(lambda.df.min, aes(x = avg.democ, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Average Democracy", y = "Effect of Alliance Part") +
  ggtitle("Non-Major Powers")



### look for trends in numbver of members
# major powers
ggplot(lambda.df.maj, aes(x = num.mem, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Alliance Size", y = "Effect of Alliance Part") +
  ggtitle("Major Powers")


# non-major powers
ggplot(lambda.df.min, aes(x = num.mem, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_classic() +
  labs(x = "Alliance Size", y = "Effect of Alliance Part") +
  ggtitle("Non-Major Powers")




### Examine full impact of alliance participation


# matrix multiplication of membership matrix by mean lambda 
lambda.maj.split <- extract(ml.model.maj, pars = c("lambda"), permuted = TRUE)

# agg.all.maj.full  <- state.mem.maj%*%t(ml.model.sum$lambda_maj)
agg.all.maj.full  <- state.mem.maj%*%t(lambda.maj.split$lambda)

# summarize the 90% credible interval 
agg.all.maj.sum <- t(apply(agg.all.maj.full, 1, function(x) quantile(x, c(.05, .95))))

agg.all.maj.sum <- cbind.data.frame(reg.state.comp.maj$ccode, reg.state.comp.maj$year, 
                                    apply(agg.all.maj.full, 1, mean), agg.all.maj.sum)
colnames(agg.all.maj.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")

# Add state and year info to full data 
agg.all.maj.full <- cbind.data.frame(reg.state.comp.maj$ccode,
                                     reg.state.comp.maj$year, agg.all.maj.full)
colnames(agg.all.maj.full)[1] <- "ccode"
colnames(agg.all.maj.full)[2] <- "year"


## United States after 1942, when they start with alliances 
us.agg.melt <- agg.all.maj.full %>% 
  filter(ccode == 2 & year >= 1942) %>%
  melt(id.vars = c("ccode", "year")) 


agg.all.maj.sum %>%
  filter(ccode == 2 & year >= 1942) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on US Defense Spending") +
  theme_bw()

ggplot(us.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 5) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.17, 0.13) +
  labs(x = "Agg. Impact", y = "Year") +
  ggtitle("Aggregate Impact of Alliance Participation on US Defense Spending: 1942-2007") 


## USSR after 1935
ussr.agg.melt <- agg.all.maj.full %>% 
  filter(ccode == 365 & year >= 1935) %>%
  melt(id.vars = c("ccode", "year")) 


agg.all.maj.sum %>%
  filter(ccode == 365 & year >= 1935) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Russian Defense Spending") +
  theme_bw()

ggplot(ussr.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 5) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.2, 0.22) +
  labs(x = "Agg. Impact", y = "Year") +
  ggtitle("Aggregate Impact of Alliance Participation on USSR Defense Spending: 1942-2007") 


## The UK 
uk.agg.melt <- agg.all.maj.full %>% 
  filter(ccode == 200) %>%
  melt(id.vars = c("ccode", "year")) 


agg.all.maj.sum %>%
  filter(ccode == 200) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on UK Defense Spending") +
  theme_bw()

# Sheer number of years makes this plot hard to read
ggplot(uk.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.18, 0.22) +
  labs(x = "Agg. Impact", y = "Year") +
  ggtitle("Aggregate Impact of Alliance Participation on UK Defense Spending") 


## France
fr.agg.melt <- agg.all.maj.full %>% 
  filter(ccode == 220) %>%
  melt(id.vars = c("ccode", "year")) 


agg.all.maj.sum %>%
  filter(ccode == 220) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on French Defense Spending") +
  theme_bw()

ggplot(fr.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.13, 0.22) +
  ggtitle("Aggregate Impact of Alliance Participation on UK Defense Spending") 




# matrix multiplication of membership matrix by mean lambda 
lambda.min.split <- extract(ml.model.min, pars = c("lambda"), permuted = TRUE)

# Multiply by state membeship matrix
agg.all.min.full  <- state.mem.min%*%t(lambda.min.split$lambda)

# summarize the 90% credible interval 
agg.all.min.sum <- t(apply(agg.all.min.full, 1, function(x) quantile(x, c(.05, .95))))

agg.all.min.sum <- cbind.data.frame(reg.state.comp.min$ccode, reg.state.comp.min$year, 
                                    apply(agg.all.min.full, 1, mean), agg.all.min.sum)
colnames(agg.all.min.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")

# Add state and year info to full data 
agg.all.min.full <- cbind.data.frame(reg.state.comp.min$ccode,
                                     reg.state.comp.min$year, agg.all.min.full)
colnames(agg.all.min.full)[1] <- "ccode"
colnames(agg.all.min.full)[2] <- "year"


# Belgium
bel.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 211 & year >= 1925) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 211 & year >= 1925) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Belgian Defense Spending") +
  theme_bw()

ggplot(bel.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.1, 0.13) +
  ggtitle("Aggregate Impact of Alliance Participation on Belgian Defense Spending") 


# Canada
can.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 20 & year >= 1942) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 20) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Canadian Defense Spending") +
  theme_bw()

ggplot(can.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.1, 0.13) +
  ggtitle("Aggregate Impact of Alliance Participation on Canadian Defense Spending") 


# spain 
esp.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 230 & year >= 1950) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 230 & year >= 1950) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Spanish Defense Spending") +
  theme_bw()

ggplot(esp.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.1, 0.03) +
  ggtitle("Aggregate Impact of Alliance Participation on Spanish Defense Spending") 


# Egypt
egy.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 651) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 651) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Egyptian Defense Spending") +
  theme_bw()

ggplot(egy.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.12, 0.04) +
  ggtitle("Aggregate Impact of Alliance Participation on Egyptian Defense Spending") 



# North Korea
nk.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 731) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 731) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on North Korean Defense Spending") +
  theme_bw()

ggplot(nk.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.07, 0.04) +
  ggtitle("Aggregate Impact of Alliance Participation on North Korean Defense Spending") 



# Philippines 
phl.agg.melt <- agg.all.min.full %>% 
  filter(ccode == 840) %>%
  melt(id.vars = c("ccode", "year")) %>%
  filter(value != 0)


agg.all.min.sum %>%
  filter(ccode == 840) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of Alliance Participation on Philippine Defense Spending") +
  theme_bw()

ggplot(phl.agg.melt, aes(x = value, y = year, group = year)) + 
  scale_y_reverse() +
  geom_vline(xintercept = 0) +
  geom_density_ridges(rel_min_height = 0.03, scale = 3) + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  xlim(-0.04, 0.03) +
  ggtitle("Aggregate Impact of Alliance Participation on Philippine Defense Spending")



# Calculate the impact of NATO 
nato.imp.maj <- state.mem.maj[, 93]%*%t(lambda.maj.split$lambda[, 93])

# summarize the 90% credible interval 
nato.imp.maj.sum <- t(apply(nato.imp.maj, 1, function(x) quantile(x, c(.05, .95))))

nato.imp.maj.sum <- cbind.data.frame(reg.state.comp.maj$ccode, reg.state.comp.maj$year, 
                                    apply(nato.imp.maj, 1, mean), nato.imp.maj.sum)
colnames(nato.imp.maj.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")

# plot for the US
nato.imp.maj.sum %>%
  filter(ccode == 2 & year >= 1949) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of NATO Participation on US Defense Spending") +
  theme_bw()




# non-major powers
nato.imp.min <- state.mem.min[, 68]%*%t(lambda.min.split$lambda[, 68])


# summarize the 90% credible interval 
nato.imp.min.sum <- t(apply(nato.imp.min, 1, function(x) quantile(x, c(.05, .95))))

nato.imp.min.sum <- cbind.data.frame(reg.state.comp.min$ccode, reg.state.comp.min$year, 
                                     apply(nato.imp.min, 1, mean), nato.imp.min.sum)
colnames(nato.imp.min.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")

summary(nato.imp.min.sum$agg.all.impact)

# plot for Belgium (they all pretty much look like this)
nato.imp.min.sum %>%
  filter(ccode == 211 & year >= 1949) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of NATO Participation on Belgian Defense Spending") +
  theme_bw()


# plot for Canada
nato.imp.min.sum %>%
  filter(ccode == 20 & year >= 1949) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of NATO Participation on Canadian Defense Spending") +
  theme_bw()




# Calculate the impact of the EU (ATOPID 4175)
eu.imp.maj <- state.mem.maj[, 136]%*%t(lambda.maj.split$lambda[, 136])

# summarize the 90% credible interval 
eu.imp.maj.sum <- t(apply(eu.imp.maj, 1, function(x) quantile(x, c(.05, .95))))

eu.imp.maj.sum <- cbind.data.frame(reg.state.comp.maj$ccode, reg.state.comp.maj$year, 
                                     apply(eu.imp.maj, 1, mean), eu.imp.maj.sum)
colnames(eu.imp.maj.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")


# UK 
eu.imp.maj.sum %>%
  filter(ccode == 200 & year >= 1992) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of EU Participation on UK Defense Spending") +
  theme_bw()


# France
eu.imp.maj.sum %>%
  filter(ccode == 220 & year >= 1992) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of EU Participation on French Defense Spending") +
  theme_bw()



# Calculate the impact of the EU (ATOPID 4175)
eu.imp.min <- state.mem.min[, 166]%*%t(lambda.min.split$lambda[, 166])

# summarize the 90% credible interval 
eu.imp.min.sum <- t(apply(eu.imp.min, 1, function(x) quantile(x, c(.05, .95))))

eu.imp.min.sum <- cbind.data.frame(reg.state.comp.min$ccode, reg.state.comp.min$year, 
                                   apply(eu.imp.min, 1, mean), eu.imp.min.sum)
colnames(eu.imp.min.sum) <- c("ccode", "year", "agg.all.impact", "l.05", "h.90")


# Belgium
eu.imp.min.sum %>%
  filter(ccode == 211 & year >= 1992) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of EU Participation on Belgian Defense Spending") +
  theme_bw()

# Italy
eu.imp.min.sum %>%
  filter(ccode == 325 & year >= 1992) %>%
  ggplot(aes(y = agg.all.impact, x = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = h.90, ymin = l.05)) +
  geom_hline(yintercept = 0) +
  ggtitle("Aggregate Impact of EU Participation on Italian Defense Spending") +
  theme_bw()


             