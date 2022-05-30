
# Import ------------------------------------------------------------------


BaselineGlanceDist <- read_csv("Data/BaselineGlanceDist.csv", col_names = FALSE) %>% 
  dplyr::rename(t = X1, count = X2)
head(BaselineGlanceDist)


# Derive datasets ---------------------------------------------------------


# Glance distribution.
zeroProb <- BaselineGlanceDist$count[1] / sum(BaselineGlanceDist$count)

x <- with(BaselineGlanceDist %>% filter(t > 0), rep(t, count))
fit <- fitdistrplus::fitdist(x, "lnorm")

glance <- BaselineGlanceDist %>% 
  mutate(relfreq = count / sum(count), 
         fitted_prob = dlnorm(t, meanlog  = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]),
         fitted_prob = (t == 0) * zeroProb + (t > 0) * (1 - zeroProb) * fitted_prob / sum(fitted_prob)) # Normalise.
head(glance)


# Overshot distribution
overshot <- glance %>%
  mutate(prob = rev(cumsum(rev(fitted_prob / 1:length(fitted_prob)))),
         prob = prob / sum(prob)) %>% # Normalise.
  dplyr::select(-count, -relfreq, -fitted_prob) 
head(overshot)


# Bivariate undershot/overshot distribution.
biv <- crossing(before = seq(0, 6.1, 0.1), after = seq(0, 6.1, 0.1), glance) %>% 
  filter(abs(t - (before + after)) < 0.01) %>% 
  group_by(t) %>% 
  mutate(prob = fitted_prob / length(fitted_prob)) %>% 
  ungroup() %>% 
  arrange(after, before) %>% 
  dplyr::select(before, after, t, prob) 
head(biv)


# Check glance distribution
biv %>% 
  group_by(t) %>% 
  summarise(prob = sum(prob))


# Check overshot distribution. 
biv %>% 
  group_by(after) %>% 
  summarise(prob = sum(prob)) %>% 
  ungroup()


# Percentiles.
pctls <- overshot %>% 
  filter(t > 0) %>% 
  mutate(ecdf = cumsum(prob / sum(prob))) %>%
  filter(row_number() %in% c(which(ecdf > 0.5)[1],
                             which(ecdf > 0.9)[1],
                             which(ecdf > 0.99)[1],
                             which(ecdf > 0.999)[1])) %>%
  mutate(pctl = c(0.5, 0.9, 0.99, 0.999)) 
head(pctls)