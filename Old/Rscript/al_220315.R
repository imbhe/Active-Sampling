
# Init --------------------------------------------------------------------

rm(list = ls())

library("cowplot")
library("fitdistrplus")
library("ggforce")
library("glmnet")
library("magrittr")
library("readxl")
library("tidyverse")
library("boot")
library("rsample")
library("purrr")


# set.seed(123456)
ptsize <- 8
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "sans"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             legend.key.width = unit(1.25, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin = margin(t = -0.25, unit = 'cm'),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = ptsize, colour = "black", family = "sans"),
             legend.title = element_text(size = ptsize, colour = "black", family = "sans"),
             text = element_text(size = ptsize, colour = "black", family = "sans"),
             plot.title = element_text(size = ptsize, colour = "black", family = "sans", face = "plain", hjust = 0),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             strip.background.x = element_blank())

setwd("C:/Users/imbergh/Box Sync/Forskning/What if/Rscript")


# Import ------------------------------------------------------------------


BaselineGlanceDist <- read_csv("./../Data/BaselineGlanceDist.csv", col_names = FALSE) %>% 
  dplyr::rename(t = X1, count = X2)



# Fit glance distribution -------------------------------------------------


zeroProb <- BaselineGlanceDist$count[1] / sum(BaselineGlanceDist$count)

x <- with(BaselineGlanceDist %>% filter(t > 0), rep(t * 10 - 1, count))
fit1 <- fitdist(x, "nbinom")

x <- with(BaselineGlanceDist %>% filter(t > 0), rep(t, count))
fit2 <- fitdist(x, "gamma")
fit3 <- fitdist(x, "lnorm")

logLik(fit1)
logLik(fit2)
logLik(fit3)
# Use log-normal distribution.




# Derive datasets ---------------------------------------------------------


# Glance distribution.
glance <- BaselineGlanceDist %>% 
  mutate(relfreq = count / sum(count), 
         fitted_prob = dlnorm(t, meanlog  = fit3$estimate["meanlog"], sdlog = fit3$estimate["sdlog"]),
         fitted_prob = (t == 0) * zeroProb + (t > 0) * (1 - zeroProb) * fitted_prob / sum(fitted_prob)) # Normalise.


# Overshot distribution
overshot <- glance %>%
  mutate(prob = rev(cumsum(rev(fitted_prob / 1:length(fitted_prob)))),
         prob = prob / sum(prob)) %>% # Normalise.
  dplyr::select(-count, -relfreq, -fitted_prob) 


# Bivariate undershot/overshot distribution.
biv <- crossing(before = seq(0, 6.1, 0.1), after = seq(0, 6.1, 0.1), glance) %>% 
  filter(abs(t - (before + after)) < 0.01) %>% 
  group_by(t) %>% 
  mutate(prob = fitted_prob / length(fitted_prob)) %>% 
  ungroup() %>% 
  arrange(after, before) %>% 
  dplyr::select(before, after, t, prob) 


# Check glance distribution
biv %>% 
  group_by(t) %>% 
  summarise(prob = sum(prob))
glance


# Check overshot distribution. 
biv %>% 
  group_by(after) %>% 
  summarise(prob = sum(prob)) %>% 
  ungroup() 
overshot


# Percentiles.
pctls <- overshot %>% 
  filter(t > 0) %>% 
  mutate(ecdf = cumsum(prob / sum(prob))) %>%
  filter(row_number() %in% c(which(ecdf > 0.5)[1],
                             which(ecdf > 0.9)[1],
                             which(ecdf > 0.99)[1],
                             which(ecdf > 0.999)[1])) %>%
  mutate(pctl = c(0.5, 0.9, 0.99, 0.999))



# Some functions ----------------------------------------------------------


# Simulate outcome.
simulateY <- function(t1, t2, beta = runif(2, 0, 1) * c(1, 0.1) + c(0.5, 0), sigma = runif(1, 0, 1), ymax = 30, q = c(runif(1, 0, 0.5), runif(1, 0.9, 1)), seed = NULL) {
  # beta = c(1, 0.1); sigma = 0.5; ymax = 30; q = c(0.10, 0.95);
  # beta = c(1, 0.1); sigma = 0.1; ymax = 30; q = c(0.10, 0.95);
  
  if ( !is.null(seed) ) {
    set.seed(seed)
  }
  
  z <- as.numeric(cbind(t1, t2) %*% beta) + rnorm(length(t1), sd = sigma)
  
  zmin <- quantile(z, min(q))
  zmax <- quantile(z, max(q))
  
  min_ix <- which(z <= zmin)
  z2 <- z
  z2[z <= zmin] <- zmin
  for (i in min_ix) {
    z2[which(t2 <= t2[i] & t1 <= t1[i])] <- zmin
  }
  
  max_ix <- which(z2 >= zmax)
  z3 <- z2
  z3[2 >= zmax] <- zmax
  for (i in max_ix) {
    z3[which(t2 >= t2[i] & t1 >= t1[i])] <- zmax
  }
  z3[length(z3)] <- zmax
  
  y <- (z3 - zmin) / max(z3 - zmin) * ymax
  
  return(y)
}


# Simulate sample of cases.
simulateSample <- function(ncases = 10, seed = NULL) {
  
  if ( !is.null(seed) ) {
    set.seed(seed)
  }
  
  df <- NULL
  for ( i in 1:ncases ) {
    
    dfi <- biv %>%
      mutate(case_id = i, 
             y = simulateY(t1 = after, t2 = before, beta = runif(2, 0, 1) * c(1, 0.1) + c(0.5, 0), sigma = runif(1, 0, 1), ymax = runif(1, 10, 30), q = c(runif(1, 0, 0.5), runif(1, 0.9, 1))))
    
    df <- df %>% 
      rbind(dfi)
    
  }
  
  return(df)
  
}


# Active learning.
active_learning <- function(df, niter = 20, seed = NULL) {
  
  if ( !is.null(seed) ) {
    set.seed(seed)
  }
  
  grid <- crossing(after = seq(0, 4, 2), before = seq(0, 4, 2)) %>% 
    filter(after + before <= 6.1) %>% 
    add_row(after = 6.1,  before = 0) %>% 
    add_row(after = 0,  before = 6.1)
  
  
  labelled <- df %>% 
    filter(after %in% grid$after & before %in% grid$before) %>% 
    mutate(w = 1,
           selected = 1)
  
  unlabelled <- df %>% 
    filter(!(after %in% grid$after) | !(before %in% grid$before)) %>% 
    mutate(w = 0,
           selected = 0)
  
  
  ymax <- max(labelled$y)
  ymin <- min(labelled$y)
  
  
  labelled0 <- labelled
  
  
  # Add certainty non-crashes and crashes to labelled set, remove from unlabelled set.
  for ( i in 1:nrow(labelled0) ){
    
    newx <- labelled0[i, ]
    
    if ( newx$y == 0) { # Add certainty non-crashes.
      add <- unlabelled %>% 
        filter(before <= newx$before & after <= newx$after) 
      
      labelled <- labelled %>%
        add_row(add)
      
      unlabelled %<>%
        filter(before > newx$before | after > newx$after) 
    } else if ( newx$y == ymax ) { # Add certainty max impact speed crashes.
      
      add <- unlabelled %>%
        filter(before >= newx$before & after >= newx$after) 
      
      labelled <- labelled %>%
        add_row(add)
      
      unlabelled %<>% 
        filter(before < newx$before | after < newx$after)
    }
    
  }

  
  # Iterate
  j <- 1
  for ( i in 1:niter ) {
    
    if ( ymin == 0 ) {
      
      # Find no-collision corner cases.
      selected0 <- labelled %>% filter(y == 0 & selected)
      exclude <- numeric(0)
      if ( nrow(selected0) == 1 ) {
        corners0 <- selected0
      } else {
        for ( j in 1:nrow(selected0) ) {
          exclude <- c(exclude, setdiff(which(selected0$after[-j] <= selected0$after[j] & 
                                                selected0$before[-j] <= selected0$before[j]), j))
        }
      }
      ix <- setdiff(1:nrow(selected0), exclude)
      corners0 <- selected0[ix, ]
      
      
      # Find max impact speed corner cases.
      selectedMax <- labelled %>% filter(y >= ymax & selected)
      exclude <- numeric(0)
      if ( nrow(selectedMax) == 1 ) {
        cornersMax <- selectedMax
      } else {
        for ( j in 1:nrow(selectedMax) ) {
          exclude <- c(exclude, setdiff(which(selectedMax$after[-j] >= selectedMax$after[j] & 
                                                selectedMax$before[-j] >= selectedMax$before[j]), j))
        }
      }
      ix <- setdiff(1:nrow(selectedMax), exclude)
      cornersMax <- selectedMax[ix, ]
      
      if ( nrow(cornersMax) > 1) {
        cornersMax %<>% filter(!(after == 6.1 & before == 0))
      }
      
      
      #  Find all collision border cases.
      selectedCrash <- labelled %>% filter(y > 0 & selected)
      exclude <- numeric(0)
      if ( nrow(selectedCrash) == 1 ) {
        cornersMax <- selectedCrash
      } else {
        for ( j in 1:nrow(selectedCrash) ) {
          exclude <- c(exclude, setdiff(which(selectedCrash$after >= selectedCrash$after[j] &
                                                selectedCrash$before >= selectedCrash$before[j]), j))
        }
      }
      ix <- setdiff(1:nrow(selectedCrash), exclude)
      cornersCrash <- selectedCrash[ix, ]
      
      if ( nrow(cornersCrash) > 1) {
        cornersCrash %<>% filter(!(after == 6.1 & before == 0))
      }
      
      
      # Estimate impact of glancing time before/after tauinv.
      train <- selectedCrash %>%
        add_row(corners0) %>% 
        dplyr::select(after, before, y, w) %>% 
        as.matrix()
      
      if ( nrow(train) >= 5 ) {
        
        X <- train[, c("after", "before")]
        Y <- train[, "y"]
        mod <- lm(Y~X)
        beta <- coef(mod)
        mult <- ifelse(beta[2] < 0 | beta[3] < 0, 0.2, beta[3] / beta[2])
      } else {
        mult <- 1
      }
      
      
      # Estimate collision probability.
      A <- c(cornersCrash$after + mult * cornersCrash$before, corners0$after + mult * corners0$before)
      b <- c(rep(log(0.99) - log(0.01), nrow(cornersCrash)), rep(log(0.01) - log(0.99), nrow(corners0)))
      alpha <- coef(lm(b~A))
      if ( is.na(alpha[2]) || alpha[2] < 0 ) {
        A <- c(cornersCrash$after, corners0$after)
        alpha <- coef(lm(b~A))
      }
      alpha <- c(alpha, alpha[2] * mult)     
      
      
      # Estimate impact speed.
      A <- c(selectedCrash$after + mult * selectedCrash$before, corners0$after + mult * corners0$before)
      b <- c(selectedCrash$y, corners0$y)
      beta1 <- coef(lm(b~A))
      beta1 <- c(beta1, beta1[2] * mult)
      
      A <- c(cornersMax$after + mult * cornersMax$before, corners0$after + mult * corners0$before)
      b <- c(cornersMax$y, corners0$y)
      beta2 <- coef(lm(b~A))
      beta2 <- c(beta2, beta2[2] * mult)
      
      
      unlabelled %<>% 
        mutate(prob_positive = (1 + exp(-(alpha[1] + alpha[2] * after + alpha[3] * before)))^(-1),
               yhat1 = pmin(pmax(beta1[1] + beta1[2] * after + beta1[3] * before, 0.1), ymax),
               yhat2 = pmin(pmax(beta2[1] + beta2[2] * after + beta2[3] * before, 0.1), ymax),
               yhat = pmax(yhat1, yhat2),
               # size = yhat + yhat^2 + yhat^3 + yhat^4) 
               size = yhat / sd(yhat) + yhat^2 / sd(yhat^2) + yhat^3 / sd(yhat^3) +  yhat^4 / sd(yhat^4))
      
      
      for ( j in 1:nrow(unlabelled) ) {
        unlabelled$prob_positive[j] <- ifelse(any(unlabelled$after[j] >= cornersCrash$after & 
                                                    unlabelled$before[j] >= cornersCrash$before), 
                                              1, unlabelled$prob_positive[j])
      }
      
    } else {
      
      print("This has not been implemented yet!")
      
    }
    
    
    # Add certainty selections.
    if ( i > 1) {
      if ( newx$y == 0 ) {
        
        certaintySelections <- unlabelled %>% 
          filter(before <= newx$before & after <= newx$after) %>%
          add_row(labelled) %>% 
          mutate(pi = 1) %>%
          dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2, -size)
        
        unlabelled %<>%
          filter(before > newx$before | after > newx$after)
        
      } else if ( newx$y == ymax ) {
        
        certaintySelections <- unlabelled %>%
          filter(before >= newx$before & after >= newx$after) %>%
          add_row(labelled) %>% 
          mutate(pi = 1) %>%
          dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2, -size)
        
        unlabelled %<>% 
          filter(before < newx$before | after < newx$after)
        
      } else {
        certaintySelections <- labelled %>% 
          mutate(pi = 1)
      }
    } else {
      certaintySelections <- labelled %>% 
        mutate(pi = 1)
    }
    
  
    # Optimise sampling scheme.
    pi <- with(unlabelled, prob * sqrt(prob_positive) * size)
    pi <- pi / sum(pi)
    
    
    # Sample new observation.
    ix <- which(rmultinom(1, size = 1, pi) == 1)

        
    # Data for new observation.
    newx <- unlabelled %>%
      filter(row_number() == ix) %>% 
      mutate(selected = 1, 
             pi = pi[ix]) %>% 
      dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2, -size)

    
    # Update labelled and unlabelled sets.
    labelled <- certaintySelections %>%
      add_row(newx) %>%
      mutate(w = w + (1 / pi - w) / j) %>%
      dplyr::select(-pi)
    
    unlabelled %<>%
      filter(row_number() != ix) %>%
      dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2, -size)
    
    j <- j + (newx$y > 0)

  }
  
  labelled <- labelled 
  
  crashes <- labelled %>% 
    filter(y > 0) %>% 
    mutate(psw = w / sum(w))
  
  noncrashes <- labelled %>% 
    filter(y == 0)
  
  return(list(all = labelled, crashes = crashes, noncrashes = noncrashes))
  
}


df <- simulateSample(ncases = 40)
plt <- df %>% 
  filter(case_id <= 4)

ggplot(data = plt) +
  geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
  scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
  coord_equal() + 
  expand_limits(fill = 30) + 
  facet_wrap(~case_id, nrow = 2) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "EOFF before tauinv = 0.2 (s)",
       fill = "Impact speed (km/h)") + 
  theme(legend.direction = "horizontal", 
        legend.justification = 0.5,
        legend.position = "bottom", 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        strip.text = element_blank())
ggsave("Sample.png", dpi = 1000, width = 180, height = 180, unit = "mm")


bins0 <- df %>% 
  filter(y > 0) %>% 
  mutate(y = floor(y)) %>% 
  group_by(y) %>% 
  summarise(wt = sum(prob)) %>% 
  ungroup() %>% 
  mutate(prob = wt / sum(wt)) 

ggplot(bins0) + 
  geom_col(aes(x = y, y = prob), width = 0.9) + 
  labs(x = "Impact speed (km/h)",
       y = "Relative frequency")
ggsave("OutcomeDist.png", dpi = 1000, width = 90, height = 70, unit = "mm")


all <- NULL
crashes <- NULL
for ( i in unique(df$case_id) ) {
  al <- active_learning(df %>% filter(case_id == i), niter = 20)
  
  all <- all %>%
    rbind(al$all)
  
  crashes <- crashes %>%
    rbind(al$crashes)
}

plt <- all %>% 
  filter(case_id <= 4)


ggplot(data = plt) +
  geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
  scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
  coord_equal() + 
  expand_limits(fill = 30) + 
  facet_wrap(~case_id, nrow = 2) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "EOFF before tauinv = 0.2 (s)",
       fill = "Impact speed (km/h)") + 
  theme(legend.direction = "horizontal", 
        legend.justification = 0.5,
        legend.position = "bottom", 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        strip.text = element_blank())
ggsave("Labelled.png", dpi = 1000, width = 180, height = 180, unit = "mm")


binest1 <- crashes %>% 
  mutate(y = floor(y / 2) * 2) %>% 
  group_by(y) %>% 
  summarise(w = sum(prob * w)) %>% 
  ungroup() %>% 
  mutate(prob = w / sum(w)) 

ggplot(binest1) + 
  geom_col(aes(x = y, y = prob), width = 1.8) + 
  labs(x = "Impact speed (km/h)",
       y = "Relative frequency")
ggsave("OutcomeDist_Estimated1.png", dpi = 1000, width = 90, height = 70, unit = "mm")

binest2 <- crashes %>% 
  mutate(y = floor(y / 2) * 2) %>% 
  group_by(y) %>% 
  summarise(w = sum(prob * psw)) %>% 
  ungroup() %>% 
  mutate(prob = w / sum(w)) 

ggplot(binest2) + 
  geom_col(aes(x = y, y = prob), width = 1.8) + 
  labs(x = "Impact speed (km/h)",
       y = "Relative frequency")
ggsave("OutcomeDist_Estimated2.png", dpi = 1000, width = 90, height = 70, unit = "mm")


mu0 <- with(df %>% filter(y >0), sum(y * prob) / sum(prob))
muhat1 <- with(crashes, sum(y * prob * w) / sum(prob * w))
muhat2 <- with(crashes, sum(y * prob * psw) / sum(prob * psw))

print(mu0)
print(muhat1)
print(muhat2)

bs <- crashes %>% nest(data = -case_id) %>% bootstraps(times = 1000)
bs2 <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = -case_id) %>%
                       summarize(muhat1 = sum(y * prob * w) / sum(prob * w),
                                 muhat2 = sum(y * prob * psw) / sum(prob * psw))) %>% 
  bind_rows(.id = 'boots')

ci <- bs2 %>% 
  summarise(lcl1 = quantile(muhat1, 0.025), 
            ucl1 = quantile(muhat1, 0.975),
            lcl2 = quantile(muhat2, 0.025), 
            ucl2 = quantile(muhat2, 0.975)) %>% 
  mutate(hl1 = ucl1 - lcl1,
         hl2 = ucl2 - lcl2)

print(ci)

nll <- function(par, y, w, p, dens) {
  - sum(w * p * dens(y, par[1], par[2], log = TRUE))
}

kl <- function(x, par1, par2, dens) {
  integrate(function(x) dens(x, par1[1], par1[2]) * (dens(x, par1[1], par1[2], log = TRUE) - dens(x, par2[1], par2[2], log = TRUE)), 0, Inf)
}

fit0a <- with(df %>% filter(y > 0), optim(c(0, 1), function(par) nll(par, dens = dlnorm, y = y, w = 1, p = prob)))
fit0b <- with(df %>% filter(y > 0), optim(c(0.01, 0.01), function(par) nll(par, dens = dgamma, y = y, w = 1, p = prob), lower = 0, method = "L-BFGS-B"))
fit1 <- with(crashes, optim(c(0, 1), function(par) nll(par, dens = dlnorm, y = y, w = w, p = prob)))
fit2 <- with(crashes, optim(c(0, 1), function(par) nll(par, dens = dlnorm, y = y, w = psw, p = prob)))

with(df %>% filter(y > 0), nll(fit0a$par, dens = dlnorm, y = y, w = 1, p = prob))
with(df %>% filter(y > 0), nll(fit1$par, dens = dlnorm, y = y, w = 1, p = prob))
with(df %>% filter(y > 0), nll(fit2$par, dens = dlnorm, y = y, w = 1, p = prob))

ggplot() + 
  geom_histogram(data = df %>% filter(y > 0), aes(x = y, y = ..density.., weight = prob), binwidth = 1, colour = "white") + 
  # geom_density(data = df %>% filter(y > 0), aes(x = y, weight = prob, colour = "KDE", linetype = "KDE"), lwd = 1) + 
  stat_function(fun = dlnorm, args = list(fit0a$par[1], fit0a$par[2]), aes(colour = "All data", linetype = "All data")) + 
  # stat_function(fun = dgamma, args = list(fit0b$par[1], fit0b$par[2]), aes(colour = "Gamma", linetype = "Gamma"), lwd = 1) + 
  stat_function(fun = dlnorm, args = list(fit1$par[1], fit1$par[2]), aes(colour = "Subsample", linetype = "Subsample")) + 
  # stat_function(fun = dlnorm, args = list(fit2$par[1], fit2$par[2]), aes(colour = "PS", linetype = "PS"), lwd = 1) + 
  scale_colour_brewer(palette = "Dark2", breaks = c("KDE", "All data", "Gamma", "Subsample", "PS")) + 
  scale_linetype(breaks = c("KDE", "All data", "Gamma", "Subsample", "PS")) + 
  labs(x = "Impact speed (km/h)",
       y = "Relative frequency",
       colour = NULL,
       linetype = NULL,) + 
  theme(legend.position = c(0.8, 0.8),
        legend.direction = "vertical")
ggsave("OutcomeDist_Combined.png", dpi = 1000, width = 90, height = 70, unit = "mm")


kl1 <- with(df %>% filter(y > 0), kl(y, fit0a$par, fit1$par, dlnorm))
kl2 <- with(df %>% filter(y > 0), kl(y, fit0a$par, fit2$par, dlnorm))

print(kl1)
print(kl2)
