
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

set.seed(123456)
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





# Derive datasets ---------------------------------------------------------


# Glance distribution.
glance <- BaselineGlanceDist %>% 
  mutate(relfreq = count / sum(count), 
         fitted_prob = dlnorm(t, meanlog  = fit3$estimate["meanlog"], sdlog = fit3$estimate["sdlog"]),
         fitted_prob = (t == 0) * zeroProb + (t > 0) * (1 - zeroProb) * fitted_prob / sum(fitted_prob)) # Normalise.


# Overshot distribution
overshot <- glance %>%
  mutate(prob = rev(cumsum(rev(fitted_prob / 1:length(fitted_prob)))),
         prob = prob / sum(prob), # Normalise.
         prob_old = rev(cumsum(rev(count / 1:length(count)))),
         prob_old = prob_old / sum(prob_old)) %>%
  dplyr::select(-count, -relfreq, -fitted_prob) 


# Bivariate undershot/overshot distribution.
biv <- crossing(before = seq(0, 6.1, 0.1), after = seq(0, 6.1, 0.1), glance) %>% 
  filter(abs(t - (before + after)) < 0.01) %>% 
  group_by(t) %>% 
  mutate(prob = fitted_prob / length(fitted_prob),
         prob_old = relfreq / length(relfreq)) %>% 
  ungroup() %>% 
  arrange(after, before) %>% 
  dplyr::select(before, after, t, prob, prob_old) 


# Check glance distribution
biv %>% 
  group_by(t) %>% 
  summarise(rel_freq = sum(prob_old),
            fitted_prob = sum(prob))
glance


# Check overshot distribution. 
biv %>% 
  group_by(after) %>% 
  summarise(prob = sum(prob),
            prob_old = sum(prob_old)) %>% 
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



# Plots -------------------------------------------------------------------


ggplot(data = glance) + 
  geom_col(aes(x = t, y = relfreq, fill = "Observed")) + 
  geom_line(aes(x = t, y = fitted_prob, colour = "Fitted")) + 
  scale_colour_manual(values = "firebrick") + 
  scale_fill_manual(values = "grey40") + 
  facet_zoom(ylim = c(0, 0.025), zoom.data = ifelse(fitted_prob <= 0.025, NA, FALSE), zoom.size = 1) +  
  labs(x = "EOFF (s)",
       y = "Relative frequency",
       colour = NULL,
       fill = NULL) + 
  theme(legend.position = c(0.8, 0.8),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.spacing =  unit(0.2, "cm"))
ggsave("BaselineGlanceDist.png", dpi = 1000, width = 180, height = 70, unit = "mm")



ggplot() +
  geom_qq_line(aes(sample = log(x)), colour = "firebrick") +
  geom_qq(aes(sample = log(x)), size = 0.1) + 
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) + 
  labs(x = "Normal distribution quantiles",
       y = "Sample quantiles (log-scale)")
ggsave("CheckDist.png", dpi = 1000, width = 90, height = 90, unit = "mm")



ggplot(data = overshot) + 
  geom_col(aes(x = t, y = prob_old, fill = "Fitted"), alpha = 0.8) + 
  geom_col(aes(x = t, y = prob, fill = "Empirical"), alpha = 0.8) + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  facet_zoom(ylim = c(0, 0.03), zoom.data = ifelse(prob_old <= 0.03, NA, FALSE), zoom.size = 1) + 
  labs(x = "Overshot time (s)",
       y = "Relative frequency",
       fill = "Glance distribution") + 
  theme(legend.position = c(0.8, 0.8),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1, "cm"))
ggsave("OverShotDist.png", dpi = 1000, width = 180, height = 70, unit = "mm")



ggplot() + 
  geom_rect(data = biv, aes(xmin = before - 0.05, xmax = before + 0.05, ymin = after - 0.05, ymax = after + 0.05, fill = prob), colour = "black", size = 0.01) + 
  geom_text(aes(x = 3, y = 6.1, label = "Probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
  coord_equal() + 
  scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "EOFF before tauinv = 0.2 (s)",
       fill = NULL) + 
  theme(legend.direction = "horizontal", 
        legend.justification = 1,
        legend.position = c(1, 0.9), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.8, "cm"))
ggsave("BivDist.png", dpi = 1000, width = 90, height = 90, unit = "mm")



ggplot() + 
  geom_rect(data = biv, aes(xmin = before - 0.05, xmax = before + 0.05, ymin = after - 0.05, ymax = after + 0.05, fill = prob_old), colour = "black", size = 0.01) + 
  geom_text(aes(x = 3, y = 6.1, label = "Probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
  coord_equal() + 
  scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1), breaks = 10^(-seq(1, 5, 2))) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "EOFF before tauinv = 0.2 (s)",
       fill = NULL) + 
  theme(legend.direction = "horizontal", 
        legend.justification = 1,
        legend.position = c(1, 0.9), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.8, "cm"))
ggsave("BivDistOld.png", dpi = 1000, width = 90, height = 90, unit = "mm")




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
  
  y <- (z3 
- zmin) / max(z3 - zmin) * ymax

  return(y)
}




active_learning <- function(sim = 1, niter = 100, seed = NULL, saveplot = 1:10,
                            beta = runif(2, 0, 1) * c(1, 0.1) + c(0.5, 0), sigma = runif(1, 0, 1), ymax = 30, q = c(runif(1, 0, 0.5), runif(1, 0.9, 1))) {
# sim = 1; niter = 1; seed = 1; saveplot = 1:100; beta = beta = runif(2, 0, 1) * c(1, 0.1) + c(0.5, 0); 
# sigma = runif(1, 0, 1); ymax = 30; q = c(runif(1, 0, 0.5), runif(1, 0.9, 1));
  
  if ( !is.null(seed) ) {
    set.seed(seed)
  }
  
  df <- biv %>%
    dplyr::select(-prob_old) %>% 
    mutate(y = simulateY(t1 = after, t2 = before, beta = beta, sigma = sigma, q = q))
  rm(beta, sigma)

  labelled <- df %>% 
    mutate(selected = 1) %>%
    filter((after == 0 & before == 0) | (after == 6.1 & before == 0)) %>% 
    mutate(w = 1,
           selected = 1)
  
  unlabelled <- df %>% 
    filter(!((after == 0 & before == 0) | (after == 6.1 & before == 0))) %>% 
    mutate(w = 0,
           selected = 0)
  
  ymax <- max(labelled$y)
  ymin <- min(labelled$y)
  
  
  # For illustrative purposes only: theoretically optimal sampling scheme.
  pi0 <- with(df, ifelse(y > 0 & y < ymax, prob * y, rep(0, length(y)))) 
  pi0 <- pi0 / sum(pi0)
  ix0 <- as.numeric(rmultinom(1, size = niter, pi0))
  
  
  # Figures.
  f0a <- biv %>% 
    filter(after > 0 | before > 0) %>% 
    mutate(prob = prob / sum(prob)) %>% 
    ggplot() + 
    geom_rect(aes(xmin = before - 0.05, xmax = before + 0.05, ymin = after - 0.05, ymax = after + 0.05, fill = prob)) + 
    geom_segment(data = pctls, aes(x = -0.05, xend = t + 0.05, y = t + 0.05, yend = 0 - 0.05), colour = "black") + 
    geom_text(data = pctls, aes(x = t, y = 0 - 0.1, label = paste0(100 * pctl, "th")), colour = "black", hjust = 0, vjust = 1, size = ptsize / ggplot2::.pt) + 
    geom_text(data = pctls %>% filter(pctl == max(pctl)), aes(x = t + 0.7, y = 0 - 0.1, label = "pctl"), colour = "black", hjust = 0, vjust = 1, size = ptsize / ggplot2::.pt) + 
    geom_text(data = data.frame(x = 2.975, y = 6), aes(x = x, y = y, label = "Probability of (x, y) pair"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
    coord_equal() + 
    scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
    labs(x = "EOFF after tauinv = 0.2 (s)",
         y = "EOFF before tauinv = 0.2 (s)",
         fill = NULL) + 
    theme(legend.direction = "horizontal", 
          legend.justification = 1,
          legend.position = c(1, 0.875), 
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.8, "cm"))


  f0b <- ggplot() +
    geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
    geom_text(aes(x = 3.85, y = 6, label = "Impact speed (km/h)"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
    scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
    coord_equal() + 
    labs(x = "EOFF after tauinv = 0.2 (s)",
         y = "EOFF before tauinv = 0.2 (s)",
         fill = NULL) + 
    theme(legend.direction = "horizontal", 
          legend.justification = 1,
          legend.position = c(1, 0.875), 
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.6, "cm"))

  
  f0c <- ggplot() + 
    geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi0)) +
    geom_rect(data = df %>% filter(y >= ymax), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") +
    geom_point(data = df[ix0 > 0, ], aes(x = after, y = before, size = ix0[ix0 > 0]), colour = "firebrick", show.legend = FALSE) +
    geom_text(aes(x = 2.975, y = 6, label = "Optimal sampling probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
    coord_equal() +
    scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
    scale_size_continuous(range = c(0.1, 1)) +
    labs(x = "EOFF after tauinv = 0.2 (s)",
         y = "EOFF before tauinv = 0.2 (s)",
         fill = NULL) +
    theme(legend.direction = "horizontal",
          legend.justification = 1,
          legend.position = c(1, 0.875),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.8, "cm"))
  
  
  f0d <- df %>% 
    filter(y > 0) %>% 
    mutate(y = floor(y)) %>% 
    group_by(y) %>% 
    summarise(wt = sum(prob)) %>% 
    ungroup() %>% 
    mutate(prob = wt / sum(wt)) %>% 
    ggplot() + 
    geom_col(aes(x = y, y = prob), width = 0.9) + 
    labs(x = "Impact speed (km/h)",
         y = "Relative frequency")
  
  title <- ggplot() + 
    labs(title = sprintf("Scenario information, Simulation %d", sim)) + 
    theme_minimal() + 
    theme(plot.title = element_text(size = 1.5 * ptsize, colour = "black", family = "sans", face = "plain", hjust = 0.5))
  grid1 <- plot_grid(f0a, f0b, f0c, f0d, ncol = 2)
  grid2 <- plot_grid(title, grid1, ncol = 1, rel_heights = c(0.05, 1))
  ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/fig%d_0.png", sim), grid2, dpi = 300, width = 180, height = 190, unit = "mm")  

  
  # Iterate
  fcount <- 1
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
        w <- train[, "w"]
        # mod <- cv.glmnet(X, Y, alpha = 0, grouped = FALSE, nfolds = ifelse(nrow(X) < 50, nrow(X), 10))
        # beta <- coef(mod, s = "lambda.min")
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
               yhat = pmax(yhat1, yhat2))

      
      for ( j in 1:nrow(unlabelled) ) {
        unlabelled$prob_positive[j] <- ifelse(any(unlabelled$after[j] >= cornersCrash$after & 
                                                    unlabelled$before[j] >= cornersCrash$before), 
                                              1, unlabelled$prob_positive[j])
      }
 
    } else {

      print("This has not been implemented yet!")
      
    }
    
    if ( i > 1) {
      if ( newx$y == 0 ) {
        
        certaintySelections <- unlabelled %>% 
          filter(before <= newx$before & after <= newx$after) %>%
          add_row(labelled) %>% 
          mutate(pi = 1) %>%
          dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2)
        
        unlabelled %<>%
          filter(before > newx$before | after > newx$after)
        
      } else if ( newx$y == ymax ) {
        
        certaintySelections <- unlabelled %>%
          filter(before >= newx$before & after >= newx$after) %>%
          add_row(labelled) %>% 
          mutate(pi = 1) %>%
          dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2)
        
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
    
    pi <- with(unlabelled, prob * sqrt(prob_positive) * yhat)
    pi <- pi / sum(pi)
    
    ix <- which(rmultinom(1, size = 1, pi) == 1)
    
    newx <- unlabelled %>%
      filter(row_number() == ix) %>% 
      mutate(selected = 1, 
             pi = pi[ix]) %>% 
      dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2)
    

    f4 <- ggplot() + 
      geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) + 
      geom_rect(data = certaintySelections, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
      geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
      geom_rect(data = certaintySelections %>% add_row(labelled) %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
      geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
      geom_rect(data = newx, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = ifelse(newx$y > 0, "firebrick", "grey40")) + 
      geom_text(aes(x = 2.65, y = 6, label = "Sampling probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
      coord_equal() + 
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = NULL) + 
      theme(legend.direction = "horizontal", 
            legend.justification = 1,
            legend.position = c(0.95, 0.88), 
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.8, "cm")) 
    
    
    # Figures.
    if ( i %in% saveplot ) {
      
      f1 <- ggplot() + 
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive)) + 
        geom_rect(data = certaintySelections %>% filter(y == 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
        geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
        geom_rect(data = certaintySelections %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
        geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
        geom_point(data = cornersCrash, aes(x = after, y = before), pch = "x", colour = "white") + 
        geom_point(data = corners0, aes(x = after, y = before), pch = "x", colour = "black") + 
        geom_text(aes(x = 2.65, y = 6, label = "Anticipated collision probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
        coord_equal() + 
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
        expand_limits(fill = 1) + 
        labs(x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = NULL) + 
        theme(legend.direction = "horizontal", 
              legend.justification = 1,
              legend.position = c(0.95, 0.88), 
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.8, "cm"))
      
      
      f2a <- ggplot() + 
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat1)) + 
        geom_rect(data = certaintySelections, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
        geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
        geom_rect(data = certaintySelections %>% add_row(labelled) %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
        geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
        geom_point(data = selectedCrash %>% filter(y < ymax) %>% add_row(cornersCrash), aes(x = after, y = before), pch = "x", colour = "white") + 
        geom_point(data = corners0, aes(x = after, y = before), pch = "x", colour = "black") +       
        geom_text(aes(x = 2.15, y = 6, label = "Anticipated impact speed (pred 1) (km/h)"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
        coord_equal() + 
        scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1), breaks = seq(0, 30, 10)) +
        expand_limits(fill = c(0, 30)) + 
        labs(x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = NULL) + 
        theme(legend.direction = "horizontal", 
              legend.justification = 1,
              legend.position = c(1, 0.88), 
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.8, "cm"))
      
      
      f2b <- ggplot() + 
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat2)) + 
        geom_rect(data = certaintySelections, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
        geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
        geom_rect(data = certaintySelections %>% add_row(labelled) %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
        geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
        geom_point(data = cornersMax, aes(x = after, y = before), pch = "x", colour = "white") +       
        geom_point(data = corners0, aes(x = after, y = before), pch = "x", colour = "black") +       
        geom_text(aes(x = 2.15, y = 6, label = "Anticipated impact speed (pred 2) (km/h)"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
        coord_equal() + 
        scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1), breaks = seq(0, 30, 10)) +
        expand_limits(fill = c(0, 30)) + 
        labs(x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = NULL) + 
        theme(legend.direction = "horizontal", 
              legend.justification = 1,
              legend.position = c(1, 0.88), 
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.8, "cm"))
      
      
      f2c <- ggplot() + 
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat)) + 
        geom_rect(data = certaintySelections, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
        geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
        geom_rect(data = certaintySelections %>% add_row(labelled) %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
        geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
        geom_text(aes(x = 1.85, y = 6, label = "Anticipated impact speed (max pred) (km/h)"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
        coord_equal() + 
        scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1), breaks = seq(0, 30, 10)) +
        expand_limits(fill = c(0, 30)) + 
        labs(x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = NULL) + 
        theme(legend.direction = "horizontal", 
              legend.justification = 1,
              legend.position = c(1, 0.88), 
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.8, "cm"))
      
      
      f3 <- ggplot() + 
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) + 
        geom_rect(data = certaintySelections, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
        geom_rect(data = certaintySelections %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
        geom_rect(data = certaintySelections %>% add_row(labelled) %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
        geom_rect(data = certaintySelections %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
        geom_text(aes(x = 2.65, y = 6, label = "Sampling probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
        coord_equal() + 
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
        labs(x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = NULL) + 
        theme(legend.direction = "horizontal", 
              legend.justification = 1,
              legend.position = c(0.95, 0.88), 
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.8, "cm")) 
      
      
      title <- ggplot() + 
        labs(title = sprintf("Simulation %d, Iteration %d", sim, i)) + 
        theme_minimal() + 
        theme(plot.title = element_text(size = 1.5 * ptsize, colour = "black", family = "sans", face = "plain", hjust = 0.5))
      
      grid1 <- plot_grid(f1, f2a, f2b, f2c, f3, f4, ncol = 3)
      grid2 <- plot_grid(title, grid1, ncol = 1, rel_heights = c(0.05, 1))
      ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/fig%d_%d.png", sim, fcount), grid2, dpi = 300, width = 270, height = 190, unit = "mm")
      fcount <- fcount + 1 
      
    }
    
    
    # Update labelled and unlabelled sets.
    labelled <- certaintySelections %>%
      add_row(newx) %>%
      mutate(w = w + (1 / pi - w) / i) %>%
      dplyr::select(-pi)

    unlabelled %<>%
      filter(row_number() != ix) %>%
      dplyr::select(-prob_positive, -yhat, -yhat1, -yhat2)

    
    # Plot current results every 10th iteration. 
    if ( mod(i, 10) == 0 ) {

      crashes <- labelled %>% filter(y  >0)
      mu0 <- with(df %>% filter(y >0), sum(y * prob) / sum(prob))
      muest <- with(crashes, sum(prob * y * w) / sum(prob * w))
      
      b <- boot(crashes, function(df, ix) with(df[ix, ], sum(prob * y * w) / sum(prob * w)), R = 1e3)
      ci <- boot.ci(b, type = "bca")
      
      f5 <- f0d +
        geom_segment(aes(x = mu0, xend = mu0, y = -0.01, yend = max(f0d$data$prob) + 0.05), colour = "firebrick") + 
        labs(title = sprintf("Mean = %.1f km/h", mu0))
      
      bars <- labelled %>% 
        filter(y > 0) %>% 
        mutate(y = floor(y / 2) * 2) %>% 
        group_by(y) %>% 
        summarise(w = sum(prob * w)) %>% 
        ungroup() %>% 
        mutate(prob = w / sum(w))
      
      f6 <- ggplot() + 
        geom_ribbon(data = data.frame(x = c(ci$bca[4], ci$bca[5])), aes(x = x, ymin = -0.01, ymax = max(bars$prob) + 0.05), fill = "steelblue", alpha = 0.5) +
        geom_col(data = bars, aes(x = y, y = prob), width = 1.8) + 
        geom_segment(aes(x = muest, xend = muest, y = -0.01, yend = max(bars$prob) + 0.05), colour = "firebrick") + 
        expand_limits(x = c(0, ymax)) + 
        labs(title = sprintf("Mean (SE) 95%% CI: %.1f (%.1f) %.1f-%.1f", muest, sd(b$t), ci$bca[4], ci$bca[5]),
             x = "Impact speed (km/h)",
             y = "Relative frequency")
      
      tit1 <- ggplot() + 
        labs(title = sprintf("Scenario information, Simulation %d", sim)) + 
        theme_minimal() + 
        theme(plot.title = element_text(size = 1.5 * ptsize, colour = "black", family = "sans", face = "plain", hjust = 0.5))
      
      tit2 <- ggplot() + 
        labs(title = sprintf("Result after %d iterations", i)) + 
        theme_minimal() + 
        theme(plot.title = element_text(size = 1.5 * ptsize, colour = "black", family = "sans", face = "plain", hjust = 0.5))
      
      grid1 <- plot_grid(f0c, f5, ncol = 2)
      grid2 <- plot_grid(f4, f6, ncol = 2)
      grid3 <- plot_grid(tit1, grid1, tit2, grid2, nrow = 4, rel_heights = c(0.1, 1, 0.1, 1))
      ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/fig%d_%d.png", sim, fcount), grid3, dpi = 300, width = 180, height = 200, unit = "mm")  
      fcount <- fcount + 1
    }
    
  }
  
}

active_learning(sim = 1,  niter = 100, beta = c(1,  1), sigma = 0.1, q = c(0.10, 0.95), seed = 1)
active_learning(sim = 2,  niter = 100, beta = c(5,  1), sigma = 0.1, q = c(0.10, 0.95), seed = 2)
active_learning(sim = 3,  niter = 100, beta = c(10, 1), sigma = 0.1, q = c(0.10, 0.95), seed = 3)

active_learning(sim = 4,  niter = 100, beta = c(1,  1), sigma = 0.1, q = c(0.25, 0.95), seed = 4)
active_learning(sim = 5,  niter = 100, beta = c(5,  1), sigma = 0.1, q = c(0.25, 0.95), seed = 5)
active_learning(sim = 6,  niter = 100, beta = c(10, 1), sigma = 0.1, q = c(0.25, 0.95), seed = 6)

active_learning(sim = 7,  niter = 100, beta = c(1,  1), sigma = 0.1, q = c(0.10, 0.75), seed = 7)
active_learning(sim = 8,  niter = 100, beta = c(5,  1), sigma = 0.1, q = c(0.10, 0.75), seed = 8)
active_learning(sim = 9,  niter = 100, beta = c(10, 1), sigma = 0.1, q = c(0.10, 0.75), seed = 9)

active_learning(sim = 10,  niter = 100, beta = c(1,  1), sigma = 1, q = c(0.10, 0.95), seed = 10)
active_learning(sim = 11,  niter = 100, beta = c(5,  1), sigma = 1, q = c(0.10, 0.95), seed = 11)
active_learning(sim = 12,  niter = 100, beta = c(10, 1), sigma = 1, q = c(0.10, 0.95), seed = 12)

active_learning(sim = 13,  niter = 100, beta = c(1,  1), sigma = 1, q = c(0.25, 0.95), seed = 13)
active_learning(sim = 14,  niter = 100, beta = c(5,  1), sigma = 1, q = c(0.25, 0.95), seed = 14)
active_learning(sim = 15,  niter = 100, beta = c(10, 1), sigma = 1, q = c(0.25, 0.95), seed = 15)

active_learning(sim = 16,  niter = 100, beta = c(1,  1), sigma = 1, q = c(0.10, 0.75), seed = 16)
active_learning(sim = 17,  niter = 100, beta = c(5,  1), sigma = 1, q = c(0.10, 0.75), seed = 17)
active_learning(sim = 18,  niter = 100, beta = c(10, 1), sigma = 1, q = c(0.10, 0.75), seed = 18)

# N <- 1890
# n <- 100
# niter <- 1e5
# y <- sort(exp(rnorm(N)))
# f1 <- 0.1
# f2 <- 0.1
# y[1:floor(N * f1)] <- 0
# y[ceiling(N * (1 - f2)):N] <- min(y[ceiling(N * (1 - f2)):N])
# hist(y)
# 
# pi <- n * y / sum(y)
# sel <- matrix(rbinom(N * niter, 1, pi), ncol = niter, byrow = FALSE)
# selprob <- apply(sel, 1, mean)
# plot(selprob, pi)




# Old ---------------------------------------------------------------------


# step <- biv %>%
#   filter(z3 <= 0) %>%
#   group_by(x) %>%
#   summarise(y = max(y))
# step <- rbind(step, c(max(step$x) + 0.1, -0.1))
# 
# step2 <- biv %>%
#   filter(z3 >= zmax) %>%
#   group_by(x) %>%
#   summarise(y = min(y))
# step2 <- rbind(c(min(step2$x), max(biv$y[z3 >= zmax]) + 0.1), 
#                step2,
#                c(6.1, 0))
# geom_step(data = step, aes(x = x - 0.05, y = y + 0.05), colour = "firebrick", lwd = 1) + 
#   geom_step(data = step2, aes(x = x - 0.05, y = y - 0.05), colour = "steelblue", lwd = 1)

# ggplot() + 
#   geom_rect(data = biv, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = z)) + 
#   geom_rect(data = biv %>% filter(z <= 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "white", alpha = 0.7) + 
#   geom_step(data = step, aes(x = x - 0.05, y = y + 0.05), colour = "firebrick", lwd = 1) + 
#   scale_fill_continuous(type = "viridis") + 
#   labs(x = NULL, 
#        y = NULL) + 
#   theme(legend.position = "none")
# 
# ggplot() + 
#   geom_rect(data = biv, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = z2)) + 
#   geom_rect(data = biv %>% filter(z2 <= 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "white", alpha = 0.7) +
#   geom_step(data = step, aes(x = x - 0.05, y = y + 0.05), colour = "firebrick", lwd = 1) + 
#   scale_fill_continuous(type = "viridis") + 
#   labs(x = NULL, 
#        y = NULL) + 
#   theme(legend.position = "none")
# 
# 
# biv %>%
#   group_by(x) %>% 
#   summarize(pmf = sum(pmf)) %>%
#   ggplot(aes(x = x, y = pmf)) + 
#   geom_col() + 
#   scale_colour_continuous(type = "viridis") + 
#   theme(legend.position = "none")
# 
# biv %>%
#   group_by(y) %>% 
#   summarize(pmf = sum(pmf)) %>%
#   ggplot(aes(x = y, y = pmf)) + 
#   geom_col() + 
#   scale_colour_continuous(type = "viridis") + 
#   theme(legend.position = "none")
# 
# 
# 
# ggplot() + 
#   geom_rect(data = biv, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = log(pi2))) + 
#   scale_fill_continuous(type = "viridis") + 
#   labs(title = "Optimal sampling scheme, boundary between collision/non-collisions known, outcome unkown",
#        x = NULL, 
#        y = NULL) + 
#   theme(legend.position = "none")
# 
# ggplot() +
#   geom_rect(data = biv, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = log(pi3))) +
#   scale_fill_continuous(type = "viridis") +
#   labs(x = NULL,
#        y = NULL) +
#   theme(legend.position = "none")
# 
# Calculate sampling schemes.
# optimisePi <- function(data, phase = 1) {
#   
#   if (phase == 1) {
#     
#   }
#   
#   X <- cbind(data$after, data$before)
#   for ( cutoff in unique(data$before)) {
#     X <- cbind(X, data$after * (data$before > cutoff))
#   }
#   
#   # Outcome model for 0 < y < ymax.
#   ymax <- max(data$y)
#   mod <- lm(y~after+before, data = data %>% filter(y > 0 & y < ymax))
#   yhat <- predict(mod, data)
#   s <- sigma(mod)
#   
#   # Binary prediction model for y>0 vs y = 0.
#   zeromod <- cv.glmnet(X, as.factor(data$y>0), family = "binomial")
#   prob_positive <- as.numeric(predict(zeromod, newx = X, s = "lambda.min", type = "response"))
#   # prob_positive <- pnorm(0, mean = yhat, sd = s, lower.tail = FALSE) # Wrong. 
#   
#   # Binary prediction model for y=ymax vs y<ymax. 
#   maxmod <- cv.glmnet(X, as.factor(data$y >= ymax), family = "binomial")
#   prob_max <- as.numeric(predict(maxmod, newx = X, s = "lambda.min", type = "response"))
#   # prob_max <- pnorm(ymax, mean = yhat, sd = s, lower.tail = FALSE) # Wrong.
#   
#   # Everything known.
#   pi1 <- with(data, ifelse(y > 0, prob * y, rep(0, length(yhat)))) 
#   pi1 <- pi1 / sum(pi1)
#   
#   # Everything known, max impact speed not sampled.
#   pi2 <- with(data, ifelse(y > 0 & y < ymax, pi1, rep(0, length(yhat)))) 
#   pi2 <- pi2 / sum(pi2)
#   
#   # Initial sample. 
#   b0 <- -log(99)
#   b1 <- 2 * log(99) / 6
#   p <- with(data, (1 + exp(-(b0 + b1 * after)))^(-1))
#   pi3 <- with(data, prob * sqrt(p) * (0.1 + after) * (before > 0 | after > 0))
#   # pi3 <- with(data, sqrt(prob) * (0.1 + after)  * (before > 0 | after > 0))
#   pi3 <- pi3 / sum(pi3)
#   
#   # General situation. 
#   pi4 <- with(data, prob * sqrt(prob_positive) * sqrt(prob_max * yhat^2 + (1 - prob_max) * (yhat^2 + s^2)))
#   pi4 <- pi4 / sum(pi4)
#   
#   # General situation, but zeroes and maximal impact speed not sampled.
#   pi5 <- with(data, ifelse(y > 0 & y < ymax, pi4, rep(0, length(yhat)))) 
#   pi5 <- pi5 / sum(pi5)
#   
#   return(data.frame(yhat, prob_positive, prob_max, pi1, pi2, pi3, pi4, pi5))
# }
# 
# 
# # Run simulations, generate plots.
# run_all <- function(n = 10, N = 100) {
#   
#   for ( i in 1:n ) {
#     
#     df <- biv %>%
#       mutate(y = simulateY(t1 = after, t2 = before)) 
#     
#     ymax <- max(df$y)
#     
#     df %<>% 
#       add_column(optimisePi(.))
#     
#     ix1 <- as.numeric(rmultinom(1, size = N, df$pi1))
#     ix2 <- as.numeric(rmultinom(1, size = N, df$pi2))
#     ix3 <- as.numeric(rmultinom(1, size = N * 0.1, df$pi3))
#     ix4 <- as.numeric(rmultinom(1, size = N * 0.2, df$pi4))
#     ix5 <- as.numeric(rmultinom(1, size = N * 0.7, df$pi5))
#     
#     fig1 <- ggplot(data = biv, aes(xmin = before - 0.05, xmax = before + 0.05, ymin = after - 0.05, ymax = after + 0.05, fill = prob)) + 
#       geom_rect(colour = "salmon", size = 0.01) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       labs(title = "Bivariate glance distribution",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig2 <- df %>% 
#       filter(y > 0) %>% 
#       mutate(y = floor(y)) %>% 
#       group_by(y) %>% 
#       summarise(wt = sum(prob)) %>% 
#       ungroup() %>% 
#       mutate(prob = wt / sum(wt)) %>% 
#       ggplot() + 
#       geom_col(aes(x = y, y = prob)) + 
#       labs(title = "Outcome distribution",
#            x = "Impact speed (km/h)",
#            y = "Relative frequency")
#     
#     
#     fig3 <- ggplot() +
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
#       geom_rect(data = df %>% filter(y >= ymax), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = NA, colour = "salmon", size = 0.01) +
#       scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
#       coord_equal() + 
#       labs(title = "Outcome distribution", 
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Impact speed (km/h) ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.6, "cm"))
#     
#     
#     fig4 <- ggplot(data = df) + 
#       geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive), colour = "salmon", size = 0.01) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
#       labs(title = "Estimated collision probability",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig5 <- ggplot(data = df) + 
#       geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_max), colour = "salmon", size = 0.01) +
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
#       labs(title = "Estimated probability of maximum impact speed",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig6 <- ggplot() + 
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi1)) + 
#       geom_rect(data = df %>% filter(y >= ymax), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = NA, colour = "salmon", size = 0.01) +
#       geom_point(data = df[ix1 > 0, ], aes(x = after, y = before, size = ix1[ix1 > 0]), colour = "firebrick", show.legend = FALSE) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       scale_size_continuous(range = c(0.1, 1)) + 
#       labs(title = "Oracle sampling scheme, unrestricted",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig7 <- ggplot() + 
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi2)) + 
#       geom_point(data = df[ix2 > 0, ], aes(x = after, y = before, size = ix2[ix2 > 0]), colour = "firebrick", show.legend = FALSE) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       scale_size_continuous(range = c(0.1, 1)) + 
#       labs(title = "Oracle sampling scheme, restricted",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig8 <- ggplot() + 
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi3)) + 
#       geom_point(data = df[ix3 > 0, ], aes(x = after, y = before, size = ix3[ix3 > 0]), colour = "firebrick", show.legend = FALSE) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       scale_size_continuous(range = c(0.1, 1)) + 
#       labs(title = "Sampling scheme, stage 1",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig9 <- ggplot() + 
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi4)) + 
#       geom_point(data = df[ix4 > 0, ], aes(x = after, y = before, size = ix4[ix4 > 0]), colour = "firebrick", show.legend = FALSE) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       scale_size_continuous(range = c(0.1, 1)) + 
#       labs(title = "Sampling scheme, stage 2",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     
#     fig10 <- ggplot() + 
#       geom_rect(data = df, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi5)) + 
#       geom_point(data = df[ix5 > 0, ], aes(x = after, y = before, size = ix5[ix5 > 0]), colour = "firebrick", show.legend = FALSE) + 
#       coord_equal() + 
#       scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#       scale_size_continuous(range = c(0.1, 1)) + 
#       labs(title = "Sampling scheme, stage 3",
#            x = "EOFF after tauinv = 0.2 (s)",
#            y = "EOFF before tauinv = 0.2 (s)",
#            fill = "Probability ") + 
#       theme(legend.direction = "horizontal", 
#             legend.justification = 1,
#             legend.position = c(1, 0.9), 
#             legend.key.height = unit(0.3, "cm"),
#             legend.key.width = unit(0.8, "cm"))
#     
#     combine <- plot_grid(fig1, fig3, fig2, fig4, fig5, fig6, fig7, fig8, fig9, fig10, ncol = 5, align = "vh")
#     ggsave(sprintf("Sim%d.png", i), dpi = 300, width = 450, height = 180, unit = "mm")
#     
#   }
# }
# 
# run_all(n = 20)
# 
#       
# a0 <- 0
# b0 <- 0
# 
# x3 <- labelled %>% filter(labelled$y == ymax) %>% filter(row_number() == which.min(after))
# yhatmax <- with(x3, max(after - a0, 0) + max(before - b0, 0) / 5)
# 
# unlabelled %<>%
#   mutate(prob_positive = 1,
#          yhat = after + before / 5,
#          yhat = yhat / max(yhat) * ymax)
#     
# 
# x2 <- labelled %>% filter(y == 0) %>% filter(row_number() == which.max(after)) %>% filter(row_number() == which.max(before))
# x1 <- labelled %>% filter(y > 0 & after > x2$after) %>% filter(row_number() == which.min(after)) %>% filter(row_number() == which.min(before))
# a0 <- x2$after
# b0 <- x2$before
# 
# A <- matrix(nrow = 2, byrow = FALSE, c(1, 1, x1$after + 0.2 * x1$before, x2$after + 0.2 * x2$before))
# 
# b <- c(log(0.99) - log(0.01), log(0.10) - log(0.90))
# beta <- solve(A, b)
# beta <- c(beta, beta[2] / 5)
# 
# x3 <- labelled %>% filter(labelled$y == ymax) %>% filter(row_number() == which.min(after))
# 
# unlabelled %<>%
#   mutate(prob_positive = (1 + exp(-(beta[1] + beta[2] * after + beta[3] * before)))^(-1),
#          yhat = 30 / yhatmax * pmin(pmax(after - a0, 0) + pmax(before - b0, 0) / 5, yhatmax))
# f5 <- ggplot() + 
#   geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) + 
#   geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
#   geom_rect(data = labelled %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
#   geom_rect(data = labelled %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
#   geom_rect(data = labelled %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
#   geom_text(aes(x = 2.75, y = 6, label = "Sampling probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
#   coord_equal() + 
#   scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#   labs(x = "EOFF after tauinv = 0.2 (s)",
#        y = "EOFF before tauinv = 0.2 (s)",
#        fill = NULL) + 
#   theme(legend.direction = "horizontal", 
#         legend.justification = 1,
#         legend.position = c(1, 0.88), 
#         legend.key.height = unit(0.3, "cm"),
#         legend.key.width = unit(0.8, "cm")) 

# if ( i %in% saveplot ) {
#   # ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/Sim%d/fig%d.png", sim, fcount), f5, dpi = 300, width = 90, height = 90, unit = "mm")
#   
#   grid1 <- plot_grid(f3, f4, f5, ncol = 3)
#   grid2 <- plot_grid(title, grid1, ncol = 1, rel_heights = c(0.1, 1))
#   ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/fig%d_%d.png", sim, fcount), grid2, dpi = 300, width = 270, height = 100, unit = "mm")
#   fcount <- fcount + 1 
#   
# }
#
#

# f5 <- ggplot() + 
#   geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) + 
#   geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40") + 
#   geom_rect(data = labelled %>% filter(y > 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "tomato") + 
#   geom_rect(data = labelled %>% filter(y > 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") + 
#   geom_rect(data = labelled %>% filter(y == 0 & selected == 1), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
#   geom_text(aes(x = 2.75, y = 6, label = "Sampling probability"), hjust = 0, vjust = -1, size = ptsize / ggplot2::.pt) + 
#   coord_equal() + 
#   scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) + 
#   labs(title = sprintf("Iteration %d", i),
#        x = "EOFF after tauinv = 0.2 (s)",
#        y = "EOFF before tauinv = 0.2 (s)",
#        fill = NULL) + 
#   theme(legend.direction = "horizontal", 
#         legend.justification = 1,
#         legend.position = c(1, 0.88), 
#         legend.key.height = unit(0.3, "cm"),
#         legend.key.width = unit(0.8, "cm")) 
# 
# if ( i %in% saveplot ) {
#   
#   # ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/Sim%d/fig%d.png", sim, fcount), f5, dpi = 300, width = 90, height = 90, unit = "mm")
#   grid1 <- plot_grid(f3, f4, f5, ncol = 3)
#   grid2 <- plot_grid(title, grid1, ncol = 1, rel_heights = c(0.1, 1))
#   ggsave(sprintf("C:/Users/imbergh/Desktop/Glance/fig%d_%d.png", sim, fcount), grid2, dpi = 300, width = 270, height = 100, unit = "mm")
#   fcount <- fcount + 1 
#   
# }