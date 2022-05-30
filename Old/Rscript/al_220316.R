
# Init --------------------------------------------------------------------

rm(list = ls())

library("cowplot")
library("fitdistrplus")
library("glmnet")
library("magrittr")
library("randomForest")
library("readxl")
library("tidyverse")


# set.seed(123456)
ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "sans"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             legend.key.width = unit(1.25, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin = ggplot2::margin(t = -0.25, unit = 'cm'),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = ptsize, colour = "black", family = "sans"),
             legend.title = element_text(size = ptsize, colour = "black", family = "sans"),
             text = element_text(size = ptsize, colour = "black", family = "sans"),
             plot.title = element_text(size = ptsize, colour = "black", family = "sans", face = "plain", hjust = 0),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             strip.background.x = element_blank())

setwd("C:/Users/imbergh/Box Sync/Forskning/VirtSim/Rscript")


# Import ------------------------------------------------------------------


BaselineGlanceDist <- read_csv("./../Data/BaselineGlanceDist.csv", col_names = FALSE) %>% 
  dplyr::rename(t = X1, count = X2)


# Derive datasets ---------------------------------------------------------

x <- with(BaselineGlanceDist %>% filter(t > 0), rep(t, count))
fit <- fitdist(x, "lnorm")

# Glance distribution.
glance <- BaselineGlanceDist %>% 
  mutate(relfreq = count / sum(count), 
         fitted_prob = dlnorm(t, meanlog  = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]),
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
simulate_y <- function(t1, t2, beta = runif(4) * c(1, 0.1, 10, 1), sigma = runif(1, 0, 2), ymin = rbinom(1, 1, 0.2) * runif(1, 0, 5), ymax = runif(1, 20, 50), q = c(runif(1, 0, 0.25), runif(1, 0.9, 1))) {
 
  z <- as.numeric(cbind(t1, t2) %*% beta[1:2] + cbind(log(0.1 + t1), log(0.1 + t2)) %*% beta[3:4]) + rnorm(length(t1), sd = sigma)
  zmin <- quantile(z, min(q))
  zmax <- quantile(z, max(q))
    
  if ( ymin > 0 ) {
     ix <- which(t1 == min(t1) & t2 == min(t2)) 
     zmin <- z[ix]
     z2 <- z
  } else {
    min_ix <- which(z <= zmin)
    z2 <- z
    z2[z <= zmin] <- zmin
    for (i in min_ix) {
      z2[which(t2 <= t2[i] & t1 <= t1[i])] <- zmin
    }
  }
  
  max_ix <- which(z2 >= zmax)
  z3 <- z2
  z3[2 >= zmax] <- zmax
  for (i in max_ix) {
    z3[which(t2 >= t2[i] & t1 >= t1[i])] <- zmax
  }
  z3[length(z3)] <- zmax

  y <- ymin + (z3 - zmin) / max(z3 - zmin) * (ymax - ymin)
  
  return(y)
}


# Simulate sample of cases.
simulate_sample <- function(ncases = 10) {
  
  df <- NULL
  for ( i in 1:ncases ) {
    
    dfi <- biv %>%
      mutate(ID = i, 
             y = simulate_y(t1 = after, t2 = before))
    
    df <- df %>% 
      rbind(dfi)
    
  }
  
  return(df)
  
}


# Get outcome (impact speed) for case ID with given glance time before and after tauinv.
get_y <- function(ID, before, after) {
  y <- as.numeric(df_with_y$y[df_with_y$ID == ID & df_with_y$before == before & df_with_y$after == after])
}
get_y <- Vectorize(get_y, c("ID", "before", "after"))


# Weighted negative log-likelihood of two-parameter distribution with density dens.
wnll <- function(par, y, w, p, dens) {
  - sum(w * p * dens(y, par[1], par[2], log = TRUE))
}


# Update collision probability and impact speed predictions.
update_predictions <- function(labelled, unlabelled, plot = FALSE, fignum = 1) {
  
  defaultW <- getOption("warn")

  ymin <- min(labelled$y)
  ymax <- max(labelled$y)
  

  # Find all collision corner cases.
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
  
 
  # Prepare data for model fitting using fractional polynomials.
  form <- ~ -1 + I((0.1 + before)^(-3)) + I((0.1 + before)^(-2)) + 
    I((0.1 + before)^(-1)) + I((0.1 + before)^(-0.5)) +
    log(0.1 + before) + before + I(before^0.5) + I(before^2) + I(before^3) + 
    I((0.1 + after)^(-3)) + I((0.1 + after)^(-2)) + 
    I((0.1 + after)^(-1)) + I((0.1 + after)^(-0.5)) +
    log(0.1 + after) + after + I(after^0.5) + I(after^2) + I(after^3)
  X <- model.matrix(form, data = labelled)
  newX <- model.matrix(form, data = unlabelled)
  ntrain <- nrow(X)
  ix <- sample(1:ntrain) # Scramble training data.
  y <- labelled$y[ix]
  X <- X[ix, ]
 
  
  # Estimate impact speed. --
  options(warn = -1)
  ix <- which(labelled$y > ymin & labelled$y < ymax)
  
  # Random forest.
  if ( length(ix) > 1 ) {
    rf <- randomForest(y ~ after + before, data = labelled[ix, ])
    yhat1 <- predict(rf, unlabelled)
    yhat1[yhat1 <= 0] <- max(min(yhat1[yhat1 > 0]), ymin)
    yhat1[yhat1 > ymax] <- ymax
  } else {
    yhat1 <- rep((ymin + ymax) / 2, nrow(unlabelled))
  }
  
  # Fractional polynomial LASSO regression.
  if ( length(ix) > 2 ) {
    ix <- which(y > 0 & y < ymax)
    
    lasso <- cv.glmnet(X[ix, ], y[ix], grouped = FALSE)
    yhat2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"), 
                        nfolds = min(length(ix), 10))
    yhat2[yhat2 <= 0] <- max(min(yhat2[yhat2 > 0]), ymin)
    yhat2[yhat2 > ymax] <- ymax

  } else {
    yhat2 <- yhat1
  }
    
  
  # Estimate collision probability. --
  
  # Random forest.
  if ( ymin == 0 ) {
    rf <- randomForest(as.factor(y > 0) ~ after + before, data = labelled)
    ppos1 <- predict(rf, unlabelled, type = "prob")[, 2]
    ppos1[ppos1 <= 0] <- min(ppos1[ppos1 > 0])
  } else {
    ppos1 <- 1
  }
  
  # Fractional polynomial LASSO logistic regression.
  if ( sum(y == 0) > 2 & sum(y > 0) > 2 ) {
    lasso <- cv.glmnet(X, as.factor(y > 0), family = "binomial", grouped = FALSE)
    ppos2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
    ppos2[ppos2 <= 0] <- min(ppos2[ppos2 > 0])
  } else {
    ppos2 <- ppos1
  }
  
  
  # Estimate max impact speed collision probability. --

  # Random forest.
  rf <- randomForest(as.factor(y == ymax) ~ after + before, data = labelled)
  pmax1 <- predict(rf, unlabelled, type = "prob")[, 2]
  pmax1[pmax1 <= 0] <- min(pmax1[pmax1 > 0])
  
  # Fractional polynomial LASSO logistic regression.
  if ( sum(y == ymax) > 2 & sum(y == ymax) > 2 ) {
    lasso <- cv.glmnet(X, as.factor(y == ymax), family = "binomial", grouped = FALSE)
    pmax2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
    pmax2[pmax2 <= 0] <- min(pmax2[pmax2 > 0])
  } else {
    pmax2 <- pmax1
  }
  options(warn = defaultW)
  

  # Combine and add predictions to unlabelled dataset.
  ppos <- (ppos1 + ppos2) / 2
  pmax <- (pmax1 + pmax2) / 2
  yhat <- (yhat1 + yhat2) / 2
  ypred <- ppos * yhat + pmax * (ymax - yhat)
  ypred[ypred <= 0] <- min(ypred[ypred > 0]) # Zero impact speed not allowed.
  
  unlabelled %<>% 
    dplyr::select(-prob_positive, -yhat) %>% 
    mutate(prob_positive = ppos,
           yhat = ypred)
  
  # Find certainty crashes.
  for ( j in 1:nrow(unlabelled) ) {
    unlabelled$prob_positive[j] <- ifelse(any(unlabelled$after[j] >= cornersCrash$after & 
                                                     unlabelled$before[j] >= cornersCrash$before), 
                                               1, unlabelled$prob_positive[j])
  }
  

  if ( plot ) {
    options(warn = -1)
    
    plt1 <- ggplot(data = labelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
      scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
      coord_equal() +
      expand_limits(fill = c(0, 30)) +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Observed impact speed (km/h)") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))
    
    plt2 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = ppos1)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Estimated collision probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))  
    
    plt3 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = ppos2)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Estimated collision probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))  
   
    plt4 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Estimated collision probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm")) 
    
    
    plt5 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pmax1)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Max impact speed probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))  
    
    plt6 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pmax2)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Max impact speed probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))  
    
    plt7 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pmax)) +
      scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
      coord_equal() +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Max impact speed probability") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm")) 
    
    plt8 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat1)) +
      geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
      scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
      coord_equal() +
      expand_limits(fill = c(0, ymax)) +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Predicted impact speed (km/h)") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))
    
    plt9 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat2)) +
      geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
      scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
      coord_equal() +
      expand_limits(fill = c(0, ymax)) +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Predicted impact speed (km/h)") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))
    
    plt10 <- ggplot(data = unlabelled) +
      geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat)) +
      geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
      scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
      coord_equal() +
      expand_limits(fill = c(0, ymax)) +
      labs(x = "EOFF after tauinv = 0.2 (s)",
           y = "EOFF before tauinv = 0.2 (s)",
           fill = "Predicted impact speed (km/h)") +
      theme(legend.direction = "horizontal",
            legend.justification = 0.5,
            legend.position = "bottom",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.6, "cm"))
    
    
    f1 <- plot_grid(plt2, plt3, plt4, ncol = 3)
    f2 <- plot_grid(plt5, plt6, plt7, ncol = 3)
    f3 <- plot_grid(plt8, plt9, plt10, ncol = 3)
    ggsave(sprintf("fig%d_%d.png", fignum, 1), plt1, dpi = 300, width = 90, height = 90, unit = "mm")
    ggsave(sprintf("fig%d_%d.png", fignum, 2), f1, dpi = 300, width = 270, height = 90, unit = "mm")
    ggsave(sprintf("fig%d_%d.png", fignum, 3), f2, dpi = 300, width = 270, height = 90, unit = "mm")
    ggsave(sprintf("fig%d_%d.png", fignum, 4), f3, dpi = 300, width = 270, height = 90, unit = "mm")
    
    options(warn = defaultW)
    
  }

  return(unlabelled)
  
}


# Estimate impact speed distribution.
fitdist <- function(data) {
  
  crashes <- data %>% filter(y > 0)
  
  # Log-normal distribution.
  lnfit <- with(crashes, optim(c(0, 1), function(par) wnll(par, dens = dlnorm, y = y, w = w, p = glance_prob)))
  
  # Gamma distribution.
  mu <- with(crashes, sum(glance_prob * y) / sum(glance_prob))
  sigma2 <- with(crashes, sum(glance_prob * (y - mu)^2) / sum(glance_prob))
  theta0 <- c(shape = mu^2 / sigma2, rate = mu / sigma2) # Initial value.
  gamfit <- with(crashes, optim(theta0, function(par) wnll(par, dens = dgamma, y = y, w = w, p = glance_prob)))
  
  # Choose best fitting model.
  if ( gamfit$value < lnfit$value ) {
    fit <- gamfit
    fit$dist <- "gamma"
  } else {
    fit <- lnfit
    fit$dist <- "lognormal"
  }
  return(fit)
}


# Calculate sampling scheme.
calculate_sampling_scheme <- function(data, crit, fit) {

  if ( crit == "uniform" ) {
    
    pi <- rep(1, nrow(data))
    
  } else if ( crit == "prob" ) {
    
    pi <- with(data, glance_prob * sqrt(prob_positive))
    
  } else if ( crit == "mean" ) {
    
    pi <- with(data, glance_prob * sqrt(prob_positive) * yhat)
    
  } else if ( crit == "moments" ) {
    
    size <- with(data, yhat + yhat^2 + yhat^3 + yhat^4)
    pi <- with(data, glance_prob * sqrt(prob_positive) * size)
    
  } else if ( crit == "std_moments" ) {
    
    size <- with(data, yhat / sd(yhat) + yhat^2 / sd(yhat^2) + yhat^3 / sd(yhat^3) + yhat^4 / sd(yhat^4))
    pi <- with(data, glance_prob * sqrt(prob_positive) * size)    
  } else if ( crit == "nll" ) {
    
    print("Not implemented yet!")
    
  } else if ( crit == "KL" ) {
    
    print("Not implemented yet!")
    
  }
  
  return(pi / sum(pi))
  
}


# Add data-points with certain outcomes (no crash or max impact speed crash) 
# to labelled set, remove from unlabelled set.
add_certainty_outcomes <- function(new_sample, labelled, unlabelled) {
  
  for ( i in unique(new_sample$ID) ) { # Iterate over all cases.
    
    # Min and max impact speed for current case.
    labelled_i <- labelled %>% 
      filter(ID == i)
    ymin <- min(labelled_i$y)
    ymax <- max(labelled_i$y)
    
    # New data for current case.
    labelled_ij <- new_sample %>% 
      filter(ID == i)
    
    
    for ( j in 1:nrow(labelled_ij) ) { # Iterative over all labelled data points.
      
      newx <- labelled_ij[j, ]
      
      if ( newx$y == ymin ) { # Add certainty non-crashes.
        ix <- with(unlabelled, which(ID == i & before <= newx$before & after <= newx$after))
        
        add <- unlabelled %>% 
          filter(row_number() %in% ix) %>%
          mutate(y = ymin) %>% 
          dplyr::select(-yhat, -prob_positive)
        
        labelled <- labelled %>%
          add_row(add)
        
        if ( length(ix) > 0) {
          unlabelled <- unlabelled[-ix, ]
        }
        
      } else if ( newx$y == ymax ) { # Add certainty max impact speed crashes.
        
        ix <- with(unlabelled, which(ID == i & before >= newx$before & after >= newx$after))
        
        add <- unlabelled %>% 
          filter(row_number() %in% ix) %>%
          mutate(y = ymax) %>% 
          dplyr::select(-yhat, -prob_positive)
        
        labelled <- labelled %>%
          add_row(add)
        
        if ( length(ix) > 0) {
          unlabelled <- unlabelled[-ix, ]
        }
      }
    }
  }
  
  return(list(labelled = labelled, unlabelled = unlabelled))
         
}



# Active learning.
active_learning <- function(data, niter = 1e2, bsize = 10, crit = "mean", plot = FALSE) {
  # INPUT
  # data: input data set with variables 
  #   - 'after': glance duration after tauinv = 0.2 s.
  #   - 'before': (glance duration before tauinv = 0.2 s.
  #   - 't': total glance duration (s).
  #   - 'glance_prob': probability of glance.
  #   - 'ID': ID for specific case. 
  # niter: number of iterations.
  # bsize: number of datapoints to sample.
  # crit: optimisation criterion. Should be one of "uniform", "mean", "prob", "moments" or "std_moments".
  # plot: should plots be saved (TRUE/FALSE). 
  #
  # OUTPUT: list of three datasets:
  #   - all: all 'labelled' observations (i.e. for which the outcome has been observed).
  #   - crashes: all generated crashes. 
  #   - noncrashes: all generated non-crashes.
  
  
  # Start grid.
  grid <- tibble(after = c(0, 2, 4, 6.1), before = 0) %>% 
    add_row(after = c(1, 1.5, 3), before = c(3, 1.5, 1)) %>% 
    add_row(after = c(0.5, 4.5), before = c(4.5, 0.5)) %>%
    add_row(after = 3, before = 3) %>%
    mutate(selected = 1)
    
  # Labelled set.
  labelled <- data %>% 
    left_join(grid, by = c("after", "before")) %>%
    filter(selected == 1) %>% 
    mutate(y = get_y(ID, before, after),
           w = 1)
  
  # Unlabelled set.
  unlabelled <- data %>% 
    left_join(grid, by = c("after", "before")) %>%
    filter(is.na(selected)) %>% 
    mutate(w = 0,
           selected = 0,
           yhat = NA,
           prob_positive = NA)
  
  
  # Iterate
  new_sample <- labelled 
  fcount <- 1
  k <- 1
  for ( i in 1:niter ) {
    
    print(sprintf("Iteration %d", i))
    
    # Add data-points with certain outcomes (no crash or max impact speed crash) 
    # to labelled set, remove from unlabelled set.
    update <- add_certainty_outcomes(new_sample, labelled, unlabelled)
    labelled <- update$labelled
    unlabelled <- update$unlabelled

    
    # First iteration: set all weights to 1 for labelled data points.
    if ( i == 1) {
      labelled$w <- 1
    }
    
    # Update predictions for cases with new data.
    for ( j in unique(new_sample$ID) ) {
      
      print(sprintf("Case = %d", j))
      
      # Update predictions.
      pred <- update_predictions(labelled %>% filter(ID == j), 
                                 unlabelled %>% filter(ID == j), plot = plot, fignum = fcount) %>% 
        dplyr::select(ID, before, after, yhat, prob_positive)
      if ( plot ) { fcount <- fcount + 1 }
      
      # Merge to unlabelled dataset.
      unlabelled %<>%
        left_join(pred, by = c("ID", "before", "after")) %>%
        mutate(yhat = ifelse(is.na(yhat.x), yhat.y, yhat.x),
               prob_positive = ifelse(is.na(prob_positive.x), prob_positive.y, prob_positive.x)) %>% 
        dplyr::select(-yhat.x, -yhat.y, -prob_positive.x, -prob_positive.y)
      
    } # End for j in unique(new_sample$ID)
    
  
    # Fit model to impact speed distribution.
    fit <- fitdist(labelled)
    
    # Calculate sampling probabilities. 
    pi <- calculate_sampling_scheme(unlabelled, crit, fit)
    
    
    # Plots.
    if ( plot ) {
      
      p1 <- ggplot(data = labelled) +
        geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
        scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
        coord_equal() +
        expand_limits(fill = c(0, 30)) +
        facet_wrap(~ID) +
        labs(title = sprintf("Iteration %d", i),
             x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = "Impact speed (km/h)") +
        theme(legend.direction = "horizontal",
              legend.justification = 0.5,
              legend.position = "bottom",
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.6, "cm")) 
      
      p2 <- ggplot() +
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat)) +
        geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y), colour = "red", size = 0.01) +
        scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
        coord_equal() +
        expand_limits(fill = c(0, 30)) +
        facet_wrap(~ID) +
        labs(title = sprintf("Iteration %d", i),
             x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = "Anticipated impact speed (km/h)") +
        theme(legend.direction = "horizontal",
              legend.justification = 0.5,
              legend.position = "bottom",
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.6, "cm"))
      
      p3 <- ggplot() +
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive)) +
        geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y>0), colour = "red", size = 0.01) +
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
        coord_equal() +
        facet_wrap(~ID) +
        labs(title = sprintf("Iteration %d", i),
             x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = "Anticipated collision probability") +
        theme(legend.direction = "horizontal",
              legend.justification = 0.5,
              legend.position = "bottom",
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.6, "cm"))
      
      p4 <- ggplot() +
        geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) +
        geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40", colour = "red", size = 0.01) +
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
        coord_equal() +
        facet_wrap(~ID) +
        labs(title = sprintf("Iteration %d", i),
             x = "EOFF after tauinv = 0.2 (s)",
             y = "EOFF before tauinv = 0.2 (s)",
             fill = "Sampling probability") +
        theme(legend.direction = "horizontal",
              legend.justification = 0.5,
              legend.position = "bottom",
              legend.key.height = unit(0.3, "cm"),
              legend.key.width = unit(0.6, "cm"))
      
      ggsave(sprintf("fig%d.png", fcount), p1, dpi = 300, width = 270, height = 190, unit = "mm")
      fcount <- fcount + 1 
      ggsave(sprintf("fig%d.png", fcount), p2, dpi = 300, width = 270, height = 190, unit = "mm")
      fcount <- fcount + 1 
      ggsave(sprintf("fig%d.png", fcount), p3, dpi = 300, width = 270, height = 190, unit = "mm")
      fcount <- fcount + 1 
      ggsave(sprintf("fig%d.png", fcount), p4, dpi = 300, width = 270, height = 190, unit = "mm")
      fcount <- fcount + 1 
      
    }
    
    
    # Sample new observations.
    ix <- as.numeric(rmultinom(n = 1, size = bsize, prob = pi))
    new_wt <- ix / (bsize * pi)
 
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(new_wt = new_wt) %>% 
      filter(new_wt > 0) %>% 
      mutate(y = get_y(ID, before, after),
             selected = 1) %>% 
      dplyr::select(-prob_positive, -yhat)

    
    # Update labelled and unlabelled sets.
    labelled <- labelled %>%
      mutate(new_wt = 1) %>% # Re-query labelled data points with probability 1. 
      add_row(new_sample) %>%
      mutate(w = w + (new_wt - w) / k) %>%
      dplyr::select(-new_wt)
    
    unlabelled %<>%
      mutate(new_wt = new_wt) %>% 
      filter(new_wt <= 0) %>% 
      dplyr::select(-new_wt)
    
    # Increase counter if at least one new crash has been generated.
    k <- k + (any(new_sample$y > 0))
    
    
  } # End active learning.
  
  labelled <- labelled 
  
  crashes <- labelled %>% 
    filter(y > 0) %>% 
    mutate(w_final = w * glance_prob)
  
  noncrashes <- labelled %>% 
    filter(y == 0)
  
  return(list(all = labelled, crashes = crashes, noncrashes = noncrashes))
  
}



# An example --------------------------------------------------------------


set.seed(12345)
df_with_y <- simulate_sample(ncases = 9) %>% 
  dplyr::rename(glance_prob = "prob")
df <- df_with_y %>% dplyr::select(-y)
head(df)

setwd("C:/Users/imbergh/Desktop/Glance")

ggplot(data = df_with_y) +
  geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
  geom_rect(data = df_with_y %>% filter(y == 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
  scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
  coord_equal() + 
  expand_limits(fill = c(0, 50)) + 
  facet_wrap(~ID) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "EOFF before tauinv = 0.2 (s)",
       fill = "Impact speed (km/h)") + 
  theme(legend.direction = "horizontal", 
        legend.justification = 0.5,
        legend.position = "bottom", 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"))
ggsave("fig0a.png", dpi = 300, width = 270, height = 190, unit = "mm")

ggplot(data = df_with_y) +
  geom_point(aes(x = after, y = y, colour = before)) +
  scale_colour_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
  lims(y = c(0, 50)) +
  facet_wrap(~ID) + 
  labs(x = "EOFF after tauinv = 0.2 (s)",
       y = "Impact speed (km/h)",
       colour = "EOFF before tauinv = 0.2 (s) ") +
  theme(legend.direction = "horizontal",
        legend.justification = 0.5,
        legend.position = "bottom",
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"))
ggsave("fig0b.png", dpi = 300, width = 270, height = 190, unit = "mm")


print("crit = prob")
setwd("C:/Users/imbergh/Desktop/Glance/Sim1")
res1 <- active_learning(df, niter = 10, bsize = 10, crit = "uniform", plot = TRUE)

print("crit = prob")
setwd("C:/Users/imbergh/Desktop/Glance/Sim2")
res2 <- active_learning(df, niter = 10, bsize = 10, crit = "prob", plot = TRUE)

print("crit = mean")
setwd("C:/Users/imbergh/Desktop/Glance/Sim3")
res3 <- active_learning(df, niter = 10, bsize = 10,  crit = "mean", plot = TRUE)

print("crit = moments")
setwd("C:/Users/imbergh/Desktop/Glance/Sim4")
res4 <- active_learning(df, niter = 10, bsize = 10, crit = "moments", plot = TRUE)

print("crit = std_moments")
setwd("C:/Users/imbergh/Desktop/Glance/Sim5")
res5 <- active_learning(df, niter = 10, bsize = 10, crit = "std_moments", plot = TRUE)
