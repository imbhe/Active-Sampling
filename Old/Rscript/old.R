if ( plot ) {
  options(warn = -1)
  
  title <- ggplot() + 
    labs(title = sprintf("Iteration %d, Case %d", iter, ID)) + 
    theme_minimal() + 
    theme(plot.title = element_text(size = 1.5 * ptsize, colour = "black", family = "sans", face = "plain", hjust = 0.5))
  
  plt1 <- ggplot(data = labelled) +
    geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
    scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
    coord_equal() +
    expand_limits(fill = c(0, 50)) +
    labs(title = sprintf("Iteration %d, Case %d", iter, ID),
         x = "EOFF after tauinv = 0.2 (s)",
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
  
  
  f1 <- plot_grid(title, plot_grid(plt2, plt3, plt4, ncol = 3), nrow = 2, rel_heights = c(0.05, 1))
  f2 <- plot_grid(title, plot_grid(plt5, plt6, plt7, ncol = 3), nrow = 2, rel_heights = c(0.05, 1))
  f3 <- plot_grid(title, plot_grid(plt8, plt9, plt10, ncol = 3), nrow = 2, rel_heights = c(0.05, 1))
  ggsave(sprintf("Output/fig%d_%d.png", fignum, 1), plt1, dpi = 300, width = 90, height = 90, unit = "mm")
  ggsave(sprintf("Output/fig%d_%d.png", fignum, 2), f1, dpi = 300, width = 270, height = 90, unit = "mm")
  ggsave(sprintf("Output/fig%d_%d.png", fignum, 3), f2, dpi = 300, width = 270, height = 90, unit = "mm")
  ggsave(sprintf("Output/fig%d_%d.png", fignum, 4), f3, dpi = 300, width = 270, height = 90, unit = "mm")
  
  options(warn = defaultW)
  
}

# df <- biv %>% 
#   mutate(y = simulate_y(after, before))
# 
# p1 <- df %>% 
#   ggplot() +
#   geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) + 
#   geom_rect(data = . %>% filter(y == 0), aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "salmon") + 
#   scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
#   coord_equal() +
#   expand_limits(fill = c(0, 50)) + 
#   labs(x = "EOFF after tauinv = 0.2 (s)",
#        y = "EOFF before tauinv = 0.2 (s)",
#        fill = "Impact speed (km/h)") + 
#   theme(legend.direction = "horizontal", 
#         legend.justification = 0.5,
#         legend.position = "bottom", 
#         legend.key.height = unit(0.3, "cm"),
#         legend.key.width = unit(0.6, "cm"))
# 
# p2 <- df %>% 
#   ggplot() +
#   geom_point(aes(x = after, y = y, colour = before)) + 
#   scale_colour_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
#   lims(y = c(0, 50)) + 
#   labs(x = "EOFF after tauinv = 0.2 (s)",
#        y = "Impact speed (km/h)",
#        colour = "EOFF before tauinv = 0.2 (s) ",) + 
#   theme(legend.direction = "horizontal", 
#         legend.justification = 0.5,
#         legend.position = "bottom", 
#         legend.key.height = unit(0.3, "cm"),
#         legend.key.width = unit(0.6, "cm"))
# 
# plot_grid(p1, p2, align = "h", axis = "tblr")


# Fit multinomial model for for probability of no collision, 
# collision with impact speed > 0 and > max, and collision with max impact speed. --

# Random forest, mtry = 1.
rfc1 <- randomForest(yfct ~ after + before, data = labelled, mtry = 1)
p1 <- predict(rfc1, unlabelled, type = "prob")

# Random forest, mtry = 2.
rfc2 <- randomForest(yfct~after + before, data = labelled, mtry = 2)
p2 <- predict(rfc2, unlabelled, type = "prob")

if ( min(table(yfct)) > 2 ) {
  
  # Stratified cross-validation samples.
  nfolds <- min(min(table(yfct)), 10)
  foldid <- rep(NA, length(yfct))
  if ( any(yfct== 0) ) {
    foldid[yfct == 0] <- sample(1:nfolds)
  }
  if ( any(yfct== 1) ) {
    foldid[yfct == 1] <- sample(1:nfolds)
  }
  if ( any(yfct == 2) ) {
    foldid[yfct == 2] <- sample(1:nfolds)
  }
  
  # LASSO logistic regression, simple linear.
  logreg1 <- cv.glmnet(X[, 1:2], yfct, family = "multinomial", grouped = FALSE, foldid = foldid)
  p3 <- predict(logreg1, newx = newX, s = "lambda.min", type = "response")
  
  # LASSO logistic regression, more flexible.
  logreg2 <- cv.glmnet(X, yfct, family = "multinomial", grouped = FALSE, foldid = foldid)
  p4 <- predict(logreg2, newx = newX, s = "lambda.min", type = "response")
  
} else {
  
  p3 <- p4 <- array(dim = c(dim(p1), 1))
  dimnames(p3)[[2]] <- colnames(p1)
  dimnames(p4)[[2]] <- colnames(p1)
  p3[,,1] <- p1
  p4[,,1] <- p2
  
}

if ( any(yfct == 0) ) {
  p10 <- p1[, "0"]
  p20 <- p2[, "0"]
  p30 <- p3[, "0", ]
  p40 <- p4[, "0", ]
} else {
  p10 <- p20 <- p30 <- p40 <- 0
}

if ( any(yfct == 1) ) {
  p11 <- p1[, "1"]
  p21 <- p2[, "1"]
  p31 <- p3[, "1", ]
  p41 <- p4[, "1", ]
} else {
  p11 <- p21 <- p31 <- p41 <- 0
}

if ( any(yfct == 2) ) {
  p12 <- p1[, "2"]
  p22 <- p2[, "2"]
  p32 <- p3[, "2", ]
  p42 <- p4[, "2", ]
} else {
  p12 <- p22 <- p32 <- p42 <- 0
}
options(warn = defaultW)


# Combine and add predictions to unlabelled dataset.
p0 <- (p10 + p20 + p30 +p40) / 4
pmax <- (p12 + p22 + p32 + p42) / 4
pother <- (p11 + p21 + p31 + p41) / 4
p0[p0 >= 1] <- max(p0[p0 < 1]) # Zero-probabilities not allowed.

yhat <- (yhat1 + yhat2 + yhat3 + yhat4) / 4 
ypred = 0 * p0 + pmax * ymax + pother * yhat
ypred[ypred <= 0] <- min(ypred[ypred > 0]) # Zero impact speed not allowed.




labelled %<>% 
  mutate(yfct = factor((y != 0) + (y == ymax)))


# Find minimal impact speed (or no collision) corner cases.
if ( ymin == ymin ) {
  
  selected0 <- labelled_data %>% filter(y == ymin & selected)
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
  
} else {
  
  corners0 <- NULL
  
}


# Find max impact speed corner cases.
selectedMax <- labelled_data %>% filter(y >= ymax & selected)
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
selectedCrash <- labelled_data %>% filter(y > 0 & selected)
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
  mult <- ifelse(beta[2] < 0 | beta[3] < 0, 1, beta[3] / beta[2])
} else {
  
  mult <- 1
  
}

# Estimate collision probability.
if ( ymin == 0 ) {
  
  A <- c(cornersCrash$after + mult * cornersCrash$before, corners0$after + mult * corners0$before)
  b <- c(rep(log(0.99) - log(0.01), nrow(cornersCrash)), rep(log(0.01) - log(0.99), nrow(corners0)))
  alpha <- coef(lm(b~A))
  if ( is.na(alpha[2]) || alpha[2] < 0 ) {
    A <- c(cornersCrash$after, corners0$after)
    alpha <- coef(lm(b~A))
  }
  alpha <- c(alpha, alpha[2] * mult)   
  
  unlabelled_data %<>% 
    mutate(prob_positive = (1 + exp(-(alpha[1] + alpha[2] * after + alpha[3] * before)))^(-1))
  
  for ( j in 1:nrow(unlabelled_data) ) {
    unlabelled_data$prob_positive[j] <- ifelse(any(unlabelled_data$after[j] >= cornersCrash$after & 
                                                     unlabelled_data$before[j] >= cornersCrash$before), 
                                               1, unlabelled_data$prob_positive[j])
  }
  
} else {
  
  unlabelled_data %<>% 
    mutate(prob_positive = 1)
  
}


# Estimate impact speed.
A <- c(selectedCrash$after + mult * selectedCrash$before, corners0$after + mult * corners0$before)
b <- c(selectedCrash$y, corners0$y)
beta1 <- coef(lm(b~A))
beta1 <- c(beta1, beta1[2] * mult)

if ( !is.na(beta1[2]) && beta1[2] < 0 ) {
  A <- c(selectedCrash$after, corners0$after)
  b <- c(selectedCrash$y, corners0$y)
  beta1 <- coef(lm(b~A))
  beta1 <- c(beta1, beta1[2] / 10)
}

A <- c(cornersMax$after + mult * cornersMax$before, corners0$after + mult * corners0$before)
b <- c(cornersMax$y, corners0$y)
beta2 <- coef(lm(b~A))
beta2 <- c(beta2, beta2[2] * mult)

if ( !is.na(beta2[2]) && beta2[2] < 0 ) {
  A <- c(cornersMax$after, corners0$after)
  b <- c(cornersMax$y, corners0$y)
  beta2 <- coef(lm(b~A))
  beta2 <- c(beta2, beta2[2] / 10)
}


unlabelled_data %<>% 
  mutate(yhat1 = pmin(pmax(beta1[1] + beta1[2] * after + beta1[3] * before, max(0.1, ymin)), ymax, na.rm = TRUE),
         yhat2 = pmin(pmax(beta2[1] + beta2[2] * after + beta2[3] * before, max(0.1, ymin)), ymax, na.rm = TRUE),
         yhat = pmax(yhat1, yhat2)) %>% 
  dplyr::select(-yhat1, -yhat2)














# Estimate collision probability. --

if ( ymin == 0 ) {
  
  # Random forest, mtry = 1.
  rfc1 <- randomForest(as.factor(y>0)~after + before, data = labelled, mtry = 1)
  prob_positive1 <- predict(rfc1, unlabelled, type = "prob")[, 2]
  prob_positive1[prob_positive1 <= 0] <- min(prob_positive1[prob_positive1 > 0])
  
  # Random forest, mtry = 2.
  rfc2 <- randomForest(as.factor(y>0)~after + before, data = labelled, mtry = 2)
  prob_positive2 <- predict(rfc2, unlabelled, type = "prob")[, 2]
  prob_positive2[prob_positive2 <= 0] <- min(prob_positive2[prob_positive2 > 0])
  
} else {
  
  prob_positive1 <- prob_positive2 <- 1
  
}

if ( sum(y == 0) > 2 & sum(y > 0) > 2 ) {
  
  # LASSO logistic regression, simple linear.
  lassoc1 <- cv.glmnet(X[, 1:2], as.factor(y>0), family = "binomial", grouped = FALSE)
  prob_positive3 <- as.numeric(predict(lassoc1, newx = newX[, 1:2], s = "lambda.min", type = "response"))
  prob_positive3[prob_positive3 <= 0] <- min(prob_positive3[prob_positive3 > 0])
  
  # LASSO logistic regression, more flexible.
  lassoc2 <- cv.glmnet(X, as.factor(y>0), family = "binomial", grouped = FALSE)
  prob_positive4 <- as.numeric(predict(lassoc2, newx = newX, s = "lambda.min", type = "response"))
  prob_positive4[prob_positive4 <= 0] <- min(prob_positive4[prob_positive4 > 0])
  
} else {
  
  prob_positive3 <- prob_positive4 <- (prob_positive1 + prob_positive2) / 2
  
}
