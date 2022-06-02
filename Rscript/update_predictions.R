# Update collision probability and impact speed predictions.
update_predictions <- function(labelled, unlabelled) {
  
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
  form <- ~ -1 + I((- before)^(-3)) + I((-before)^(-2)) +
    I((-before)^(-1)) + I((-before)^(-0.5)) +
    log(-before) - before + I((-before)^0.5) + I(before^2) + I((-before)^3) +
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
  # if ( length(ix) > 2 ) {
  #   ix <- which(y > 0 & y < ymax)
  # 
  #   lasso <- cv.glmnet(X[ix, ], y[ix], grouped = FALSE)
  #   yhat2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"),
  #                       nfolds = min(length(ix), 10))
  #   yhat2[yhat2 <= 0] <- max(min(yhat2[yhat2 > 0]), ymin)
  #   yhat2[yhat2 > ymax] <- ymax
  # 
  # } else {
  #   yhat2 <- yhat1
  # }
  # 
  yhat2 <- yhat1
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
  # if ( sum(y == 0) > 2 & sum(y > 0) > 2 ) {
  #   lasso <- cv.glmnet(X, as.factor(y > 0), family = "binomial", grouped = FALSE)
  #   ppos2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
  #   ppos2[ppos2 <= 0] <- min(ppos2[ppos2 > 0])
  # } else {
  #   ppos2 <- ppos1
  # }
  ppos2 <- ppos1
  
  # Estimate max impact speed collision probability. --
  
  # Random forest.
  rf <- randomForest(as.factor(y == ymax) ~ after + before, data = labelled)
  pmax1 <- predict(rf, unlabelled, type = "prob")[, 2]
  pmax1[pmax1 <= 0] <- min(pmax1[pmax1 > 0])
  
  # Fractional polynomial LASSO logistic regression.
  # if ( sum(y == ymax) > 2 & sum(y == ymax) > 2 ) {
  #   lasso <- cv.glmnet(X, as.factor(y == ymax), family = "binomial", grouped = FALSE)
  #   pmax2 <- as.numeric(predict(lasso, newx = newX, s = "lambda.1se", type = "response"))
  #   pmax2[pmax2 <= 0] <- min(pmax2[pmax2 > 0])
  # } else {
  #   pmax2 <- pmax1
  # }
  pmax2 <- pmax1
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
  
  # Find certainty crashes, set crash probability to 1 for observations "to the right or above".
  for ( j in 1:nrow(unlabelled) ) {
    unlabelled$prob_positive[j] <- ifelse(any(unlabelled$after[j] >= cornersCrash$after & 
                                                unlabelled$before[j] >= cornersCrash$before), 
                                          1, unlabelled$prob_positive[j])
  }
  
  return(unlabelled)
  
}