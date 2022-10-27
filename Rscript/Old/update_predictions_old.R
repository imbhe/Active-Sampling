update_predictions <- function(labelled, unlabelled, verbose = FALSE, plot = FALSE) {
  
  defaultW <- getOption("warn")
  
  
  # Transform impact speed to common range [0, 1].
  maximpact0 <- labelled %>% 
    group_by(caseID) %>% 
    summarise(maximpact0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() %>% 
    dplyr::select(caseID, maximpact0)
  
  labelled %<>% 
    left_join(maximpact0, by = "caseID") %>% 
    mutate(impact_speed0 = impact_speed0 / maximpact0,
           impact_speed1 = impact_speed1 / maximpact0)
  
  
  # Labelled, wide to long format.
  labelled0 <- labelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, final_weight, impact_speed0) %>% 
    mutate(scenario = 0) %>% 
    dplyr::rename("impact_speed" = impact_speed0)
  
  labelled1 <- labelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, final_weight, impact_speed1) %>% 
    mutate(scenario = 1) %>% 
    dplyr::rename("impact_speed" = impact_speed1)
  
  labelled <- labelled0 %>% 
    add_row(labelled1) %>% 
    left_join(maximpact0, by = "caseID") %>% 
    mutate(caseID = factor(caseID, labels = paste0("caseID", sort(unique(caseID)))))
  
  
  # Unlabelled, wide to long format.
  unlabelled0 <- unlabelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, crash0, non_crash0, max_impact0) %>% 
    mutate(scenario = 0) %>% 
    dplyr::rename("crash" = crash0,
                  "non_crash" = non_crash0,
                  "max_impact" = max_impact0)
  
  unlabelled1 <- unlabelled %>% 
    dplyr::select(caseID, eoff, acc, eoff_acc_prob, crash1, non_crash1, max_impact1) %>% 
    mutate(scenario = 1) %>% 
    dplyr::rename("crash" = crash1,
                  "non_crash" = non_crash1,
                  "max_impact" = max_impact1)
  
  unlabelled <- unlabelled0 %>% 
    add_row(unlabelled1) %>% 
    left_join(maximpact0, by = "caseID") %>% 
    mutate(caseID = factor(caseID, labels = paste0("caseID", sort(unique(caseID)))))
  
  
  # Some variables.
  crash <- unlabelled$crash
  non_crash <- unlabelled$non_crash
  max_impact <- unlabelled$max_impact
  ymin <- min(labelled$impact_speed)
  ymax <- 1
  
  
  # Estimate impact speed when > 0 and < max. ----
  
  # Train random forest.  
  options(warn = -1)
  train <- labelled %>% filter(impact_speed > 0 & impact_speed < ymax)
  rf <- safe_caret_train(log(impact_speed) ~ scenario + caseID + eoff + acc, # Log impact speed to account for heteroscedasticity.
                         data = train,
                         method = "ranger",
                         num.trees = 100,
                         tuneLength = 3,
                         trControl = trainControl(method = "cv",
                                                  number = 5,
                                                  search = "random"))
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    # Root mean squared error of predictions on log-scale.
    rmse <- sd(log(train$impact_speed) - predict(rf, train))
    
    # Prediction on labelled data.
    yhat_train <- exp(predict(rf, labelled) + rmse^2 / 2) 
    yhat_train[yhat_train <= 0] <- max(min(yhat_train[yhat_train > 0]), 0)
    yhat_train[yhat_train > ymax] <- ymax
    
    # Prediction unlabelled data.
    yhat_test <- exp(predict(rf, unlabelled))
    yhat_test[yhat_test <= 0] <- max(min(yhat_test[yhat_test > 0]), 0)
    yhat_test[yhat_test > ymax] <- ymax
    
  } else { # If unable to fit model: use mean of 0 and max impact speed. 
    
    yhat_train <- rep(ymax / 2, nrow(labelled))
    yhat_test <- rep(ymax / 2, nrow(unlabelled))
    
  }
  
  
  # Estimate collision probability. ----
  
  # Train random forest.
  options(warn = -1)
  sink("msg.txt")
  rf <- safe_caret_train(factor(impact_speed > 0, labels = paste0("Y", 0:1)) ~ scenario + caseID + eoff + acc, 
                         data = labelled,
                         method = "ranger",
                         num.trees = 100,
                         tuneLength = 3,
                         trControl = trainControl(method = "cv",
                                                  number = 5,
                                                  search = "random",
                                                  classProbs = TRUE))
  sink()
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    # Prediction on labelled data.
    ppos_train <- predict(rf, labelled, type = "prob")$Y1
    ppos_train[ppos_train <= 0] <- min(ppos_train[ppos_train > 0])
    
    # Prediction on unlabelled data.
    ppos_test <- predict(rf, unlabelled, type = "prob")$Y1
    ppos_test[ppos_test <= 0] <- min(ppos_test[ppos_test > 0])
    
  } else { # If unable to fit model: set probabilities manually.
    
    if ( ymax == 0 ) { # If no crashes.
      
      ppos_train <- rep(0, nrow(labelled))
      ppos_test <- rep(0, nrow(unlabelled))
      
    } else if ( ymin > 0 ) { # If no non-crashes.
      
      ppos_train <- rep(1, nrow(labelled))
      ppos_test <- rep(1, nrow(unlabelled))
      
    } else {
      
      stop("Error in update_predictions > Estimate collision probability > Random forest >  Unable to fit model. Case not implemented. ")
      
    }
    
  } # End !is.null(rf).
  
  
  # Estimate max impact speed collision probability. ----
  
  # Train random forest.
  options(warn = -1)
  sink("msg.txt")
  rf <- safe_caret_train(factor(impact_speed == ymax, labels = paste0("Y", 0:1)) ~ 
                           scenario + caseID + eoff + acc, 
                         data = labelled %>% mutate(ymax = ymax),
                         method = "ranger",
                         num.trees = 100,
                         tuneLength = 3,
                         trControl = trainControl(method = "cv",
                                                  number = 5,
                                                  search = "random",
                                                  classProbs = TRUE))
  sink()
  options(warn = defaultW)
  
  if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
    
    # Prediction on labelled data.
    pmax_train <- predict(rf, labelled, type = "prob")$Y1
    pmax_train[pmax_train] <- min(pmax_train[pmax_train > 0])
    
    # Prediction on unlabelled data.
    pmax_test <- predict(rf, unlabelled, type = "prob")$Y1
    pmax_test[pmax_test <= 0] <- min(pmax_test[pmax_test > 0])
    
  } else { # If unable to fit model: set probabilities manually.
    
    if ( ymin == ymax ) { # If all crashes at maximal impact speed.
      
      pmax_train <- rep(1, nrow(labelled))
      pmax_test <- rep(1, nrow(unlabelled))
      
    } else {
      
      stop("Error in update_predictions > Estimate max impact speed collision probability > Random forest >  Unable to fit model. Case not implemented. ")
      
    }
  }
  
  
  # Combine to calculate predictions on labelled  data. 
  labelled %<>%
    mutate(impact_speed = impact_speed * maximpact0, 
           injury_risk = (1 + exp(-(-5.35 + 0.11 * impact_speed / 2)))^(-1), 
           collision_prob_pred = ppos_train, 
           impact_speed_pred = maximpact0 * (ppos_train * yhat_train + pmax_train * (ymax - yhat_train)), # Predicted impact speed.
           injury_risk_pred = (1 + exp(-(-5.35 + 0.11 * impact_speed_pred / 2)))^(-1)) # Predicted injury risk.
  
  
  # Combine to calculate predictions on unlabelled data + add logic.
  ppos <- pmax(ppos_test, crash, na.rm = TRUE) 
  ppos <- ifelse( !is.na(non_crash) & non_crash, rep(0, length(ppos)), ppos)
  pmax <- pmax(pmax_test, max_impact, na.rm = TRUE) 
  pmax <- ifelse( !is.na(non_crash) & non_crash, rep(0, length(pmax)), pmax)
  yhat <- yhat_test
  ypred <- unlabelled$maximpact0 * (ppos * yhat + pmax * (ymax - yhat))
  
  
  if ( plot ) {
    par(mfrow = c(1, 2))
    plot(labelled$eoff, yhat_train, col = labelled$scenario + 1, bty = "l", ylim = c(0, 1))
    plot(unlabelled$eoff, yhat_test, col = unlabelled$scenario + 1, bty = "l", ylim = c(0, 1))
    
    plot(labelled$eoff, ppos_train, col = labelled$scenario + 1, bty = "l", ylim = c(0, 1))
    plot(unlabelled$eoff, ppos_test, col = unlabelled$scenario + 1, bty = "l", ylim = c(0, 1))
    
    plot(labelled$eoff, pmax_train, col = labelled$scenario + 1, bty = "l", ylim = c(0, 1))
    plot(unlabelled$eoff, pmax_test, col = unlabelled$scenario + 1, bty = "l", ylim = c(0, 1))
    
    par(mfrow = c(1, 1))
    plot(labelled$eoff, labelled$impact_speed_pred, col = labelled$scenario + 1, bty = "l")
    plot(unlabelled$eoff, ypred, col = unlabelled$scenario + 1, bty = "l")
  }
  
  
  # Labelled data, long to wide format.
  labelled0 <- labelled %>% 
    filter(scenario == 0) %>% 
    dplyr::rename("impact_speed0" = impact_speed,
                  "collision_prob0_pred" = collision_prob_pred,
                  "impact_speed0_pred" = impact_speed_pred,
                  "injury_risk0" = injury_risk,
                  "injury_risk0_pred" = injury_risk_pred) %>% 
    dplyr::select(-scenario, -maximpact0)
  
  labelled1 <- labelled %>% 
    filter(scenario == 1) %>% 
    dplyr::rename("impact_speed1" = impact_speed,
                  "collision_prob1_pred" = collision_prob_pred,
                  "impact_speed1_pred" = impact_speed_pred,
                  "injury_risk1" = injury_risk,
                  "injury_risk1_pred" = injury_risk_pred) %>%
    dplyr::select(-scenario, -maximpact0)
  
  labelled <- labelled0 %>% 
    left_join(labelled1, by = c("caseID", "eoff", "acc", "eoff_acc_prob", "final_weight")) %>% 
    mutate(log_impact_speed0_sqresid = (log(impact_speed0) - log(impact_speed0_pred))^2, 
           impact_speed_reduction = impact_speed0 - impact_speed1,
           impact_speed_reduction_pred = pmax(impact_speed0_pred - impact_speed1_pred, 0), # Never smaller than 0.
           impact_speed_reduction_sqresid = (impact_speed_reduction - impact_speed_reduction_pred)^2,
           injury_risk_reduction = pmax(injury_risk0 - injury_risk1), 
           injury_risk_reduction_pred = pmax(injury_risk0_pred - injury_risk1_pred, 0), # Never smaller than 0. 
           injury_risk_reduction_sqresid = (injury_risk_reduction - injury_risk_reduction_pred)^2)
  
  
  if ( verbose ) {
    
    r2_baseline_impact_speed <- with(labelled, cor(impact_speed0, impact_speed0_pred))^2
    r2_impact_speed_reduction <- with(labelled, cor(impact_speed_reduction, impact_speed_reduction_pred))^2
    r2_injury_risk_reduction <- with(labelled, cor(injury_risk_reduction, injury_risk_reduction_pred))^2
    r2_crash0 <- with(labelled, cor(impact_speed0 > 0, collision_prob0_pred))^2
    r2_crash1 <- with(labelled, cor(impact_speed1 > 0, collision_prob1_pred))^2
    
    cat(sprintf("R-squared, training data:
Baseline impact speed = %.2f
Impact speed reduction = %.2f
Injury risk reduction = %.2f
Baseline crash probability = %.2f
Counter-meature crash probability = %.2f.", 
                r2_baseline_impact_speed, 
                r2_impact_speed_reduction,
                r2_injury_risk_reduction,
                r2_crash0,
                r2_crash1))
  }
  
  
  # RMSE on training data.
  impact_speed0_rmse <- with(labelled %>% filter(impact_speed0 > 0 & impact_speed0_pred > 0), sqrt(mean(log_impact_speed0_sqresid)))
  impact_speed_reduction_rmse <- with(labelled, sqrt(mean(impact_speed_reduction_sqresid)))
  injury_risk_reduction_rmse <- with(labelled, sqrt(mean(injury_risk_reduction_sqresid)))
  
  
  # Return.
  return(list(collision_prob0 = as.numeric(ppos[unlabelled$scenario == 0]), 
              collision_prob1 = as.numeric(ppos[unlabelled$scenario == 1]),
              impact_speed_pred0 = as.numeric(ypred[unlabelled$scenario == 0]),
              impact_speed_pred1 = as.numeric(ypred[unlabelled$scenario == 1]),
              impact_speed0_rmse = impact_speed0_rmse,
              impact_speed_reduction_rmse = impact_speed_reduction_rmse,
              injury_risk_reduction_rmse))
  
}
