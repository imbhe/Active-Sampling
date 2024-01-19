update_predictions <- function(labelled, 
                               unlabelled, 
                               target = c("impact speed reduction",
                                          "crash avoidance"),
                               verbose = FALSE, 
                               plot = FALSE,
                               iter = NA,
                               prediction_model_type = c("rg", "xg_boost", "knn")) {
  
  # Check input parameters.
  target <- match.arg(target)
  prediction_model_type <- match.arg(prediction_model_type)
  
  # Store default handling of warning messages.
  defaultW <- getOption("warn")
  
  # caseID to factor.
  labelled$caseID <- factor(labelled$caseID)
  unlabelled$caseID <- factor(unlabelled$caseID)
  
  # Crashes.
  crashes <- labelled %>% filter(impact_speed0 > 0)
  
  
  # Hyper-parameter tuning grid for random forest regression and classification.
  regGrid <- crossing(splitrule = "variance", 
                      min.node.size = 1:20,
                      mtry = 1:3)
  
  classGrid <- crossing(splitrule = "gini", 
                        min.node.size = 1:20,
                        mtry = 1:3)
  
  
  # Estimate baseline collision probability. ----
  if(prediction_model_type == "rg"){
    # Train random forest.  
    options(warn = -1)
    grid <- classGrid[sample(1:nrow(classGrid), 3), ]
    rf <- safe_caret_train(factor(impact_speed0 > 0, labels = paste0("Y", 0:1)) ~ eoff + acc + impact_speed_max0, 
                           data = labelled,
                           method = "ranger",
                           num.trees = 100,
                           tuneGrid = as.data.frame(grid),
                           trControl = trainControl(method = "cv",
                                                    number = 5,
                                                    classProbs = TRUE))
    options(warn = defaultW)
    
    
    if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
      
      # Prediction on labelled and unlabelled data.
      p0_train <- predict(rf, labelled, type = "prob")$Y1
      p0_train[p0_train <= 0] <- min(p0_train[p0_train > 0]) # Avoid zero probabilities.
      
      p0_test <- predict(rf, unlabelled, type = "prob")$Y1
      p0_test[p0_test <= 0] <- min(p0_test[p0_test > 0]) # Avoid zero probabilities.
      
      # Accuracy.
      ix <- with(rf$results, which(min.node.size == rf$finalModel$min.node.size & mtry == rf$finalModel$mtry))
      p0_acc <- rf$results[ix, "Accuracy"]
      
    } else { # If unable to fit model: set to constant.
      
      p0_train <- 1
      p0_test <- 1
      p0_acc <- NA
      
    } # End !is.null(rf).
    
    
    # Predict impact speed reduction ----
    if ( target == "impact speed reduction" ) {
      
      # Train random forest.  
      options(warn = -1)
      grid <- regGrid[sample(1:nrow(regGrid), 3), ]
      rf <- safe_caret_train(impact_speed_reduction ~ eoff + acc + impact_speed_max0, 
                             data = crashes,
                             method = "ranger",
                             num.trees = 100,
                             tuneGrid = as.data.frame(grid),
                             trControl = trainControl(method = "cv",
                                                      number = 5))
      options(warn = defaultW)
      
      if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
        
        # Prediction on labelled and unlabelled data.
        yhat_train <- predict(rf, crashes)
        yhat_test <- predict(rf, unlabelled)
        
        # Root mean squared prediction error and prediction R-squared.
        rmse_yhat <- sqrt(rf$finalModel$prediction.error) 
        r2_yhat <- rf$finalModel$r.squared
        
      } else { # If unable to fit model: set to constant.
        
        yhat_train <- 1
        yhat_test <- 1
        rmse_yhat <- NA
        r2_yhat <- NA
        
      }
    } else { # If target not impact speed reduction: don't fit model, set to constant.
      
      yhat_train <- 1
      yhat_test <- 1
      rmse_yhat <- NA
      r2_yhat <- NA
      
    }
    
    
    # Estimate counter-measure collision probability. ----
    if ( target == "crash avoidance" ) {
      
      # Train random forest.  
      options(warn = -1)
      grid <- classGrid[sample(1:nrow(classGrid), 3), ]
      rf <- safe_caret_train(factor(impact_speed1 > 0, labels = paste0("Y", 0:1)) ~ eoff + acc + impact_speed_max0, 
                             data = crashes,
                             method = "ranger",
                             num.trees = 100,
                             tuneGrid = as.data.frame(grid),
                             trControl = trainControl(method = "cv",
                                                      number = 5,
                                                      classProbs = TRUE))
      options(warn = defaultW)
      
      if ( !is.null(rf) ) { # If able to fit model: calculate predictions.
        
        # Prediction on labelled and unlabelled data.
        p1_train <- predict(rf, crashes, type = "prob")$Y1
        p1_train[p1_train <= 0] <- min(p1_train[p1_train > 0]) # Avoid zero probabilities.
        
        p1_test <- predict(rf, unlabelled, type = "prob")$Y1
        p1_test[p1_test <= 0] <- min(p1_test[p1_test > 0]) # Avoid zero probabilities.
        
        # Accuracy.
        ix <- with(rf$results, which(min.node.size == rf$finalModel$min.node.size & mtry == rf$finalModel$mtry))
        p1_acc <- rf$results[ix, "Accuracy"]
        
      } else { # If unable to fit model: set to constant.
        
        p1_train <- 1
        p1_test <- 1
        p1_acc <- NA
        
      } # End !is.null(rf).
    } else { # If target not crash avoidance: don't fit model, set to constant.
      
      p1_train <- 1
      p1_test <- 1
      p1_acc <- NA
      
    }
  } else if(prediction_model_type == "xg_boost"){
    # Train xg boost
    options(warn = -1)
    
    # Assuming 'labelled' is your training data and 'unlabelled' is your test data
    labelled_xgb = labelled
    labelled_xgb$impact_speed0_factor = as.numeric(labelled$impact_speed0 > 0)
    train_label = labelled_xgb$impact_speed0_factor
    labelled_xgb_matrix = sparse.model.matrix(impact_speed0 ~ eoff + acc + impact_speed_max0,
                                              data = labelled_xgb)
    train_matrix = xgb.DMatrix(data = labelled_xgb_matrix, label = train_label)    
    
    unlabelled_xgb = unlabelled
    unlabelled_xgb$impact_speed0_factor = as.numeric(unlabelled$impact_speed0 > 0)
    test_label = unlabelled_xgb$impact_speed0_factor
    unlabelled_xgb_matrix = sparse.model.matrix(impact_speed0 ~ eoff + acc + impact_speed_max0,
                                                data = unlabelled_xgb)
    test_matrix = xgb.DMatrix(data = unlabelled_xgb_matrix, label = test_label)  
      param_grid <- expand.grid(
      eta = c(0.01, 0.1, 0.2),  # Learning rate
      max_depth = c(3, 6),      # Maximum depth of a tree
      nrounds = c(100, 300)     # Number of boosting rounds
    )
    # Initialize best score
    best_score <- Inf
    xgb <- NULL
    best_params <- list()
    best_nround <- NULL
    
    # Search over the grid
    for (i in 1:nrow(param_grid)) {
      params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = param_grid[i, "eta"],
        max_depth = param_grid[i, "max_depth"]
      )
      nrounds = param_grid[i, "nrounds"]
      
      xgb_model <- xgb.train(
        params = params,
        data = train_matrix,
        nrounds = nrounds,
        watchlist = list(train = train_matrix, test = test_matrix),
        verbose = 0
      )
      
      # Accessing the last Log Loss value from the evaluation history
      last_logloss <- min(xgb_model$evaluation_log$test_logloss)
      
      # Update best model based on Log Loss
      if (last_logloss < best_score) {
        best_score <- last_logloss
        xgb <- xgb_model
        best_params <- params
        best_nround <- nrounds
      }
    }
    options(warn = defaultW)
    
    if (!is.null(xgb)) {
      p0_train <- predict(xgb, newdata = train_matrix)
      p0_train[p0_train <= 0] <- min(p0_train[p0_train > 0])  # Avoid zero probabilities.
      
      p0_test <- predict(xgb, newdata = test_matrix)
      p0_test[p0_test <= 0] <- min(p0_test[p0_test > 0])  # Avoid zero probabilities.
      
      # Accuracy. recheck later
      p0_acc <-( sum(p0_train>=0.5 & train_label == "1") + sum(p0_train<0.5 & train_label == "0") )/length(p0_train)
      
    } else {
      # If unable to fit model: set to constant.
      p0_train <- 1
      p0_test <- 1
      p0_acc <- NA
    }  # End !is.null(xgb).
    if (target == "impact speed reduction") {
      
      # Train XGBoost.  
      options(warn = -1)
      
      crashes_xgb = crashes
      crashes_xgb_matrix = sparse.model.matrix(impact_speed_reduction~eoff + acc + impact_speed_max0,
                                               data = crashes_xgb)
      train_matrix = xgb.DMatrix(data = crashes_xgb_matrix, label = crashes$impact_speed_reduction)
      
      unlabelled_xgb = unlabelled
      unlabelled_xgb_matrix = sparse.model.matrix(impact_speed_reduction~eoff + acc + impact_speed_max0,
                                                  data = unlabelled_xgb)
      test_matrix = xgb.DMatrix(data = unlabelled_xgb_matrix, label = unlabelled$impact_speed_reduction)
      param_grid <- expand.grid(
        eta = c(0.01, 0.1, 0.2),  # Learning rate
        max_depth = c(3, 6),      # Maximum depth of a tree
        nrounds = c(100, 300)     # Number of boosting rounds
      )
      
      # Initialize best score
      best_score <- Inf
      xgb <- NULL
      best_params <- list()
      best_nround <- NULL
      
      # Search over the grid
      for (i in 1:nrow(param_grid)) {
        params <- list(
          objective = "reg:squarederror",
          eta = param_grid[i, "eta"],
          max_depth = param_grid[i, "max_depth"]
        )
        nrounds = param_grid[i, "nrounds"]
        
        xgb_model <- xgb.train(
          params = params,
          data = train_matrix,
          nrounds = nrounds,
          watchlist = list(train = train_matrix, test = test_matrix),
          eval_metric = "rmse",
          verbose = 0
        )
        
        # Accessing the last RMSE value from the evaluation history
        last_rmse <- min(xgb_model$evaluation_log$test_rmse)
        
        # Update best model based on RMSE
        if (last_rmse < best_score) {
          best_score <- last_rmse
          xgb <- xgb_model
          best_params <- params
          best_nround <- nrounds
        }
      }
      
      if (!is.null(xgb)) { # If able to fit model: calculate predictions.
        
        # Prediction on labelled and unlabelled data.
        yhat_train <- predict(xgb, newdata = train_matrix)
        yhat_test <- predict(xgb, newdata = test_matrix)
        
        # Root mean squared prediction error and prediction R-squared.
        rmse_yhat <- sqrt(mean((yhat_train - crashes$impact_speed_reduction)^2))
        r2_yhat <- 1 - (sum((crashes$impact_speed_reduction - yhat_train)^2) / sum((crashes$impact_speed_reduction - mean(crashes$impact_speed_reduction))^2))
        
      } else { # If unable to fit model: set to constant.
        
        yhat_train <- 1
        yhat_test <- 1
        rmse_yhat <- NA
        r2_yhat <- NA
        
      }
    } else { # If target not impact speed reduction: don't fit model, set to constant.
      
      yhat_train <- 1
      yhat_test <- 1
      rmse_yhat <- NA
      r2_yhat <- NA
      
    }
    if (target == "crash avoidance") {
      
      # Train XGBoost.  
      options(warn = -1)
      crashes_xgb = crashes
      crashes_xgb$impact_speed1_factor = as.numeric(crashes$impact_speed1 > 0)
      train_label = crashes_xgb$impact_speed1_factor
      crashes_xgb_matrix = sparse.model.matrix(impact_speed1_factor~eoff + acc + impact_speed_max0,
                                               data = crashes_xgb)
      train_matrix = xgb.DMatrix(data = crashes_xgb_matrix, label = train_label)    
      
      unlabelled_xgb = unlabelled
      unlabelled_xgb$impact_speed1_factor = as.numeric(unlabelled$impact_speed1 > 0)
      test_label = unlabelled_xgb$impact_speed1_factor
      unlabelled_xgb_matrix = sparse.model.matrix(impact_speed1_factor~eoff + acc + impact_speed_max0,
                                                  data = unlabelled_xgb)
      test_matrix = xgb.DMatrix(data = unlabelled_xgb_matrix, label = test_label)  
      param_grid <- expand.grid(
        eta = c(0.01, 0.1, 0.2),  # Learning rate
        max_depth = c(3, 6),      # Maximum depth of a tree
        nrounds = c(100, 300)     # Number of boosting rounds
      )

      # Initialize best score
      best_score <- Inf
      xgb <- NULL
      best_params <- list()
      best_nround <- NULL
      
      # Search over the grid
      for (i in 1:nrow(param_grid)) {
        params <- list(
          objective = "binary:logistic",
          eval_metric = "logloss",
          eta = param_grid[i, "eta"],
          max_depth = param_grid[i, "max_depth"]
        )
        nrounds = param_grid[i, "nrounds"]
        
        xgb_model <- xgb.train(
          params = params,
          data = train_matrix,
          nrounds = nrounds,
          watchlist = list(train = train_matrix, test = test_matrix),
          verbose = 0
        )
        
        # Accessing the last Log Loss value from the evaluation history
        last_logloss <- min(xgb_model$evaluation_log$test_logloss)
        
        # Update best model based on Log Loss
        if (last_logloss < best_score) {
          best_score <- last_logloss
          xgb <- xgb_model
          best_params <- params
          best_nround <- nrounds
        }
      }
      
      # best_model now contains the model with the lowest Log Loss
      options(warn = defaultW)
      if (!is.null(xgb)) { # If able to fit model: calculate predictions.
        # Prediction on labelled and unlabelled data.
        p1_train <- predict(xgb, newdata = train_matrix)
        p1_train[p1_train <= 0] <- min(p1_train[p1_train > 0])  # Avoid zero probabilities.
        
        p1_test <- predict(xgb, newdata = test_matrix)
        p1_test[p1_test <= 0] <- min(p1_test[p1_test > 0])  # Avoid zero probabilities.
        
        # Accuracy.
        p1_acc <- ( sum(p0_train>=0.5 & train_label == "Y1") + sum(p0_train<0.5 & train_label == "Y0") )/
          length(p0_train)
        
      } else { # If unable to fit model: set to constant.
        
        p1_train <- 1
        p1_test <- 1
        p1_acc <- NA
        
      } # End !is.null(xgb).
    } else { # If target not crash avoidance: don't fit model, set to constant.
      
      p1_train <- 1
      p1_test <- 1
      p1_acc <- NA
      
    }
  }
  else if(prediction_model_type == "knn"){
    # Train knn
    options(warn = -1)
    labelled_knn = labelled

    labelled_knn$impact_speed0_factor = as.factor(labelled$impact_speed0 > 0)
    train_label = labelled_knn$impact_speed0_factor
    unlabelled_knn = unlabelled
    unlabelled_knn$impact_speed0_factor = as.factor(unlabelled$impact_speed0 > 0)
    test_label = unlabelled_knn$impact_speed0_factor
    # Prepare the data for k-NN
    train_data <- labelled_knn[c("eoff", "acc", "impact_speed_max0","impact_speed0_factor")]
    test_data <- unlabelled_knn[c("eoff", "acc", "impact_speed_max0","impact_speed0_factor")]
    
    # Preprocess the data: scaling the features
    preProcValues <- preProcess(train_data, method = c("center", "scale"))
    
    train_data_processed <- predict(preProcValues, train_data)
    test_data_processed <- predict(preProcValues, test_data)
    
    # Train a k-NN model
    set.seed(123)  # for reproducibility
    knn_fit <- train(impact_speed0_factor ~ eoff + acc + impact_speed_max0, data = train_data_processed, method = "knn", 
                     tuneLength = 10,  # to try 30 different values of k
                     trControl = trainControl(method = "cv", number = 10))  # 10-fold cross-validation
    options(warn = defaultW)
    
    if (!is.null(knn_fit)) {
      p0_train <- predict(knn_fit, newdata = train_data_processed, type = "prob")$`TRUE`
      p0_test <- predict(knn_fit, newdata = test_data_processed, type = "prob")$`TRUE`
      # Accuracy. recheck later
      p0_acc <- sum(predict(knn_fit, newdata = train_data_processed)==train_data_processed$impact_speed0_factor) /length(train_data_processed$impact_speed_max0)
      
    } else {
      # If unable to fit model: set to constant.
      p0_train <- 1
      p0_test <- 1
      p0_acc <- NA
    }  
    if (target == "impact speed reduction") {
      
      # Train knn.  
      options(warn = -1)
      
      # Prepare the data
      train_features <- crashes[c("eoff", "acc", "impact_speed_max0")]
      train_label <- crashes$impact_speed_reduction
      test_features <- unlabelled[c("eoff", "acc", "impact_speed_max0")]
      test_label <- unlabelled$impact_speed_reduction  # Assuming you have the actual values for evaluation
      
      # Preprocess the data: scaling the features only
      preProcValues <- preProcess(train_features, method = c("center", "scale"))
      train_features_processed <- predict(preProcValues, train_features)
      test_features_processed <- predict(preProcValues, test_features)
      
      # Combine the processed features with the target variable
      train_data_processed <- cbind(train_features_processed, impact_speed_reduction = train_label)
      test_data_processed <- cbind(test_features_processed, impact_speed_reduction = test_label)
      
      
      # Train a k-NN model
      set.seed(123)  # for reproducibility
      knn_fit <- train(impact_speed_reduction~ eoff + acc + impact_speed_max0, data = train_data_processed, method = "knn", 
                       tuneLength = 10,  # to try 10 different values of k
                       trControl = trainControl(method = "cv", number = 10))  # 10-fold cross-validation
      options(warn = defaultW)
      
      if (!is.null(knn_fit)) { # If able to fit model: calculate predictions.
        yhat_train <-  predict(knn_fit, newdata = train_data_processed)
        yhat_test <-   predict(knn_fit, newdata = test_data_processed)
        # Root mean squared prediction error and prediction R-squared.
        rmse_yhat <- sqrt(mean((yhat_train - crashes$impact_speed_reduction)^2))
        r2_yhat <- 1 - (sum((crashes$impact_speed_reduction - yhat_train)^2) / sum((crashes$impact_speed_reduction - mean(crashes$impact_speed_reduction))^2))
        
      } else { # If unable to fit model: set to constant.
        
        yhat_train <- 1
        yhat_test <- 1
        rmse_yhat <- NA
        r2_yhat <- NA
        
      }
    } else { # If target not impact speed reduction: don't fit model, set to constant.
      
      yhat_train <- 1
      yhat_test <- 1
      rmse_yhat <- NA
      r2_yhat <- NA
      
    }
    if (target == "crash avoidance") {
      labelled_knn = crashes
      labelled_knn$impact_speed1_factor = as.factor(crashes$impact_speed1 > 0)
      train_label = labelled_knn$impact_speed1_factor
      unlabelled_knn = unlabelled
      unlabelled_knn$impact_speed1_factor = as.factor(unlabelled$impact_speed1 > 0)
      test_label = unlabelled_knn$impact_speed1_factor
      # Prepare the data for k-NN
      train_data <- labelled_knn[c("eoff", "acc", "impact_speed_max0","impact_speed1","impact_speed1_factor")]
      test_data <- unlabelled_knn[c("eoff", "acc", "impact_speed_max0","impact_speed1","impact_speed1_factor")]
      preProcValues <- preProcess(train_data, method = c("center", "scale"))
      
      train_data_processed <- predict(preProcValues, train_data)
      test_data_processed <- predict(preProcValues, test_data)
      
      # Train a k-NN model
      set.seed(123)  # for reproducibility
      knn_fit <- train(impact_speed1_factor ~ eoff + acc + impact_speed_max0, data = train_data_processed, method = "knn", 
                       tuneLength = 10,  # to try 10 different values of k
                       trControl = trainControl(method = "cv", number = 10))  # 10-fold cross-validation
      options(warn = defaultW)
      
      if (!is.null(knn_fit)) {
        p1_train <- predict(knn_fit, newdata = train_data_processed,type = "prob")$`TRUE`
        p1_train[p1_train <= 0] <- min(p1_train[p1_train > 0]) # Avoid zero probabilities.
        
        p1_test <- predict(knn_fit, newdata = test_data_processed, type = "prob")$`TRUE`
        p1_test[p1_test <= 0] <- min(p1_test[p1_test > 0]) # Avoid zero probabilities.
        p1_acc <- sum(predict(knn_fit, newdata = train_data_processed)==train_data_processed$impact_speed1_factor) /length(train_data_processed$impact_speed_max0)
        
      } else {
        # If unable to fit model: set to constant.
        p1_train <- 1
        p1_test <- 1
        p1_acc <- NA
      }  # End !is.null(xgb).
      
    } else { # If target not crash avoidance: don't fit model, set to constant.
      
      p1_train <- 1
      p1_test <- 1
      p1_acc <- NA
      
    }
  }
  # Combine to calculate predictions on labelled  data. 
  labelled %<>% 
    mutate(collision_prob0_pred = p0_train)
  
  crashes %<>%
    mutate(impact_speed_reduction_pred = yhat_train, 
           collision_prob1_pred = p1_train) 
  
  # Print.
  if ( verbose ) {
    
    cat(sprintf("Out-of-bag R-squared:
Impact speed reduction = %.2f

Out-of-bag Accuracy:
Baseline crash probability = %.2f.
Counter-meature crash probability = %.2f.
                ", 
                r2_yhat,
                p0_acc,
                p1_acc))
    cat("\n")
  }
  
  
  # Plot. ----
  if ( plot ) {
    
    # Plot data.
    plt <- unlabelled %>% 
      mutate(caseID = as.numeric(caseID)) %>%
      mutate(collision_prob0 = p0_test,
             collision_prob1 = p1_test,
             impact_speed_reduction_pred = yhat_test) %>% 
      filter(caseID <= 42) # Plot 42 cases on 7x6 grid. 
    
    
    # Baseline collision probability.
    
    # 1D.
    ggplot(plt) +
      geom_point(aes(x = eoff, y = 100 * collision_prob0, colour = -acc)) +
      scale_colour_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = "Baseline collision probability (%)",
           colour = bquote('Maximal deceleration '(km/s^2))) +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselineCollisionProbability_1D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(plt) +
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = 100 * collision_prob0)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline collision probability (%)") +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Output/BaselineCollisionProbability_2D_Iter%d.png", iter)
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    
    # Impact speed reduction.
    if ( target == "impact speed reduction" ) {
      
      # 1D.
      ggplot(plt) +
        geom_point(aes(x = eoff, y = impact_speed_reduction_pred, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Impact speed reduction (km/h)",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ImpactSpeedReduction_Pred_1D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
      # 2D.
      ggplot(plt) +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed_reduction_pred)) +
        scale_fill_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Impact speed reduction (km/h)") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ImpactSpeedReduction_Pred_2D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
    }
    
    
    # Countermeasure collision probability.
    if ( target == "crash avoidance" ) {
      
      # 1D.
      ggplot(plt) +
        geom_point(aes(x = eoff, y = 100 * collision_prob1, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Countermeasure collision probability (%)",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/CountermeasureCollisionProbability_1D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
      # 2D.
      ggplot(plt) +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = 100 * collision_prob1)) +
        scale_fill_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Countermeasure collision probability (%)") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/CountermeasureCollisionProbability_2D_Iter%d.png", iter)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
    }
    
  } # End plot.
  
  
  # Return.

  return(list(collision_prob0 = p0_test,
              collision_prob1 = p1_test,
              impact_speed_reduction_pred = yhat_test,
              rmse_impact_speed_reduction = rmse_yhat,
              r2_impact_speed_reduction = r2_yhat,
              accuracy_crash0 = p0_acc,
              accuracy_crash1 = p1_acc))
  
}
