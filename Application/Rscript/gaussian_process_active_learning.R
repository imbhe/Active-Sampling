library("kernlab")
library("magrittr")
library("purrr")
library("tidyverse")

rm(list = ls())
gc()

load("Application/Data/Data.R")

source("Application/Rscript/estimate_targets.R")
source("Application/Rscript/safe_gausspr_train.R")

run_experiments <- function(yvar, n_per_case_per_iter = 5, niter = 10, nreps = 50, plot = F) {
  
  t0 <- Sys.time()
  
  df %<>% 
    mutate(z1 = eoff / max(eoff),
           z2 = -acc,
           z2 = (z2 - min(z2)) / diff(range(z2)),
           caseID = as.numeric(caseID))
  
  Z <- as.matrix(df[, paste0("z", 1:2)])
  Y <- df[, yvar, drop = TRUE]
  N <- nrow(df)
  n_cases <- length(unique(df$caseID))
  
  # Calculate target quantities on full data.
  ground_truth <- estimate_targets(df, weightvar = "eoff_acc_prob") 
  if ( yvar == "impact_speed_reduction" ) {
    ground_truth <- ground_truth["mean_impact_speed_reduction"]
  } else if ( yvar == "crash_avoidance") {
    ground_truth <- ground_truth["crash_avoidance_rate"]
  }
  
  sqerr <- matrix(NA, nrow = nreps, ncol = niter)
  
  for ( i in 1:nreps ) {
    # print(sprintf("i = %d", i))
    
    sd_pred <- rep(1, N) 
    labelled <- NULL
    
    for ( j in 1:niter )  {
      print(sprintf("j = %d", j))
      
      # Select new subsample. 
      
      # Make sure sd_pred > 0.
      if ( all(is.na(sd_pred)) || max(sd_pred, na.rm = TRUE) == 0 ) {
        sd_pred <- rep(1, N) 
      } else {
        sd_pred[sd_pred == 0] <- min(sd_pred[sd_pred > 0])
        sd_pred[is.na(sd_pred)] <- min(sd_pred[!is.na(sd_pred)])        
      }
      
      # Stratified sampling by case ID. 
      for ( k in 1:n_cases ) {
        ix <- which(df$caseID == k)
        pi <- sd_pred[ix]
        x <- rmultinom(1, n_per_case_per_iter, pi)
        jx <- rep(ix, x)
        labelled <- c(labelled, jx)      
      }
      
      # Fit Gaussian process regression model for probability of collision in baseline scenario.
      pred0 <- rep(NA, N)
      for ( k in 1:n_cases ) {
        
        ix <- which(df$caseID == k)
        jx <- intersect(labelled, ix)
        gp <- quietly(safe_gausspr_train)(as.factor(impact_speed0>0)~z1+z2, data = df[jx, ])$result
        
        if ( !is.null(gp) && is.na(gp@error) ) {
          gp <- quietly(safe_gausspr_train)(as.factor(impact_speed0>0)~z1+z2, data = df[jx, ])$result
        } 
        
        # Predictions. 
        if ( is.null(gp) ) {
          pred0[ix] <- rep(mean(df$impact_speed0[jx] > 0), length(ix))
        } else {
          pred0[ix] <- predict(gp, df[ix, ], type = "probabilities")[, 2]
        }
      }
      
      # Fit model to target variable impact speed reduction or crash avoidance. 
      pred1 <- sd_pred <- rep(NA, N)
      for ( k in 1:n_cases ) {
        
        ix <- which(df$caseID == k)
        jx <- intersect(labelled, ix)
        crashix <- intersect(which(df$impact_speed0 > 0), jx)
        
        if ( yvar == "impact_speed_reduction" ) {
          
          # Fit model.
          gp <- quietly(safe_gausspr_train)(impact_speed_reduction~z1+z2, data = df[jx, ], variance.model = TRUE)$result
          
          if ( !is.null(gp) && is.na(gp@error) ) {
            gp <- quietly(safe_gausspr_train)(impact_speed_reduction~z1+z2, data = df[jx, ], variance.model = TRUE)$result
          } 
          
          # Predictions. 
          if ( is.null(gp) || is.na(gp@error) ) {
            pred1[ix] <- rep(mean(Y[crashix]), length(ix))
            sd_pred[ix] <- 1
          } else {
            pred1[ix] <- predict(gp, df[ix, ])
            sd_pred[ix] <- predict(gp, df[ix, ], type = "sdeviation")
          }
          
        } else if ( yvar == "crash_avoidance" ) {
          
          # Fit model on crashes.
          gp <- quietly(safe_gausspr_train)(as.factor(crash_avoidance>0)~z1+z2, data = df[crashix, ])$result 
          
          if ( !is.null(gp) && is.na(gp@error) ) {
            gp <- quietly(safe_gausspr_train)(as.factor(crash_avoidance>0)~z1+z2, data = df[crashix, ])$result 
          } 
          
          # Predictions
          if ( is.null(gp) || is.na(gp@error) ) {
            pred1[ix] <- rep(mean(Y[crashix]), length(ix))
          } else {
            pred1[ix] <- predict(gp, df[ix, ], type = "probabilities")[, 2]
          }
          sd_pred[ix] <- sqrt(pred1[ix] * (1 - pred1[ix]))  
        } 
      }      
      
      if ( i == 1 & plot ) {
        
        plt <- df %>% 
          mutate(pred0 = pred0, 
                 pred1 = pred1, 
                 sd_pred = sd_pred)
        
        p1 <- ggplot(plt %>% filter(caseID <= 42), aes(x = eoff, y = acc, z = pred0)) + 
          geom_contour_filled() + 
          facet_wrap(~caseID) + 
          labs(title = sprintf("Pred0, iter = %d", j)) + 
          theme_classic()
        
        p2 <- ggplot(plt %>% filter(caseID <= 42), aes(x = eoff, y = acc, z = pred1)) + 
          geom_contour_filled() + 
          facet_wrap(~caseID) + 
          labs(title = sprintf("Pred1, iter = %d", j)) + 
          theme_classic()
        
        p3 <- ggplot(plt %>% filter(caseID <= 42), aes(x = eoff, y = acc, z = sd_pred)) + 
          geom_contour_filled() + 
          facet_wrap(~caseID) + 
          labs(title = sprintf("SD Pred, iter = %d", j)) + 
          theme_classic()
        
        print(p1)
        print(p2)
        print(p3)        
      }
      
      
      # Estimates and errors. 
      est <- sum( df$eoff_acc_prob * pred1 * pred0, na.rm = TRUE ) / sum( df$eoff_acc_prob * pred0, na.rm = TRUE )
      sqerr[i, j] <- (est - ground_truth)^2
      
      rm(est, gp, ix, jx, pi)
    }
    
    # Clean-up.
    rm(labelled, pred0, pred1, sd_pred)
  }
  
  t1 <- Sys.time()
  print(t1 - t0)
  
  res <- tibble(n = n_per_case_per_iter * n_cases * 1:niter, 
                mse = apply(sqerr, 2, mean, na.rm = TRUE), 
                sdmse = apply(sqerr, 2, sd, na.rm = TRUE),
                nreps = nreps)
  
  return(res)
  
}

n_per_case_per_iter <- 1
niter <- 46
nreps <- 1

set.seed(34235) # For reproducibility.
res1 <- run_experiments("impact_speed_reduction", n_per_case_per_iter, niter, nreps)

res1 %<>%
  plyr::rename(c("mse" = "mse_misr",
                 "sdmse" = "sdmse_misr"))

set.seed(5672) # For reproducibility.
res2 <- run_experiments("crash_avoidance", n_per_case_per_iter, niter, nreps)

res2 %<>%
  plyr::rename(c("mse" = "mse_car",
                 "sdmse" = "sdmse_car"))

# Combine.
res <- res1 %>%
  left_join(res2 %>% dplyr::select(-nreps), by = "n")

save(res, file = sprintf("Application/Results/result_%drepetitions_eRMSE_GPR.RData", nreps))
