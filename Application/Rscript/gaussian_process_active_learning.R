library("kernlab")
library("magrittr")
library("purrr")
library("tidyverse")

rm(list = ls())

load("Application/Data/Data.R")

source("Application/Rscript/estimate_targets.R")
source("Application/Rscript/safe_gausspr_train.R")

run_experiments <- function(yvar, nseq = seq(250, 2000, 250), nreps = 2) {
  
  t0 <- Sys.time()
  
  df %<>% 
    mutate(z1 = eoff / max(eoff),
           z2 = -acc,
           z2 = (z2 - min(z2)) / diff(range(z2)), 
           z3 = (impact_speed_max0 - min(impact_speed_max0)) / diff(range(impact_speed_max0)))
  
  Z <- as.matrix(df[, paste0("z", 1:3)])
  Y <- df[, yvar, drop = TRUE]
  p <- ncol(Z)
  
  # Calculate target quantities on full data.
  ground_truth <- estimate_targets(df, weightvar = "eoff_acc_prob") 
  if ( yvar == "impact_speed_reduction" ) {
    ground_truth <- ground_truth["mean_impact_speed_reduction"]
  } else if ( yvar == "crash_avoidance") {
    ground_truth <- ground_truth["crash_avoidance_rate"]
  }
  
  sqerr <- matrix(NA, nrow = nreps, ncol = length(nseq))
  
  for ( i in 1:nreps ) {
    print(sprintf("i = %d", i))
  
    sd_pred <- rep(1, nrow(Z)) 
    labelled <- NULL
    
    for ( j in seq_along(nseq) )  {
      print(sprintf("j = %d", j))
      
      if ( j == 1 ) {
        n <- nseq[j]
      } else {
        n <- diff(nseq[(j-1):j])
      }
      
      # Probabilistic uncertainty sampling. 
      pi <- sd_pred
      ix <- which(rmultinom(1, n, pi) == 1)
      labelled <- unique(union(labelled, ix))

      # Fit Gaussian process regression model for probability of collision in baseline scenario.
      gp0 <- quietly(safe_gausspr_train)(as.factor(impact_speed0>0)~z1+z2+z3, data = df[labelled, ])$result

      # Predictions. 
      if ( is.null(gp0) ) {
        pred0 <- rep(1, nrow(Z))
      } else {
        pred0 <- predict(gp0, df, type = "probabilities")[, 2]
      }
      
      # Fit model to target variable impact speed reduction or crash avoidance. 
      if ( yvar == "impact_speed_reduction" ) {
        gp1 <- quietly(safe_gausspr_train)(impact_speed_reduction~z1+z2+z3, data = df[labelled, ], variance.model = TRUE)$result 
  
        # Predictions
        if ( is.null(gp1) ) {
          pred1 <- rep(mean(Y[labelled]), nrow(Z))
        } else {
          pred1 <- predict(gp1, df)
          inc <- nrow(Z) / 20 # Do iteratively to reduce computation time. 
          for ( k in 1:20 ) {
            start <- (k - 1) * inc + 1
            end <- start + inc - 1
            sd_pred[start:end] <- predict(gp1, df[start:end, ], type = "sdeviation")
          }
        }
        
      } else if ( yvar == "crash_avoidance" ) {
        
        # Fit model.
        gp1 <- quietly(safe_gausspr_train)(as.factor(crash_avoidance>0)~z1+z2+z3, data = df[labelled, ])$result 
        
        # Predictions
        if ( is.null(gp1) ) {
          pred1 <- rep(0.5, nrow(Z))
        } else {
          pred1 <- predict(gp1, df, type = "probabilities")[, 2]
          sd_pred <- sqrt(pred1 * (1 - pred1))  
        }
      }
      
      # Estimates and errors. 
      est <- sum( df$eoff_acc_prob * pred1 * pred0 ) / sum(df$eoff_acc_prob * pred0)
      sqerr[i, j] <- (est - ground_truth)^2
      
      rm(est)
    }
    
    # Clean-up.
    rm(n, pi, ix, labelled, gp1, gp0, pred0, pred1, sd_pred)
  }
  
  t1 <- Sys.time()
  print(t1 - t0)
  
  res <- tibble(n = nseq, 
                mse = apply(sqerr, 2, mean, na.rm = TRUE), 
                sdmse = apply(sqerr, 2, sd, na.rm = TRUE),
                nreps = nreps)
  
  return(res)
  
}

set.seed(1472) # For reproducibility.
res1 <- run_experiments(yvar = "impact_speed_reduction")

res1 %<>%
  plyr::rename(c("mse" = "mse_misr",
                 "sdmse" = "sdmse_misr"))

set.seed(6921) # For reproducibility.
res2 <- run_experiments(yvar = "crash_avoidance")

res2 %<>%
  plyr::rename(c("mse" = "mse_car",
                 "sdmse" = "sdmse_car"))

# Combine. 
res <- res1 %>% 
  left_join(res2 %>% dplyr::select(-nreps), by = "n")

save(res1, file = "Application/Results/result_50groups_eRMSE_GPR_misr.RData")
