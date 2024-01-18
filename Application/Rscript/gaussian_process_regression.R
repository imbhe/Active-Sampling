library("laGP")
library("lhs")
library("kernlab")
library("magrittr")
library("MatchIt")
library("tidyverse")

rm(list = ls())

load("Application/Data/Data.R")

source("Application/Rscript/estimate_targets.R")

run_experiments <- function(yvar, nseq = c(seq(10, 100, 10), seq(200, 2000, 200)), nreps = 50) {
  
  t0 <- Sys.time()
  
  df %<>% 
    mutate(z1 = eoff / max(eoff),
           z2 = -acc,
           z2 = (z2 - min(z2)) / diff(range(z2)), 
           z3 = (impact_speed_max0 - min(impact_speed_max0)) / diff(range(impact_speed_max0)))
  
  Z <- as.matrix(df[, paste0("z", 1:3)])
  Y <- df[, yvar, drop = TRUE]
  p <- ncol(Z)
  ymax <- max(Y)
  
  # Calculate target quantities on full data.
  ground_truth <- estimate_targets(df, weightvar = "eoff_acc_prob") 
  if ( yvar == "impact_speed_reduction" ) {
    ground_truth <- ground_truth["mean_impact_speed_reduction"]
  } else if ( yvar == "crash_avoidance") {
    ground_truth <- ground_truth["crash_avoidance_rate"]
  }
  sqerr <- matrix(NA, nrow = nreps, ncol = length(nseq))
  
  sd_pred <- 1 
  
  labelled <- NULL
  
  for ( i in 1:nreps ) {
    print(sprintf("i = %d", i))
    
    for ( j in seq_along(nseq) )  {
      print(sprintf("j = %d", j))
      
      if ( j == 1 ) {
        n <- nseq[j]
      } else {
        n <- diff(nseq[(j-1):j])
      }
      
      # Probabilistic uncertainty sampling. 
      pi <- df$eoff_acc_prob * sd_pred
      ix <- which(rmultinom(1, n, pi) == 1)
      labelled <- unique(union(labelled, ix))
      
      # Fit Gaussian process regression model.
      if ( j == 1) { 
        g <- garg(list(mle=TRUE, max=ymax), Y[ix])
        d <- darg(list(mle=TRUE, max=1), Z[ix, ])
        gp1 <- newGP(Z[ix, ], Y[ix], d=d$start, g=g$start, dK=TRUE)
        mle1 <- jmleGP(gp1, c(d$min, d$max), c(g$min, g$max), d$ab, g$ab)
      } else { 
        updateGP(gp1, Z[ix, ], Y[ix])
      }
      
      gp0 <- gausspr(as.factor(impact_speed0>0)~z1+z2+z3, data = df[labelled, ])
      
      # Predictions. 
      pred0 <- predict(gp0, df, type = "probabilities")[, 2]
      pred1 <- predGP(gp1, Z, lite=TRUE)
      pred1$mean <- pmax(0, pred1$mean) # Know benefit cannot be smaller than 0. 
      sd_pred <- sqrt(pred1$s2)
      
      # Estimates and errors. 
      est <- sum( df$eoff_acc_prob * pred1$mean * pred0 ) / sum(df$eoff_acc_prob * pred0)
      sqerr[i, j] <- (est - ground_truth)^2
    }
  }
  
  t1 <- Sys.time()
  print(t1 - t0)
  
  res <- tibble(n = nseq, 
                mse = apply(sqerr, 2, mean, na.rm = TRUE), 
                sdmse = apply(sqerr, 2, sd, na.rm = TRUE),
                nreps = nreps)
  
  return(res)
  
}

res1 <- run_experiments(yvar = "impact_speed_reduction")
res2 <- run_experiments(yvar = "crash_avoidance")

res1 %<>%
  plyr::rename(c("mse" = "mse_misr",
                 "sdmse" = "sdmse_misr"))

res2 %<>%
  plyr::rename(c("mse" = "mse_car",
                 "sdmse" = "sdmse_car"))

res <- res1 %>% 
  left_join(res2 %>% dplyr::select(-nreps), by = "n")

save(res, file = "Application/Results/result_500groups_eRMSE_GPR.RData")
