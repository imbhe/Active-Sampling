
library("magrittr")
library("tidyverse")

rm(list = ls())

load("Application/Data/Data.R")

source("Application/Rscript/estimate_targets.R")
source("Application/Rscript/maximin.cand.R")

run_experiments <- function(method) {
  
  t0 <- Sys.time()

  df %<>% 
    mutate(z1 = eoff / max(eoff),
           z2 = -acc,
           z2 = (z2 - min(z2)) / diff(range(z2)), 
           z3 = (impact_speed_max0 - min(impact_speed_max0)) / diff(range(impact_speed_max0)))
  
  Z <- as.matrix(df[, paste0("z", 1:3)])
  ground_truth <- estimate_targets(df, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  nreps <- 100
  nseq <- seq(500, 2000, 500)
  N <- nrow(Z)
  p <- ncol(Z)
  sqerr_misr <- sqerr_car <- matrix(NA, nrow = nreps, ncol = length(nseq))
  
  for ( i in 1:nreps ) {
    print(sprintf("i = %d", i))
    
    for ( j in seq_along(nseq) )  {

      n <- nseq[j]
      D <- randomLHS(n, p) 
      
      if ( method == "match" ) {
        D <- as.data.frame(D)
        colnames(D) <- paste0("z", 1:3)
        D$trt <- 1
        Z <- as.data.frame(Z)
        Z$trt <- 0
        dta <- rbind(D, Z)
        m <- matchit(trt~z1+z2+z3, data = dta, distance = "mahalanobis")
        ix <- as.numeric(m$match.matrix) - nrow(D)
  
      } else if ( method == "maximin") {
        ix <- maximin.cand(n=n, Xcand=Z, Tmax=N, Xorig=D)$inds
      } 
      
      labelled <- df[ix, ]
      est <- estimate_targets(labelled, weightvar = "eoff_acc_prob")
      
      sqerr <- (est - ground_truth)^2
      sqerr_misr[i, j] <- sqerr["mean_impact_speed_reduction"]
      sqerr_car[i, j] <- sqerr["crash_avoidance_rate"]
    }
  }
  
  t1 <- Sys.time()
  
  res <- tibble(n = nseq, 
                mse_misr = apply(sqerr_misr, 2, mean), 
                sdmse_misr = apply(sqerr_misr, 2, sd) ,
                mse_car = apply(sqerr_car, 2, mean), 
                sdmse_car = apply(sqerr_car, 2, sd),
                nreps = nreps)
  
  return(res)
}

res <- run_experiments(method = "match")
save(res, file = "Application/Results/result_500groups_eRMSE_LHD.RData")

res <- run_experiments(method = "maximin")
save(res, file = "Application/Results/maximin.RData")

