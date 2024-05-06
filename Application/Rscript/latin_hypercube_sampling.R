
library("lhs")
library("magrittr")
library("MatchIt")
library("tidyverse")

rm(list = ls())

load("Application/Data/Data.R")

source("Application/Rscript/estimate_targets.R")

generate_lhd <- function(df, Z, n_cases, n_per_case, p) {
  labelled <- NULL
  for ( k in 1:n_cases ) {
    D <- as.data.frame(randomLHS(n_per_case, p))
    colnames(D) <- paste0("z", 1:p)
    
    ix <- which(df$caseID == k)
    dftmp <- df[ix, ]
    Ztmp <- as.data.frame(Z[ix, ])
    
    D$design <- 1
    Ztmp$design <- 0
    dta <- rbind(D, Ztmp)
    m <- matchit(design~z1+z2, data = dta, distance = "scaled_euclidean")
    
    ix <- as.numeric(m$match.matrix) - nrow(D)
    labelled <- rbind(labelled, dftmp[ix, ])
  }
  return(labelled)
}

run_experiments <- function(n_per_case = seq(1, 50, 1), nreps = 100) {
  
  t0 <- Sys.time()

  df %<>% 
    mutate(z1 = eoff / max(eoff),
           z2 = -acc,
           z2 = (z2 - min(z2)) / diff(range(z2)),
           caseID = as.numeric(caseID))

  p <- 2
  Z <- as.matrix(df[, paste0("z", 1:p)])

  ground_truth <- estimate_targets(df, weightvar = "eoff_acc_prob") 
  sqerr_misr <- sqerr_car <- matrix(NA, nrow = nreps, ncol = length(n_per_case))
  n_cases <- length(unique(df$caseID))
  
  for ( i in 1:nreps ) {
    # print(sprintf("i = %d", i))
    
    for ( j in seq_along(n_per_case) )  {

      labelled <- generate_lhd(df, Z, n_cases, n_per_case[j], p)
      est <- estimate_targets(labelled, weightvar = "eoff_acc_prob")
      
      sqerr <- (est - ground_truth)^2
      sqerr_misr[i, j] <- sqerr["mean_impact_speed_reduction"]
      sqerr_car[i, j] <- sqerr["crash_avoidance_rate"]
    }
  }
  
  t1 <- Sys.time()
  print(t1 - t0)
  
  res <- tibble(n = n_per_case * n_cases, 
                mse_misr = apply(sqerr_misr, 2, mean, na.rm = TRUE), 
                sdmse_misr = apply(sqerr_misr, 2, sd, na.rm = TRUE) ,
                mse_car = apply(sqerr_car, 2, mean, na.rm = TRUE), 
                sdmse_car = apply(sqerr_car, 2, sd, na.rm = TRUE),
                nreps = nreps)
  
  return(res)
}

set.seed(92375) # For reproducibility. 
res <- run_experiments(nreps = 500)
print(res)
save(res, file = "Application/Results/result_500groups_eRMSE_LHS.RData")