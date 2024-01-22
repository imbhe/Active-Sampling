rm(list = ls())

source("Application/RScript/active_sampling.R")
load("Application/Data/Data.R")
cat("\14")

run_experiments <- function(target, nreps = 100, bsize = 100, niter = 10) {
  
  sqerr_misr <- sqerr_car <- matrix(NA, nrow = nreps, ncol = niter)
  
  print(target)
  
  for ( i in 1:nreps ) {
    print(sprintf("i = %d", i))
    res <- active_sampling(df, 
                           sampling_method = "active sampling", 
                           proposal_dist = "NA",
                           target = target, 
                           opt_method = "default",
                           prediction_model_type = "gp",
                           batch_size = bsize, 
                           niter = niter, 
                           verbose = FALSE, 
                           plot = FALSE, 
                           nboot = 0)
    
    sqerr_misr[i, ] <- res$results$mean_impact_speed_reduction_sqerr
    sqerr_car[i, ] <- res$results$crash_avoidance_rate_sqerr
  }
  
  res <- tibble(n = seq(bsize, bsize * niter, bsize), 
                target = target,
                mse_misr = apply(sqerr_misr, 2, mean, na.rm = TRUE), 
                sdmse_misr = apply(sqerr_misr, 2, sd, na.rm = TRUE),
                mse_car = apply(sqerr_car, 2, mean, na.rm = TRUE), 
                sdmse_car = apply(sqerr_car, 2, sd, na.rm = TRUE),
                nreps = nreps)
  
}

set.seed(962455) # For reproducibility.
res1 <- run_experiments("impact speed reduction")

set.seed(842) # For reproducibility.
res2 <- run_experiments("crash avoidance")

res <- rbind(res1, res2)
res$prediction_model_type = "GP"

save(res, file = "Application/Results/result_100groups_eRMSE_AS_GP.RData")
