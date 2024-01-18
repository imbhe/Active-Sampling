##################################################
## Project: Active Sampling
## Description: Active sampling experiments on simulated data. 
## Date: 9 Jan 2024
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())
gc()


# Load packages -----------------------------------------------------------

library("kernlab")
library("mboost")
library("mgcv")
library("ranger")
library("tidyverse")


# Load functions ----------------------------------------------------------

source("Simulation experiments/Rscript/functions.R")


# Active sampling ---------------------------------------------------------

# Set parameters.
nreps <- 500 # Number of simulation experiments. 
nmax <- 250 # Maximal subsample size.

# SRS and PPS.
params1 <- crossing(model = c("const", "pps"),
                    naive = FALSE,
                    bsize = 10, 
                    estimator = c("default", "Hajek")) 

# LM and GAM.
params2 <- tibble(model = c("lm", "gam")) %>% # Auxiliary models. 
  crossing(naive = c(TRUE, FALSE), # Naive implementation of active sampling? Ignores prediction if this is TRUE.
           bsize = c(10, 50), # Small or large batch size. 
           estimator = c("default", "Hajek")) # Estimator for the finite population mean. 

# Remaining ML models.
params3 <- crossing(model = c("rf", "gbt", "gpr"), 
                    naive = FALSE, 
                    bsize = 50, 
                    estimator = c("default", "Hajek")) 

# All experiments. 
params <- params1 %>% 
  add_row(params2) %>%
  add_row(params3) %>%
  mutate(niter = nmax / bsize) %>% # Number of iterations. 
  crossing(datafile = list.files("Simulation experiments/Data", ".RData")) # Input data sets. 

rm(nmax)


# Experiments.
for ( i in 1:nrow(params) ) {
  
  # Extract bandwidth and R2 of true signal from filename. 
  file <- unlist(params$datafile[i])
  splt <- unlist(strsplit(gsub("[^[:alnum:] ]", "&", file), "&"))
  bandwidth <- as.numeric(splt[3]) + as.numeric(splt[4]) / 100 
  r2 <- as.numeric(splt[6]) + as.numeric(splt[7]) / 100 
  normalization <- sprintf("%s_%s", splt[8], splt[9])
  rm(splt, file)

  # Load data.
  load(sprintf("Simulation experiments/Data/SimData_Bandwidth_%.2f_R2_%.2f_%s.RData", bandwidth, r2, normalization))
    
  # To store results. 
  sqerr <- matrix(NA, nrow = nreps, ncol = params$niter[i]) 
  if ( params$model[i] ==  "const" ) {
    sqerr_cv <- sqerr_ratio <- matrix(NA, nrow = nreps, ncol = params$niter[i]) 
  }
  
  # Run repeated subsampling experiments. 
  for ( j in 1:nreps ) {
    
    # Active sampling.
    res <- active_sampling(data = dta, 
                           ninit = params$bsize[i], 
                           bsize = params$bsize[i], 
                           niter = params$niter[i], 
                           model = params$model[i],
                           naive = params$naive[i],
                           estimator = params$estimator[i]) 
    
    # Store results 
    sqerr[j, ] <- res$sqerr
    if ( params$model[i] ==  "const" ) {
      sqerr_cv[j, ] <- res$sqerr_cv
      sqerr_ratio[j, ] <- res$sqerr_ratio
    }
    
    rm(res)

  }
  
  # Combine results.
  res <- tibble(bandwidth = bandwidth, # Gaussian bandwidth, true signal.
                r2 = r2, # R2, true signal.
                normalization = normalization, # Normalization, true signal (zero mean or support of positive real line)
                model = params$model[i], # Auxiliary model.
                bsize = params$bsize[i], # Batch size.
                naive = params$naive[i], # Implementation of active sampling (naive or not).
                estimator = params$estimator[i], # Implementation of active sampling (naive or not).
                n = cumsum(seq(params$bsize[i], params$bsize[i], length.out = params$niter[i])), # Cumulative sample size.
                mse = apply(sqerr, 2, mean), # Mean squared error.
                sd_mse = apply(sqerr, 2, sd), # Standard deviation of the MSE.
                nreps = nreps) # Number of simulations/repetitions.
  
  if ( params$model[i] == "const" ) {
    res$mse_cv <- apply(sqerr_cv, 2, mean)
    res$sd_mse_cv <- apply(sqerr_cv, 2, sd)
    res$mse_ratio <- apply(sqerr_ratio, 2, mean)
    res$sd_mse_ratio <- apply(sqerr_ratio, 2, sd)  
  }
  
  # Save. 
  save(res, file = sprintf("Simulation experiments/Results/Bandwidth_%s_R2_%s_%s_Model_%s_BatchSize_%s_Naive_%s_estimator_%s.RData", 
                           bandwidth,
                           r2, 
                           normalization, 
                           params$model[i],
                           params$bsize[i],
                           params$naive[i],
                           params$estimator[i]))
  
  rm(bandwidth, dta, normalization, res, r2, sqerr)
  gc()
  
}

# Clean-up.
rm(i, j, nreps, params, params1, params2, params3)