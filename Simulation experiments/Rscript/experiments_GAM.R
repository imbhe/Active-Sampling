##################################################
## Project: Active Sampling
## Description: Active sampling experiments on simulated data. Constant, linear and generalized additive model.
## Date: 9 Jan 2024
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())
gc()


# Load packages -----------------------------------------------------------

# library("kernlab")
# library("mboost")
library("mgcv")
# library("ranger")
library("tidyverse")


# Load functions ----------------------------------------------------------

source("Simulation experiments/Rscript/functions.R")


# Active sampling ---------------------------------------------------------

# Set parameters.
nreps <- 500 # Number of simulation experiments. 
nmax <- 250 # Maximal subsample size.
params <- tibble(model = c("const", "lm", "gam")) %>% # Auxiliary models. 
  crossing(datafile = list.files("Simulation experiments/Data", ".RData"), # Input data sets. 
           naive = c(TRUE, FALSE), # Ignore prediction uncertainty (TRUE) or account for prediction uncertainty (FALSE).
           bsize = c(10, 50), # Small or large batch size. 
           estimator = c("default", "Hajek")) %>% # Estimator for the finite population mean. 
  mutate(niter = nmax / bsize)

rm(nmax)


# Experiments.
for ( i in 321:nrow(params) ) {
  
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
rm(i, j, nreps, params)
