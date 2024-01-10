##################################################
## Project: Active Sampling
## Description: Simulate datasets
## Date: 9 Jan 2024
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())
gc()

# Load functions. 
source("Simulation experiments/Rscript/functions.R")

# Load packages
library("MASS")
library("stringr")

# Set parameters. 
N <- 1e3
bandwidth <- c(0.1, 1, 10) # Linear, polynomial, or non-linear signal.
r2 <- c(0.1, 0.5, 0.75, 0.9) # Very weak, moderate, strong, or very strong signal.
normalization <- c("zero mean", "strictly positive")

# Simulate data. 
for ( i in seq_along(bandwidth) ) {
  for ( j in seq_along(r2) ) {
    for ( k in seq_along(normalization) ) {
      
      # Set seed, for reproducibility. 
      set.seed(1351)
      
      # Simulate. 
      dta <- sim_data(N = N, 
                      bandwidth = bandwidth[i],
                      r2 = r2[j], 
                      normalization = normalization[k]) 
      
      # Plot.
      png(sprintf("Simulation experiments/Data/SimData_Bandwidth_%.2f_R2_%.2f_%s.png", bandwidth[i], r2[j], str_replace(normalization[k], " ", "_")), width = 640, height = 480, unit = "px")
      plot(dta$z, dta$y, bty = "l", main = sprintf("sigma = %.2f, r2 = %.2f, ", bandwidth[i], r2[j]))
      lines(dta$z, dta$yhat)
      dev.off()
      
      # Drop column yhat.
      dta$yhat <- NULL 
      
      # Save. 
      save(file = sprintf("Simulation experiments/Data/SimData_Bandwidth_%.2f_R2_%.2f_%s.RData", bandwidth[i], r2[j], str_replace(normalization[k], " ", "_")), dta)
    }
  }
}