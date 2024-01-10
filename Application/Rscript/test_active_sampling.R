rm(list = ls())

source("Application/RScript/active_sampling.R")
load("Application/Data/Data.R")
set.seed(123)
cat("\14")

res <- active_sampling(df, 
                       sampling_method = "active sampling", 
                       proposal_dist = "NA",
                       target = "crash avoidance", 
                       opt_method = "naive",
                       batch_size = 2, 
                       niter = 10, 
                       verbose = TRUE, 
                       plot = FALSE, 
                       nboot = 0)
