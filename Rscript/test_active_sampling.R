rm(list = ls())

source("RScript/active_sampling.R")
load("Data/glance_dec_data_test.R")
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
                       plot = TRUE, 
                       nboot = 0)