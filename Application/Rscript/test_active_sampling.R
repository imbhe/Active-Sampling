rm(list = ls())

source("Application/RScript/active_sampling.R")
load("Application/Data/Data.R")
set.seed(123)
cat("\14")

t0 <- Sys.time()
res <- active_sampling(df, 
                       sampling_method = "active sampling", 
                       proposal_dist = "NA",
                       target = "impact speed reduction", 
                       opt_method = "default",
                       prediction_model_type = "gp",
                       batch_size = 10, 
                       niter = 200, 
                       verbose = TRUE, 
                       plot = FALSE, 
                       nboot = 0)
t1 <- Sys.time()
diff <- t1 - t0
print(diff)