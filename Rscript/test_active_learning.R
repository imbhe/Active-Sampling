
rm(list = ls())
library("tictoc")
load("Data/glance_dec_data_test.R")
# load("Data/test_prediction_100.RData")
source("RScript/active_learning.R")
set.seed(123)
par(mfrow = c(1, 3))
cat("\14")

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_learning(df, 
                         sampling_method = "importance sampling", 
                         proposal_dist = "pps, size = prior weight",
                         target = "NA", 
                         use_logic = TRUE,
                         n_cases_per_iter = 5, 
                         niter = 50, 
                         verbose = TRUE, 
                         plot = TRUE, 
                         nboot = 100)
  toc()
}