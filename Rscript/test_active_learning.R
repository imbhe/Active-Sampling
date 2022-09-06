
rm(list = ls())
library("tictoc")
load("Data/glance_dec_data_test.R")
# load("Data/test_prediction_100.RData")
source("RScript/active_learning.R")
set.seed(123)

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_learning(df, 
                         sampling_method = "optimised", 
                         proposal_dist = "NA",
                         target = "crash avoidance", 
                         use_logic = TRUE,
                         n_cases_per_iter = 10, 
                         niter = 10, 
                         verbose = TRUE, 
                         plot = TRUE, 
                         nboot = 0)
  toc()
}