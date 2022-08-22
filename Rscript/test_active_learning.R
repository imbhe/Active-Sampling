
library("tictoc")
load("Data/glance_dec_data_test.R")
source("RScript/active_learning.R")
set.seed(123)

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_learning(df, 
                         sampling_method = "optimised", 
                         proposal_dist = "propto eoff_acc_prob * eoff * abs(acc) * maximpact0",
                         target = "crash avoidance", 
                         reduce_simulations_by_logic = TRUE,
                         num_cases_per_iteration = 2, 
                         nburnin = 2, 
                         niter = 15, 
                         verbose = TRUE, 
                         plot = FALSE, 
                         nboot = 0)
  toc()
}