
load("Data/glance_dec_data_test.R")
source("RScript/active_learning.R")

for (i in 1:1) {
  print(sprintf("%d", i))
  res <- active_learning(df, 
                         sampling_method = "optimised", 
                         target = "baseline impact speed distribution", 
                         nburnin = 2, 
                         niter = 10, 
                         verbose = TRUE, 
                         plot = TRUE, 
                         nboot = 0)
}
