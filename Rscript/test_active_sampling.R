
rm(list = ls())
library("tictoc")
load("Data/glance_dec_data_test.R")
# load("Data/test_prediction_100.RData")
source("RScript/active_sampling.R")
set.seed(123)
par(mfrow = c(1, 3))
cat("\14")

# data <- df
# sampling_method <- "active sampling"
# proposal_dist <- "NA"
# target <- "impact speed reduction"
# opt_method <- "naive"
# use_logic <- FALSE
# n_cases_per_iter <- 7
# niter <- 50
# verbose <- TRUE
# plot <- TRUE
# nboot <-  100
# i <- 1

for (i in 1:1) {
  tic()
  print(sprintf("%d", i))
  res <- active_sampling(df, 
                         sampling_method = "active sampling", 
                         proposal_dist = "NA",
                         target = "impact speed reduction", 
                         opt_method = "+ model uncertainty",
                         use_logic = FALSE,
                         n_cases_per_iter = 3, 
                         niter = 5, 
                         verbose = TRUE, 
                         plot = TRUE, 
                         nboot = 100)
  toc()
}