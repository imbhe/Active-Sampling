
rm(list = ls())
library("tidyverse")

source("Application/RScript/active_sampling.R")

load("Application/Data/Data.R")
set.seed(123)
cat("\14")

source("Application/Rscript/calculate_sampling_scheme.R")
source("Application/Rscript/estimate_targets.R")
source("Application/Rscript/estimate_totals.R")
source("Application/Rscript/initialise_grid.R")
source("Application/Rscript/safe_caret_train.R")
source("Application/Rscript/safe_gausspr_train.R")
source("Application/Rscript/update_predictions.R")

data <- df
grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
init <- initialise_grid(data, grid)
labelled <- init$labelled 
unlabelled <- init$unlabelled 
