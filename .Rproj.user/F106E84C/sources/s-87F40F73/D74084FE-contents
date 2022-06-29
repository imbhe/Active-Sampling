# short explaination for the code 
# num_cases_per_iteration is limited by the real case number
library("readxl")
source("Rscript/active_learning.R")
#library("tidyverse")
source("Rscript/sim_output.R")
load("Data/glance_dec_data_test.R")

sampling_input <- read_excel("Data/Sampling_input2.xlsx")
# have another table input for simulation number choice
param_input <- read_excel("Data/param_input.xlsx")
add_effn = 300
nburnin = 1
verbose = FALSE

total_iter = ceiling(add_effn/param_input$num_cases_per_iteration)
res_total= replicate(length(param_input$Sim_n), data.frame())


for(j in 1:length(param_input$Sim_n)){
  inputparameter <- data.frame(param_input$num_cases_per_iteration[j],total_iter[j], nburnin,param_input$total_nboot[j],verbose,param_input$Sim_n[j])
  colnames(inputparameter) <- c("num_cases_per_iteration","total_iter", "nburnin","total_nboot","verbose","Sim_n")
  set.seed(1)
  res_top_loop = replicate(tail(sampling_input$sim_order, 1), data.frame())
  
  for(i in 1:tail(sampling_input$sim_order, 1)){
    set.seed(1)
    res_in_loop <- sim_output(df,sampling_input[i,],inputparameter)
    print(paste("sampling method",i,"is finished"))
    res_top_loop[[i]] <- res_in_loop
  }
  print(paste("loop",j,"is finished"))
  res_total[[j]] <- res_top_loop
}  
  
save(res_total, file = "Data/res_analysis_data_test.R")


 