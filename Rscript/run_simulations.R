library("readxl")
source("Rscript/active_sampling.R")
source("Rscript/call_active_sampling.R")
load("Data/Data.R")
sampling_input <- read_excel("Data/sampling_method_input_coverage_example.xlsx")
param_input <- read_excel("Data/parameter_input.xlsx")
add_effn = 500
niter = ceiling(add_effn/param_input$batch_size)
res_total= replicate(length(param_input$Sim_n), data.frame())

for(j in 1:length(param_input$Sim_n)){
  inputparameter <- data.frame(param_input$batch_size[j],niter[j],param_input$nboot[j],param_input$Sim_n[j])
  colnames(inputparameter) <- c("batch_size","niter", "nboot","Sim_n")
  res_top_loop = replicate(tail(sampling_input$sim_order, 1), data.frame())
  
  for(i in 1:tail(sampling_input$sim_order, 1)){
    res_in_loop <- call_active_sampling(df,sampling_input[i,],inputparameter)
    print(paste("sampling method",i,sampling_input$group[i],"is finished"))
    res_top_loop[[i]] <- res_in_loop
  }
  print(paste("loop",j,"is finished"))
  res_total[[j]] <- res_top_loop
}  

save(res_total, file = "Output/result2.R")

