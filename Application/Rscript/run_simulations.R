library("readxl")
# source the sampling scripts
source("Application/Rscript/active_sampling.R")

# load the experiment data
load("Application/Data/Data.R")
# load which sampling methods to run
sampling_input <- read_excel("Application/Input/sampling_method_input_rmse_example.xlsx")

# load how many simulations to run
param_input <- read_excel("Application/Input/parameter_input.xlsx")
max_sample_size = 200
niter = ceiling(max_sample_size/param_input$batch_size)
res_total= replicate(length(param_input$n_repetition), data.frame())
prediction_model_type = "rg" # "xg_boost" "rg" "knn" "Gaussian"
for(j in 1:length(param_input$n_repetition)){
  inputparameter <- data.frame(param_input$batch_size[j],niter[j],param_input$nboot[j],param_input$n_repetition[j])
  colnames(inputparameter) <- c("batch_size","niter", "nboot","n_repetition")
  res_top_loop = replicate(tail(sampling_input$sim_order, 1), data.frame())
  for(i in 1:tail(sampling_input$sim_order, 1)){
    res_list <- replicate(inputparameter$n_repetition, data.frame())
    for (k in 1:inputparameter$n_repetition){
      print(paste("Sampling method",sampling_input[i,]$group,",simulation",k,"starts,","number per iteration:",
                  inputparameter$batch_size,",total iteration:",inputparameter$niter))
      set.seed(k)
      out <- active_sampling (df, 
                              sampling_input[i,]$sampling_method, 
                              sampling_input[i,]$proposal_dist,
                              sampling_input[i,]$target, 
                              sampling_input[i,]$opt_method,
                              inputparameter$batch_size,
                              inputparameter$niter,
                              inputparameter$nboot,
                              verbose = FALSE,
                              plot = FALSE,
                              prediction_model_type)
      res_list[[k]] <- out$results
    }
    res <- do.call(rbind, res_list)
    res_in_loop <- list(res = res)
    print(paste("sampling method",i,sampling_input$group[i],"is finished"))
    res_top_loop[[i]] <- res_in_loop
  }
  print(paste("loop",j,"is finished"))
  res_total[[j]] <- res_top_loop
}  

save(res_total, file = "Application/Results/test.R")

