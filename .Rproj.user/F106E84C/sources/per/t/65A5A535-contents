# Question: where is the seed used?
source("Rscript/active_learning.R")
library("tidyverse")
library("ggplot2")
plotSingle = TRUE
load("Data/glance_dec_data_test.R")
sampling_method = c("uniform", 
                    "propto eoff_acc_prob", 
                    "propto eoff_acc_prob * eoff * maximpact0", 
                    "propto eoff_acc_prob * abs(acc) * maximpact0", 
                    "propto eoff_acc_prob * eoff * abs(acc) * maximpact0", 
                    "optimised") 
target = c("none", 
           "baseline impact speed distribution", 
           "impact speed reduction", 
           "crash avoidance",
           "injury risk reduction", 
           "injury risk reduction, stratified")
fcount = 1
total_iter = 500
total_nboot = 1
Sim_n = 100
if (plotSingle){
  for ( j in c(TRUE,FALSE)){
    for (i in target[2:6]){
      res_list <- replicate(Sim_n, data.frame())
      for (k in 1:Sim_n){
      set.seed(k)
      out <- active_learning (df, sampling_method = c("optimised"), 
                              target = c(i), 
                              reduce_simulations_by_logic = j, # TRUE or FALSE 
                              num_cases_per_iteration = 1,
                              niter = total_iter, 
                              nboot = total_nboot)
      res_list[[k]] <- out$results
      }
      res = do.call(rbind, res_list)
      aver = as.data.frame(matrix(nrow=total_iter,ncol=0))
      biggest = as.data.frame(matrix(nrow=total_iter,ncol=0))
      smallest = as.data.frame(matrix(nrow=total_iter,ncol=0))
      index = matrix(nrow = total_iter,ncol = Sim_n)
      for(p in 1:total_iter){
        index[p,] = seq(p,total_iter*Sim_n,total_iter)
        aver$iter[p] = mean(res$iter[p])
        aver$mean_impact_speed0_sqerr[p] = mean(res$mean_impact_speed0_sqerr[index[p,]])
        aver$absolute_impact_speed_reduction_sqerr[p] = mean(res$absolute_impact_speed_reduction_sqerr[index[p,]])
        aver$proportion_crashes_avoided_sqerr[p] = mean(res$proportion_crashes_avoided_sqerr[index[p,]])
        aver$absolute_injury_risk_reduction_sqerr[p] = mean(res$absolute_injury_risk_reduction_sqerr[index[p,]])
        
        biggest$iter[p] = mean(res$iter[p])
        biggest$mean_impact_speed0_sqerr[p] = max(res$mean_impact_speed0_sqerr[index[p,]])
        biggest$absolute_impact_speed_reduction_sqerr[p] = max(res$absolute_impact_speed_reduction_sqerr[index[p,]])
        biggest$proportion_crashes_avoided_sqerr[p] = max(res$proportion_crashes_avoided_sqerr[index[p,]])
        biggest$absolute_injury_risk_reduction_sqerr[p] = max(res$absolute_injury_risk_reduction_sqerr[index[p,]])
        
        smallest$iter[p] = mean(res$iter[p])
        smallest$mean_impact_speed0_sqerr[p] = min(res$mean_impact_speed0_sqerr[index[p,]])
        smallest$absolute_impact_speed_reduction_sqerr[p] = min(res$absolute_impact_speed_reduction_sqerr[index[p,]])
        smallest$proportion_crashes_avoided_sqerr[p] = min(res$proportion_crashes_avoided_sqerr[index[p,]])
        smallest$absolute_injury_risk_reduction_sqerr[p] = min(res$absolute_injury_risk_reduction_sqerr[index[p,]])
      }
      
      if (i == target[2]){
        out_param_val = aver$mean_impact_speed0_sqerr
        out_param = "Baseline impact speed sd"
      }else if(i == target[3]){
        out_param_val = aver$absolute_impact_speed_reduction_sqerr
        out_param = "Impact speed reduction sd"
      }else if(i == target[4]){
        out_param_val = aver$proportion_crashes_avoided_sqerr
        out_param = "Proportion_crashes_avoided_sqerr"
      }else if(i == target[5]){
        out_param_val = aver$absolute_injury_risk_reduction_sqerr
        out_param = "Injury risk reduction sd"
      }else if(i == target[6]){
        out_param_val = aver$absolute_injury_risk_reduction_sqerr
        out_param = "Injury risk reduction sd"
      }
      g1 <- ggplot(aver, aes(x = iter, y = sqrt(mean_impact_speed0_sqerr))) +
        geom_point(color = "firebrick", shape = "diamond", size = 2)+
        labs(x = "Iteration", y = "Baseline impact speed sd")+
        theme(axis.title.x = element_text(vjust = 0, size = 15),
              axis.title.y = element_text(vjust = 2, size = 15),
              axis.text = element_text(size = 12))+
        ggtitle(paste("Impact speed baseline sd for" ,i,"and reduced logic is",j))
      g2 <- ggplot(aver, aes(x = iter, y = sqrt(out_param_val))) +
        geom_point(color = "firebrick", shape = "diamond", size = 2)+
        labs(x = "Iteration", y = out_param)+
        theme(axis.title.x = element_text(vjust = 0, size = 15),
              axis.title.y = element_text(vjust = 2, size = 15),
              axis.text = element_text(size = 12))+
        ggtitle(paste("Output same as target parameter","and reduced logic is",j))
      ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
      fcount <- fcount + 1 
      ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
      fcount <- fcount + 1
    }
  }
  # This part has not included the multiple simulations
  for ( j in c(TRUE,FALSE)){
    for (i in sampling_method[1:5]){
      
      res <- active_learning (df, sampling_method = i, 
                              target = "none", 
                              reduce_simulations_by_logic = j, # TRUE or FALSE 
                              num_cases_per_iteration = 1,
                              niter = total_iter, 
                              nboot = total_nboot)
      g3 <- ggplot(aver, aes(x = iter, y = sqrt(mean_impact_speed0_sqerr))) +
        geom_point(color = "firebrick", shape = "diamond", size = 2)+
        labs(x = "Iteration", y = "Baseline impact speed sd")+
        theme(axis.title.x = element_text(vjust = 0, size = 15),
              axis.title.y = element_text(vjust = 2, size = 15),
              axis.text = element_text(size = 12))+
        ggtitle(paste("Impact speed baseline sd for" ,i,"and reduced logic is",j))
      
      ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
      fcount <- fcount + 1 
    }
  }
}
