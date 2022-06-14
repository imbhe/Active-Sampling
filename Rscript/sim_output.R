sim_output <- function(df,inputparameter){
  total_iter = as.numeric(inputparameter[5])
  Sim_n = as.numeric(inputparameter[7])
  res_list <- replicate(Sim_n, data.frame())
  for (k in 1:Sim_n){
    set.seed(k)
    out <- active_learning (df, sampling_method = inputparameter[1], 
                            target = inputparameter[2], 
                            reduce_simulations_by_logic = TRUE, # TRUE or FALSE 
                            num_cases_per_iteration = as.numeric(inputparameter[4]),
                            niter = as.numeric(inputparameter[5]), 
                            nboot = as.numeric(inputparameter[6]))
    print(paste("simulation round:",k))
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
    aver$mean_impact_speed0_sqerr[p] = mean(sqrt(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$absolute_impact_speed_reduction_sqerr[p] = mean(sqrt(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    aver$proportion_crashes_avoided_sqerr[p] = mean(sqrt(res$proportion_crashes_avoided_sqerr[index[p,]]))
    aver$absolute_injury_risk_reduction_sqerr[p] = mean(sqrt(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    aver$nsim_tot[p] = mean(res$nsim_tot[index[p,]])
    
    biggest$iter[p] = mean(res$iter[p])
    biggest$mean_impact_speed0_sqerr[p] = max(sqrt(res$mean_impact_speed0_sqerr[index[p,]]))
    biggest$absolute_impact_speed_reduction_sqerr[p] = max(sqrt(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    biggest$proportion_crashes_avoided_sqerr[p] = max(sqrt(res$proportion_crashes_avoided_sqerr[index[p,]]))
    biggest$absolute_injury_risk_reduction_sqerr[p] = max(sqrt(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    biggest$nsim_tot[p] = max(res$nsim_tot[index[p,]])
    
    smallest$iter[p] = mean(res$iter[p])
    smallest$mean_impact_speed0_sqerr[p] = min(sqrt(res$mean_impact_speed0_sqerr[index[p,]]))
    smallest$absolute_impact_speed_reduction_sqerr[p] = min(sqrt(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    smallest$proportion_crashes_avoided_sqerr[p] = min(sqrt(res$proportion_crashes_avoided_sqerr[index[p,]]))
    smallest$absolute_injury_risk_reduction_sqerr[p] = min(sqrt(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    smallest$nsim_tot[p] = min(res$nsim_tot[index[p,]])
  }
  return(list(aver = aver, 
              biggest = biggest, 
              smallest = smallest,
              res_list = res_list))
}