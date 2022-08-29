sim_output <- function(df,input,inputparameter){
  total_iter = inputparameter$total_iter
  num_cases_per_iteration = inputparameter$num_cases_per_iteration
  Sim_n = inputparameter$Sim_n
  target = input$target
  proposal_dist = input$proposal_dist
  sampling_method = input$sampling_method
  group = input$group
  reduce_simulations_by_logic = input$reduce_simulations_by_logic
  res_list <- replicate(Sim_n, data.frame())
  for (k in 1:Sim_n){
    print(paste("simulation",k,"start,","reduced logic is",input$reduce_simulations_by_logic,", number per iteration:",num_cases_per_iteration,",total iteration:",total_iter))
    set.seed(k)
    out <- active_learning (df, sampling_method, 
                            proposal_dist,
                            target, 
                            reduce_simulations_by_logic, # TRUE or FALSE 
                            num_cases_per_iteration,
                            total_iter,
                            inputparameter$nburnin,
                            inputparameter$total_nboot,
                            inputparameter$verbose,
                            plot = FALSE)
    #print(out$results$neff0)
    res_list[[k]] <- out$results
  }
  res = do.call(rbind, res_list)
  
  aver = as.data.frame(matrix(nrow=total_iter,ncol=0))
  biggest = as.data.frame(matrix(nrow=total_iter,ncol=0))
  smallest = as.data.frame(matrix(nrow=total_iter,ncol=0))
  index = matrix(nrow = total_iter,ncol = Sim_n)
  for(p in 1:total_iter){
    aver$group[p] = group
    aver$reduce_simulations_by_logic[p] = reduce_simulations_by_logic
    index[p,] = seq(p,total_iter*Sim_n,total_iter)
    aver$neff0[p] = mean(res$neff0[index[p,]])
    aver$iter[p] = mean(res$iter[p])
    aver$mean_impact_speed0_sqerr[p] = sqrt(mean(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$absolute_impact_speed_reduction_sqerr[p] = sqrt(mean(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    aver$proportion_crashes_avoided_sqerr[p] = sqrt(mean(res$proportion_crashes_avoided_sqerr[index[p,]]))
    aver$absolute_injury_risk_reduction_sqerr[p] = sqrt(mean(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    aver$impact_speed0_KLdiv_sqerr[p] = sqrt(mean(res$impact_speed0_KLdiv[index[p,]]))
    
    aver$sd_mean_impact_speed0_sqerr[p] = sqrt(var(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$sd_absolute_impact_speed_reduction_sqerr[p] = sqrt(var(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    aver$sd_proportion_crashes_avoided_sqerr[p] = sqrt(var(res$proportion_crashes_avoided_sqerr[index[p,]]))
    aver$sd_absolute_injury_risk_reduction_sqerr[p] = sqrt(var(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    
    
    biggest$group[p] = group
    biggest$reduce_simulations_by_logic[p] = reduce_simulations_by_logic
    biggest$iter[p] = mean(res$iter[p])
    biggest$neff0[p] = max(res$neff0[index[p,]])
    biggest$mean_impact_speed0_sqerr[p] = sqrt(max(res$mean_impact_speed0_sqerr[index[p,]]))
    biggest$absolute_impact_speed_reduction_sqerr[p] = sqrt(max(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    biggest$proportion_crashes_avoided_sqerr[p] = sqrt(max(res$proportion_crashes_avoided_sqerr[index[p,]]))
    biggest$absolute_injury_risk_reduction_sqerr[p] = sqrt(max(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    
    
    smallest$group[p] = group
    smallest$reduce_simulations_by_logic[p] = reduce_simulations_by_logic
    smallest$iter[p] = mean(res$iter[p])
    smallest$neff0[p] = min(res$neff0[index[p,]])
    smallest$mean_impact_speed0_sqerr[p] = sqrt(min(res$mean_impact_speed0_sqerr[index[p,]]))
    smallest$absolute_impact_speed_reduction_sqerr[p] = sqrt(min(res$absolute_impact_speed_reduction_sqerr[index[p,]]))
    smallest$proportion_crashes_avoided_sqerr[p] = sqrt(min(res$proportion_crashes_avoided_sqerr[index[p,]]))
    smallest$absolute_injury_risk_reduction_sqerr[p] = sqrt(min(res$absolute_injury_risk_reduction_sqerr[index[p,]]))
    
  }
  return(list(aver = aver, 
              biggest = biggest, 
              smallest = smallest,
              res = res))
  
}