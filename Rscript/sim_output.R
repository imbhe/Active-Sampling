sim_output <- function(df,input,inputparameter){
  total_iter = inputparameter$total_iter
  n_cases_per_iter = inputparameter$n_cases_per_iter
  Sim_n = inputparameter$Sim_n
  target = input$target
  proposal_dist = input$proposal_dist
  sampling_method = input$sampling_method
  group = input$group
  use_logic = input$use_logic
  res_list <- replicate(Sim_n, data.frame())
  labelled_list <- replicate(Sim_n, data.frame())
  crashes_list<- replicate(Sim_n, data.frame())
  for (k in 1:Sim_n){
    print(paste("simulation",k,"start,","reduced logic is",input$use_logic,", number per iteration:",n_cases_per_iter,",total iteration:",total_iter))
    set.seed(k)
    out <- active_learning (df, sampling_method, 
                            proposal_dist,
                            target, 
                            use_logic, # TRUE or FALSE 
                            n_cases_per_iter,
                            total_iter,
                            inputparameter$total_nboot,
                            inputparameter$verbose,
                            plot = FALSE)
    #print(out$results$neff0)
    res_list[[k]] <- out$results
    labelled_list[[k]] <- out$labelled
    crashes_list[[k]] <- out$crashes
  }
  res = do.call(rbind, res_list)
  labelled = do.call(rbind, labelled_list)
  crashes = do.call(rbind, crashes_list)
  
  aver = as.data.frame(matrix(nrow=total_iter,ncol=0))
  biggest = as.data.frame(matrix(nrow=total_iter,ncol=0))
  smallest = as.data.frame(matrix(nrow=total_iter,ncol=0))
  index = matrix(nrow = total_iter,ncol = Sim_n)
  for(p in 1:total_iter){
    aver$group[p] = group
    aver$use_logic[p] = use_logic
    index[p,] = seq(p,total_iter*Sim_n,total_iter)
    aver$neff0[p] = mean(res$neff0[index[p,]])
    aver$neff_tot[p] = mean(res$neff_tot[index[p,]])
    aver$nsim_tot[p] = mean(res$nsim_tot[index[p,]])
    aver$nsim0[p] = mean(res$nsim0[index[p,]])
    aver$nsim1[p] = mean(res$nsim1[index[p,]])
    aver$iter[p] = mean(res$iter[p])
    aver$mean_impact_speed0_sqerr[p] = sqrt(mean(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$mean_impact_speed_reduction_sqerr[p] = sqrt(mean(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    aver$crash_avoidance_rate_sqerr[p] = sqrt(mean(res$crash_avoidance_rate_sqerr[index[p,]]))
    aver$mean_injury_risk_reduction_sqerr[p] = sqrt(mean(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    aver$impact_speed0_KLdiv_sqerr[p] = sqrt(mean(res$impact_speed0_KLdiv[index[p,]]))
    aver$impact_speed0_KLdiv[p] = mean(res$impact_speed0_KLdiv[index[p,]])
    
    aver$sd_mean_impact_speed0_sqerr[p] = sqrt(var(res$mean_impact_speed0_sqerr[index[p,]]))
    aver$sd_mean_impact_speed_reduction_sqerr[p] = sqrt(var(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    aver$sd_crash_avoidance_rate_sqerr[p] = sqrt(var(res$crash_avoidance_rate_sqerr[index[p,]]))
    aver$sd_mean_injury_risk_reduction_sqerr[p] = sqrt(var(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    
    
    biggest$group[p] = group
    biggest$use_logic[p] = use_logic
    biggest$iter[p] = mean(res$iter[p])
    biggest$neff0[p] = max(res$neff0[index[p,]])
    biggest$mean_impact_speed0_sqerr[p] = sqrt(max(res$mean_impact_speed0_sqerr[index[p,]]))
    biggest$mean_impact_speed_reduction_sqerr[p] = sqrt(max(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    biggest$crash_avoidance_rate_sqerr[p] = sqrt(max(res$crash_avoidance_rate_sqerr[index[p,]]))
    biggest$mean_injury_risk_reduction_sqerr[p] = sqrt(max(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    
    
    smallest$group[p] = group
    smallest$use_logic[p] = use_logic
    smallest$iter[p] = mean(res$iter[p])
    smallest$neff0[p] = min(res$neff0[index[p,]])
    smallest$mean_impact_speed0_sqerr[p] = sqrt(min(res$mean_impact_speed0_sqerr[index[p,]]))
    smallest$mean_impact_speed_reduction_sqerr[p] = sqrt(min(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    smallest$crash_avoidance_rate_sqerr[p] = sqrt(min(res$crash_avoidance_rate_sqerr[index[p,]]))
    smallest$mean_injury_risk_reduction_sqerr[p] = sqrt(min(res$mean_injury_risk_reduction_sqerr[index[p,]]))
    
  }
  return(list(aver = aver, 
              biggest = biggest, 
              smallest = smallest,
              res = res,
              labelled = labelled,
              crashes = crashes))
  
}