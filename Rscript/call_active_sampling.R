call_active_sampling <- function(df,input,inputparameter){
  niter = inputparameter$niter
  batch_size = inputparameter$batch_size
  Sim_n = inputparameter$Sim_n
  target = input$target
  proposal_dist = input$proposal_dist
  sampling_method = input$sampling_method
  group = input$group
  use_logic = input$use_logic
  opt_method = input$opt_method
  res_list <- replicate(Sim_n, data.frame())
  labelled_list <- replicate(Sim_n, data.frame())
  crashes_list<- replicate(Sim_n, data.frame())
  nboot = inputparameter$nboot
  
  for (k in 1:Sim_n){
    print(paste("Sampling method",group,",simulation",k,"starts,","number per iteration:",batch_size,",total iteration:",niter))
    set.seed(k)
    out <- active_sampling (df, sampling_method, 
                            proposal_dist,
                            target, 
                            opt_method,
                            batch_size,
                            niter,
                            nboot,
                            verbose = FALSE,
                            plot = FALSE)
    res_list[[k]] <- out$results
    labelled_list[[k]] <- out$labelled
    crashes_list[[k]] <- out$crashes
  }
  res = do.call(rbind, res_list)
  labelled = do.call(rbind, labelled_list)
  crashes = do.call(rbind, crashes_list)
  
  aver = as.data.frame(matrix(nrow=niter,ncol=0))
  biggest = as.data.frame(matrix(nrow=niter,ncol=0))
  smallest = as.data.frame(matrix(nrow=niter,ncol=0))
  index = matrix(nrow = niter,ncol = Sim_n)
  for(p in 1:niter){
    aver$group[p] = group
    aver$opt_method[p] = opt_method
    aver$n_crashes[p] = (mean(res$n_crashes[index[p,]]))
    index[p,] = seq(p,niter*Sim_n,niter)
    aver$total_sample_size[p] = mean(res$total_sample_size[index[p,]])
    aver$iter[p] = mean(res$iter[p])

    aver$mean_impact_speed_reduction_sqerr[p] = sqrt(mean(res$mean_impact_speed_reduction_sqerr[index[p,]]))
    aver$crash_avoidance_rate_sqerr[p] = sqrt(mean(res$crash_avoidance_rate_sqerr[index[p,]]))
    aver$mean_impact_speed_reduction_se_boot[p] = mean(res$mean_impact_speed_reduction_se_boot[index[p,]])
    aver$crash_avoidance_rate_se_boot[p] = mean(res$crash_avoidance_rate_se_boot[index[p,]])
    aver$mean_impact_speed_reduction_se_classic[p] = mean(res$mean_impact_speed_reduction_se_classic[index[p,]])
    aver$crash_avoidance_rate_se_classic[p] = mean(res$crash_avoidance_rate_se_classic[index[p,]])
    aver$mean_impact_speed_reduction_se_mart[p] = mean(res$mean_impact_speed_reduction_se_mart[index[p,]])
    aver$crash_avoidance_rate_se_mart[p] = mean(res$crash_avoidance_rate_se_mart[index[p,]])
    
    
    aver$mean_impact_speed_reduction_ci_cover_boot[p] = sum(res$mean_impact_speed_reduction_ci_cover_boot[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_boot[p] = sum(res$crash_avoidance_rate_ci_cover_boot[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_boot[p] = sum(res$mean_injury_risk_reduction_ci_cover_boot[index[p,]])/Sim_n
    aver$mean_impact_speed_reduction_ci_cover_classic[p] = sum(res$mean_impact_speed_reduction_ci_cover_classic[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_classic[p] = sum(res$crash_avoidance_rate_ci_cover_classic[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_classic[p] = sum(res$mean_injury_risk_reduction_ci_cover_classic[index[p,]])/Sim_n
    aver$mean_impact_speed_reduction_ci_cover_mart[p] = sum(res$mean_impact_speed_reduction_ci_cover_mart[index[p,]])/Sim_n
    aver$crash_avoidance_rate_ci_cover_mart[p] = sum(res$crash_avoidance_rate_ci_cover_mart[index[p,]])/Sim_n
    aver$mean_injury_risk_reduction_ci_cover_mart[p] = sum(res$mean_injury_risk_reduction_ci_cover_mart[index[p,]])/Sim_n
    
  }
  return(list(aver = aver, 
              res = res))
  
}