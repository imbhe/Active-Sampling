estimate_targets <- function(data, weightvar) {
  crashes <- data %>% filter(impact_speed0 > 0)
  crashes$w <- crashes[, weightvar]
  
  mean_impact_speed0 <- with(crashes, sum(w * (impact_speed0)) / sum(w))
  mean_impact_speed1 <- with(crashes, sum(w * (impact_speed1)) / sum(w))
  absolute_impact_speed_reduction <- mean_impact_speed1 - mean_impact_speed0
  relative_impact_speed_reduction <- mean_impact_speed1 / mean_impact_speed0
  mean_injury_risk0 <- with(crashes, sum(w * (injury_risk0)) / sum(w))
  mean_injury_risk1 <- with(crashes, sum(w * (injury_risk1)) / sum(w))
  absolute_injury_risk_reduction <- mean_injury_risk1 - mean_injury_risk0
  relative_injury_risk_reduction <- mean_injury_risk1 / mean_injury_risk0
  proportion_crashes_avoided <- 1 - with(crashes, sum(w * (impact_speed1 > 0)) / sum(w))
  
  impact_speed0_logmean <- with(crashes, sum(w * log(impact_speed0)) / sum(w))
  impact_speed0_logSD <- with(crashes, sqrt(sum(w * (log(impact_speed0) - impact_speed0_logmean)^2) / sum(w)))

  return(c("mean_impact_speed0" = mean_impact_speed0, 
           "mean_impact_speed1" = mean_impact_speed1, 
           "absolute_impact_speed_reduction" = absolute_impact_speed_reduction, 
           "relative_impact_speed_reduction" = relative_impact_speed_reduction, 
           "mean_injury_risk0" = mean_injury_risk0, 
           "mean_injury_risk1" = mean_injury_risk1, 
           "absolute_injury_risk_reduction" = absolute_injury_risk_reduction, 
           "relative_injury_risk_reduction" = relative_injury_risk_reduction, 
           "proportion_crashes_avoided" = proportion_crashes_avoided, 
           "impact_speed0_logmean" = impact_speed0_logmean,
           "impact_speed0_logSD" = impact_speed0_logSD))
}