estimate_targets <- function(data, weightvar = NULL) {
  
  data$w <- data[, weightvar]

  mean_impact_speed_reduction <- with(data, sum( w * (impact_speed0 > 0) * (impact_speed_reduction) ) / sum (w * (impact_speed0 > 0)) )
  mean_injury_risk_reduction <- with(data, sum( w * (impact_speed0 > 0) * (injury_risk_reduction) ) / sum( w * (impact_speed0 > 0)) )
  crash_avoidance_rate <- 1 - with(data, sum( w * (impact_speed0 > 0) * (impact_speed1 > 0) ) / sum( w * (impact_speed0 > 0)) )

  return(c("mean_impact_speed_reduction" = mean_impact_speed_reduction, 
           "mean_injury_risk_reduction" = mean_injury_risk_reduction, 
           "crash_avoidance_rate" = crash_avoidance_rate))
}