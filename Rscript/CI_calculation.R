CI_calculation <- function(data = data,
                           parameter = "crash_avoidance") #"mean_impact_speed_reduction"
{
  if(parameter == "crash_avoidance"){
    errors = sqrt(data$crash_avoidance_rate_sqerr)
  } else if(parameter == "mean_impact_speed_reduction") {
    errors = sqrt(data$mean_impact_speed_reduction_sqerr)
  }

  # Number of simulations
  n <- length(errors)
  
  # Calculate MSE
  mse <- mean(errors^2, na.rm = TRUE)

  # Standard error of the MSE
  se_mse <- sd(errors^2, na.rm = TRUE) / sqrt(n)

  # 95% Confidence interval for MSE
  ci_lower <- mse - qnorm(0.975) * se_mse
  ci_upper <- mse + qnorm(0.975) * se_mse
  
  # Return 95% confidence interval for RMSE.
  return(list(ci_lower = sqrt(ci_lower), 
              ci_upper = sqrt(ci_upper),
              errors = errors,
              rmse = sqrt(mse)))
}