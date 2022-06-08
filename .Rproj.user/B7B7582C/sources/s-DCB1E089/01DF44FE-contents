# Calculate sampling scheme.
calculate_sampling_scheme <- function(data,
                                      sampling_method = c("uniform", 
                                                                "propto prob", 
                                                                "propto prob * eoff", 
                                                                "propto prob * abs(acc)", 
                                                                "propto prob * eoff * abs(acc)", 
                                                                "optimised"), 
                                      target = c("none", 
                                                 "impact speed reduction", 
                                                 "baseline impact speed distribution", 
                                                 "proportion collisions avoided",
                                                 "injury risk reduction", 
                                                 "baseline injury risk distribution"),
                                      num_cases) {
  
  # Check input parameters.
  sampling_method <- match.arg(sampling_method)
  target <- match.arg(target)
  if ( sampling_method != "optimised " ) { 
    target = "none" 
  }
  
  # num_cases should be integer between 1 and number of cases in input data set.
  num_cases <- round(num_cases)
  num_cases <- max(c(num_cases, 1))
  num_cases <- min(c(num_cases, length(unique(data$caseID))))
  
  
  
  # Calculate 'size'.
  if ( sampling_method == "uniform" ) {
    
    size <- rep(1, nrow(data))
    
  } else if ( sampling_method == "propto eoff_acc_prob" ) {
    
    size <- with(data, eoff_acc_prob)
    
  } else if ( sampling_method == "propto eoff_acc_prob * eoff" ) {
    
    size <- with(data, eoff_acc_prob  * (eoff + 0.1))
    
  } else if ( sampling_method == "propto eoff_acc_prob * abs(acc)" ) {
    
    size <- with(data, eoff_acc_prob  * abs(acc))
 
  } else if ( sampling_method == "propto eoff_acc_prob * eoff * abs(acc)" ) {
    
    size <- with(data, eoff_acc_prob * (eoff + 0.1) * abs(acc))
    
  } else if ( sampling_method == "optimised" ) {
    
    if ( target == "impact speed reduction" ) {
      
      size <- with(data, abs(impact_speed1_pred - impact_speed0_pred))
      size[size <= 0] <- min(size[size > 0])
      
    } else if ( target == "baseline impact speed distribution" ) {
      
      size <- with(data, abs(impact_speed1_pred - impact_speed0_pred))
      size[size <= 0] <- min(size[size > 0])
      
    } else if ( target == "proportion collisions avoided" ) {
      
      size <- with(data, sqrt(collision_prob1_pred))
      
    } else if ( target == "injury risk reduction" ) {
      
      size <- with(data, abs(injury_risk1_pred - injury_risk0_pred))
      size[size <= 0] <- min(size[size > 0])
      
    } else if ( target == "baseline injury risk distribution" ) {
      
      size <- with(data, abs(injury_risk1_pred - injury_risk0_pred))
      size[size <= 0] <- min(size[size > 0])
      
    }
    
    # Account for probability of (deceleration, glance) pair and probability of crash in baseline scenario.
    size <- with(data, sqrt(collision_prob0_pred) * eoff_acc_prob * size )
    
  } else {
    
    stop(sprintf("ERROR in caltulate_sampling_scheme. Option sampling_method = %s not found.", sampling_method))
    
  }
  
  # Probability proportional to size.
  sampling_probability <- num_cases * size / sum(size)
  
  
  # Adjustment to account for sampling of multiple cases per iteration.
  case_probability <- tapply(sampling_probability, data$caseID, sum)
  
  cases <- unique(data$caseID)
  certainty_selection_cases <- NULL
  while ( any(case_probability > 1) ) {
    
    ix <- as.numeric(names(which(case_probability > 1)))
    certainty_selection_cases <- union(certainty_selection_cases, ix)
    
    for ( i in seq_along(ix) ) {
      
      jx <- which(data$caseID == ix[i])
      sampling_probability[jx] <- sampling_probability[jx] / case_probability[paste(ix[i])]
    
    }
    
    jx <- which( !(data$caseID %in% certainty_selection_cases) )
    sampling_probability[jx] <- (num_cases - length(certainty_selection_cases)) * size[jx] / sum(size[jx]) 
    
    case_probability <- tapply(sampling_probability, data$caseID, sum)
    
  } 
  
  # Return.
  return(list(sampling_probability = sampling_probability, case_probability = case_probability))
  
}
