# Calculate sampling scheme.
calculate_sampling_scheme <- function(unlabelled,
                                      labelled,
                                      sampling_method = c("uniform", 
                                                                "propto eoff_acc_prob", 
                                                                "propto eoff_acc_prob * eoff * maximpact0", 
                                                                "propto eoff_acc_prob * abs(acc) * maximpact0", 
                                                                "propto eoff_acc_prob * eoff * abs(acc) * maximpact0", 
                                                                "optimised"), 
                                      target = c("none", 
                                                 "impact speed reduction", 
                                                 "baseline impact speed distribution", 
                                                 "crash avoidance",
                                                 "injury risk reduction", 
                                                 "baseline injury risk distribution"),
                                      num_cases) {
  
  # Check input parameters.
  sampling_method <- match.arg(sampling_method)
  target <- match.arg(target)
  
  # target should be "none" when sampling_method not equal to "optimised".
  if ( sampling_method != "optimised" ) { 
    target = "none" 
  } 
  
  # Target must be specified if sampling_method = "optimised".
  if ( sampling_method == "optimised" & target == "none" ) {
    stop("Error in calculate_sampling_schene. sampling_method = optimised and target = none not allowed.")
  }
  
  # num_cases should be integer between 1 and number of cases in input unlabelled set.
  num_cases <- round(num_cases)
  num_cases <- max(c(num_cases, 1))
  num_cases <- min(c(num_cases, length(unique(unlabelled$caseID))))
  

  # Calculate maximal impact speed per case.
  maximpact0 <- labelled %>% 
    group_by(caseID) %>% 
    summarise(maximpact0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() %>% 
    dplyr::select(caseID, maximpact0)
  
  unlabelled %<>% 
    left_join(maximpact0, by = "caseID")
  
  
  # Calculate 'size'.
  if ( sampling_method == "uniform" ) {
    
    size <- rep(1, nrow(unlabelled))
    
  } else if ( sampling_method == "propto eoff_acc_prob" ) {
    
    size <- with(unlabelled, eoff_acc_prob)
    
  } else if ( sampling_method == "propto eoff_acc_prob * eoff * maximpact0" ) {
    
    size <- with(unlabelled, eoff_acc_prob  * (eoff + 0.1) * maximpact0)
    
  } else if ( sampling_method == "propto eoff_acc_prob * abs(acc) * maximpact0" ) {
    
    size <- with(unlabelled, eoff_acc_prob  * abs(acc) * maximpact0)
 
  } else if ( sampling_method == "propto eoff_acc_prob * eoff * abs(acc) * maximpact0" ) {
    
    size <- with(unlabelled, eoff_acc_prob * (eoff + 0.1) * abs(acc) * maximpact0)
    
  } else if ( sampling_method == "optimised" ) {
    
    if ( target == "impact speed reduction" ) {
      
      size <- with(unlabelled, abs(impact_speed1_pred - impact_speed0_pred))

    } else if ( target == "baseline impact speed distribution" ) {
      
      size <- with(unlabelled, abs(impact_speed1_pred - impact_speed0_pred))

    } else if ( target == "crash avoidance" ) {
      
      size <- with(unlabelled, sqrt(collision_prob1_pred))

    } else if ( target == "injury risk reduction" ) {
      
      size <- with(unlabelled, abs(injury_risk1_pred - injury_risk0_pred))
      
    } else if ( target == "baseline injury risk distribution" ) {
      
      size <- with(unlabelled, abs(injury_risk1_pred - injury_risk0_pred))

    }
    
    size[size <= 0] <- min(size[size > 0]) # Zeros not allowed.
    
    # Account for probability of (deceleration, glance) pair and probability of crash in baseline scenario.
    size <- with(unlabelled, sqrt(collision_prob0_pred) * eoff_acc_prob * size )
    
  } else {
    
    stop(sprintf("ERROR in caltulate_sampling_scheme. Option sampling_method = %s not found.", sampling_method))
    
  }
  
  # Probability proportional to size.
  sampling_probability <- num_cases * size / sum(size)
  
  
  # Adjustment to account for sampling of multiple cases per iteration.
  case_probability <- tapply(sampling_probability, unlabelled$caseID, sum)
  
  cases <- unique(unlabelled$caseID)
  certainty_selection_cases <- NULL
  while ( any(case_probability > 1) ) {
    
    ix <- as.numeric(names(which(case_probability > 1)))
    certainty_selection_cases <- union(certainty_selection_cases, ix)
    
    for ( i in seq_along(ix) ) {
      
      jx <- which(unlabelled$caseID == ix[i])
      sampling_probability[jx] <- sampling_probability[jx] / case_probability[paste(ix[i])]
    
    }
    
    jx <- which( !(unlabelled$caseID %in% certainty_selection_cases) )
    sampling_probability[jx] <- (num_cases - length(certainty_selection_cases)) * size[jx] / sum(size[jx]) 
    
    case_probability <- tapply(sampling_probability, unlabelled$caseID, sum)
    
  } 
  
  # Return.
  return(list(sampling_probability = sampling_probability, case_probability = case_probability))
  
}