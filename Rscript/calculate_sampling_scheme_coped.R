# Calculate sampling scheme.
calculate_sampling_scheme <- function(unlabelled,
                                      labelled,
                                      sampling_method = c("SRS", 
                                                          "importance sampling", 
                                                          "optimised"), 
                                      proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                                        "pps, size = prior weight", 
                                                        "pps, size = prior weight * severity"), 
                                      target = c("NA", # Only used when sampling_method = "optimised", "NA" otherwise.
                                                 "baseline impact speed distribution", 
                                                 "counter-measure injury risk", 
                                                 "impact speed reduction",
                                                 "injury risk reduction", 
                                                 "crash avoidance"),
                                      n_cases = 1,
                                      r2 = NULL,
                                      rmse = NULL) {

  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)
  
  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf("Error in calculate_sampling_scheme. sampling_method = %s and proposal_dist = %s not allowed.", 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "optimised".
  if ( sampling_method != "optimised" & target != "NA") { 
    stop(sprintf("Error in calculate_sampling_scheme. sampling_method = %s and target = %s not allowed.", 
                 sampling_method, target))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop("Error in calculate_sampling_scheme. sampling_method = importance sampling and proposal_dist = NA not allowed.")
  }
  
  # target must be specified if sampling_method = "optimised".
  if ( sampling_method == "optimised" & target == "NA" ) {
    stop("Error in calculate_sampling_scheme. sampling_method = optimised and target = NA not allowed.")
  }
  
  # n_cases should be integer between 1 and number of cases in unlabelled dataset.
  n_cases <- round(n_cases)
  if ( n_cases < 1 ) {
    stop("Error in calculate_sampling_scheme. n_cases must be greater than or equal to 1.")
  }
  if ( n_cases > length(unique(unlabelled$caseID)) ) {
    stop(sprintf("Error in calculate_sampling_scheme. n_cases must be smaller than or equal to %d (number of cases in unlabelled dataset).", 
                 length(unique(unlabelled$caseID))))
  }
  
  
  # Calculate maximal impact speed per case. ----
  if ( nrow(labelled) > 0) {
    maximpact0 <- labelled %>% 
      group_by(caseID) %>% 
      summarise(maximpact0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
      ungroup() %>% 
      dplyr::select(caseID, maximpact0)
    
    unlabelled %<>% 
      left_join(maximpact0, by = "caseID")
  }

  # Set 'size' from previous iteration to prev_size.
  unlabelled %<>% 
    mutate(prev_size = size / eoff_acc_prob) %>% 
    dplyr::select(-size)
  
  
  # Calculate 'size' of pps (probability proportional to size) sampling. ----
  if ( sampling_method == "SRS" ) {
    
    size <- rep(1, nrow(unlabelled))
    
  } else if ( sampling_method == "importance sampling" ) {
    
    if ( proposal_dist == "pps, size = prior weight" ) {
      
      size <- with(unlabelled, eoff_acc_prob)
      
    } else if ( proposal_dist == "pps, size = prior weight * severity" ) {
      
      # Severity is assumed to increase with increase in EOFF, deceleration and 
      # maximal impact speed under baseline scenario. 
      size <- with(unlabelled, eoff_acc_prob * (eoff + 0.1) * (acc - min(acc) + 0.1) * maximpact0)
      
    } 
    
  } else if ( sampling_method == "optimised" ) {
    
    if ( target == "impact speed reduction" ) {
      
      if ( is.na(r2$impact_speed_reduciton) | r2$impact_speed_reduciton < 0 ) { # If prediction R-square is negative: set all equal.

        size <- 1
        
      } else { # Else: optimal sampling.
        
        size <- with(unlabelled, sqrt((impact_speed_reduction_pred)^2 + rmse$impact_speed_reduction^2))

      }
      
    } else if ( target == "injury risk reduction" ) {
      
      if ( is.na(r2$injury_risk_reduciton) | r2$injury_risk_reduciton < 0 ) { # If prediction R-square is negative: set all equal.

        size <- 1
        
      } else { # Else: optimal sampling.
        
        size <- with(unlabelled, sqrt((injury_risk_reduction_pred)^2 + rmse$injury_risk_reduction^2))

      }
      
      
    } else if ( target == "baseline impact speed distribution" ) {
      
      if ( is.na(r2$impact_speed0) | r2$impact_speed0 < 0 ) { # If prediction R-square is negative: set all equal.

        size <- 1

      } else { # Else: optimal sampling.
        
        est <- estimate_targets(labelled, weightvar = "final_weight")
        
        # Ad-hoc correction for zero SD in small samples.
        if ( is.null(est) || is.na(est["impact_speed0_logSD"]) | est["impact_speed0_logSD"] == 0 ) { 
          est["impact_speed0_logSD"] <- Inf
        }
        
        Z <- with(unlabelled, (log(impact_speed0_pred) - est["impact_speed0_logmean"]) / 
                    est["impact_speed0_logSD"])
        sigma <- rmse$log_impact_speed0
        r <- sigma / est["impact_speed0_logSD"]
        
        size <- sqrt(Z^2 + r^2 + 0.25 * (1 + 2 * Z^2 + Z^4 + 6 * Z^2 * r^2 + 3 * r^2))
        size[is.infinite(size)] <- 0

      }

      
    } else if ( target == "crash avoidance" ) {
      
      crashes <- labelled %>% filter(impact_speed0 > 0 & final_weight > 0)
      rr <- 1 - estimate_targets(crashes, weightvar = "final_weight")["crash_avoidance_rate"]
      size <- with(unlabelled, sqrt(rr^2 - collision_prob1_pred * (2 * rr - 1)))

    } 
    
    # If no positive 'sizes' found -> set all equal. 
    # Becomes same as importance sampling with probability proportional to EOFF-ACC probability.
    if ( any(is.na(size)) || !any(size > 0) || any(is.infinite(size)) ) { 
      size[1:length(size)] <- 1
    }
    size[size <= 0] <- min(size[size > 0]) # Zeroes and negative values not allowed.

    # Account for probability of deceleration-glance pair.
    #  + Smoothing: take average of (standardised) 'size' in current and previous iteration. 
    size <- with(unlabelled, eoff_acc_prob * (size / sum(size) + prev_size / sum(prev_size)))
        
  } 
  
  # Probability proportional to size.
  sampling_probability <- n_cases * size / sum(size)
  
  
  # Adjustment to account for sampling of multiple cases per iteration. ----
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
    sampling_probability[jx] <- (n_cases - length(certainty_selection_cases)) * size[jx] / sum(size[jx]) 
    
    case_probability <- tapply(sampling_probability, unlabelled$caseID, sum)
    
  } 
  
  # Return.
  return(list(size = size, sampling_probability = sampling_probability, case_probability = case_probability))
  
}