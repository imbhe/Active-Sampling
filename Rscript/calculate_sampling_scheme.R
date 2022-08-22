# Calculate sampling scheme.
calculate_sampling_scheme <- function(unlabelled,
                                      labelled,
                                      sampling_method = c("uniform", 
                                                          "importance sampling", 
                                                          "optimised"), 
                                      proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                                        "propto eoff_acc_prob", 
                                                        "propto eoff_acc_prob * eoff * abs(acc) * maximpact0"), 
                                      target = c("NA", # Only used when sampling_method = "optimised", "NA" otherwise.
                                                 "baseline impact speed distribution", 
                                                 "impact speed reduction", 
                                                 "crash avoidance",
                                                 "injury risk reduction",
                                                 "injury risk reduction, stratified"),
                                      num_cases = 1, 
                                      sigma = 0) {
  
  # Check input parameters.
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)
  
  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" ) { 
    proposal_dist <- "NA"
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop("Error in calculate_sampling_scheme. sampling_method = importance sampling and proposal_dist = NA not allowed.")
  }
  
  # target should be "NA" when sampling_method not equal to "optimised".
  if ( sampling_method != "optimised" ) { 
    target <- "NA" 
  } 
  
  # target must be specified if sampling_method = "optimised".
  if ( sampling_method == "optimised" & target == "NA" ) {
    stop("Error in calculate_sampling_scheme. sampling_method = optimised and target = NA not allowed.")
  }
  
  # num_cases should be integer between 1 and number of cases in input unlabelled set.
  num_cases <- round(num_cases)
  num_cases <- max(c(num_cases, 1))
  num_cases <- min(c(num_cases, length(unique(unlabelled$caseID))))
  

  # Calculate maximal impact speed per case.
  if ( nrow(labelled) > 0) {
    maximpact0 <- labelled %>% 
      group_by(caseID) %>% 
      summarise(maximpact0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
      ungroup() %>% 
      dplyr::select(caseID, maximpact0)
    
    unlabelled %<>% 
      left_join(maximpact0, by = "caseID")
  }
  
  # Set to one, i.e., assume all simulations will produce a crash under baseline scenario.
  collision_prob0_pred <- 1
  
  
  # Calculate 'size'.
  if ( sampling_method == "uniform" ) {
    
    size <- rep(1, nrow(unlabelled))
    
  } else if ( sampling_method == "importance sampling" ) {
    
    if ( proposal_dist == "propto eoff_acc_prob" ) {
      
      size <- with(unlabelled, eoff_acc_prob)
      
    } else if ( proposal_dist == "propto eoff_acc_prob * eoff * abs(acc) * maximpact0" ) {
      
      size <- with(unlabelled, eoff_acc_prob * (eoff + 0.1) * abs(acc) * maximpact0)
      
    } 

  } else if ( sampling_method == "optimised" ) {
    
    if ( target == "impact speed reduction" ) {
      
      size <- with(unlabelled, sqrt((impact_speed0_pred - impact_speed1_pred)^2 + sigma^2))

    } else if ( target == "baseline impact speed distribution" ) {
      
      est <- estimate_targets(labelled, weightvar = "final_weight")
      if ( is.null(est) || is.na(est["impact_speed0_logSD"]) | est["impact_speed0_logSD"] == 0 ) { # Ad-hoc correction for zero SD in small samples.
        est["impact_speed0_logSD"] <- Inf
      }
      Z <- with(unlabelled, (log(impact_speed0_pred) - est["impact_speed0_logmean"]) / 
                  est["impact_speed0_logSD"])
      r <- sigma / est["impact_speed0_logSD"]
      size <- sqrt(Z^2 + r^2 + 0.25 * (1 + 2 * Z^2 + Z^4 + 6 * Z^2 * r^2 + 3 * r^2))
      size[is.infinite(size)] <- 0
      
    } else if ( target == "crash avoidance" ) {
      
      crashes <- labelled %>% filter(impact_speed0 > 0 & final_weight > 0)
      rr <- 1 - estimate_targets(crashes)["proportion crashes avoided"]
      size <- with(unlabelled, sqrt(rr^2 - collision_prob1_pred * (2 * rr - 1)))

    } else if ( target == "injury risk reduction" ) {
      
      size <- with(unlabelled, sqrt((injury_risk0_pred - injury_risk1_pred)^2 + sigma^2))
      
    } else if ( target == "injury risk reduction, stratified" ) {
      
      nStrata <- 10
      sizeMat <- matrix(0, nrow = nrow(unlabelled), ncol = nStrata)
      for ( i in 1:nStrata ) {
        sizeMat[, i] <- with(unlabelled, ifelse(impact_speed0_pred > 10 * (i - 1) & impact_speed0_pred <= 10 * i, sqrt(collision_prob0_pred) * eoff_acc_prob * sqrt((injury_risk0_pred - injury_risk1_pred)^2 + sigma^2), rep(0, length(injury_risk0_pred))))
      }
      
      # Pooled 'size' is root sum of squares of standardised individual 'sizes'.
      # Without standardisation: produces same result as non-stratified version.
      size2 <- sizeMat^2
      csum <- colSums(size2)
      ix <- which(csum > 0)
      size <- sqrt(rowSums(scale(size2[, ix], center = FALSE, scale = csum[ix]))) 
      
      # To account for probability of (deceleration, glance) pair and probability of crash in baseline scenario. 
      # Note: use division here since we multiple both on row 98 and 118.
      size <- with(unlabelled, (sqrt(collision_prob0_pred) * eoff_acc_prob)^(-1) * size)
      size[is.na(size)] <- 0
      
    } 

    if ( all(is.na(size)) || !any(size > 0) ) { # If no positive 'sizes' found -> set all equal. Becomes same as importance sampling with probability proportional to eoff acc probability.
      size[1:length(size)] <- 1
    }
    size[size <= 0] <- min(size[size > 0]) # Zeroes and negative values not allowed.

    # Account for probability of (deceleration, glance) pair and probability of crash in baseline scenario.
    size <- with(unlabelled, sqrt(collision_prob0_pred) * eoff_acc_prob * size)
    
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
