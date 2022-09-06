################################################################################
#
# active_learning.R
#
# INPUT:
#
# data: input dataset with variables
#   - 'caseID': ID for original crash event. 
#   - 'eoff': glance duration off road after tauinv = 0.2 s (overshot).
#   - 'acc': acceleration (negative value means positive deceleration).
#   - 'eoff_acc_prob': probability of (eoff, acc) pair according to baseline distribution.
#   - 'impact_speed0': impact speed in baseline scenario.
#   - 'impact_speed1': impact speed  in counter factual scenario (i.e., with counter measure such as AEB).                           
#   - 'injury_risk0': injury risk in baseline scenario.
#   - 'injury_risk1': injury risk in counter factual scenario (i.e. with counter measure such as AEB).                           
#
# sampling_method: SRS (simple random sampling), importance sampling (probability proportional to 'size'), or optimised.
#
# target: target of optimisation, only used when sampling_method = "optimised".
#
# use_logic: Use logical constraints (TRUE or FALSE) to infer regions with certainty outcomes 
#                              (no crash or maximal impact speed collision) and
#                              avoid sampling in those regions.
#
# n_cases_per_iter: number of cases to sample from per iteration. 
#
# niter: number of iterations.
#
# nboot: number of bootstrap replicates used to calculate confidence intervals.
#
# verbose: should iteration progress be printed to console? (TRUE/FALSE).
#
# plot: should plots of predicted outcomes and sampling probabilities be produced? (TRUE/FALSE).
#
#
# OUTPUT: 
#
# List of three datasets:
#   - results: meta information of simulation, iteration history, estimates with standard errors and squared errors.
#   - labelled: all labelled data points. 
#   - crashes: all generated crashes. 
#
################################################################################


active_learning <- function(data, 
                            sampling_method = c("SRS", 
                                                "importance sampling", 
                                                "optimised"), 
                            proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                              "pps, size = prior weight", 
                                              "pps, size = prior weight * severity"), 
                            target = c("NA", # Only used when sampling_method = "optimised", "NA" otherwise.
                                       "baseline impact speed distribution", 
                                       "impact speed reduction",
                                       "injury risk reduction", 
                                       "crash avoidance"),
                            use_logic = TRUE, # TRUE or FALSE. 
                            n_cases_per_iter = 1,
                            niter = 500, 
                            nboot = 100, 
                            verbose = FALSE, # TRUE or FALSE.
                            plot = FALSE) { # TRUE or FALSE.
  
  
  # Make sure packages are loaded. ----
  require("boot")
  require("caret")
  require("magrittr")
  require("ranger")
  require("sampling")
  require("tidyverse")
  
  
  # Calculate variables. ----
  data %<>% 
    mutate(impact_speed_reduction = impact_speed0 - impact_speed1,
           injury_risk_reduction = injury_risk0 - injury_risk1)

  
  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)

  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and proposal_dist = %s not allowed.", 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "optimised".
  if ( sampling_method != "optimised" & target != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and target = %s not allowed.", 
                 sampling_method, target))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop("Error in active_learning. sampling_method = importance sampling and proposal_dist = NA not allowed.")
  }

  # target must be specified if sampling_method = "optimised".
  if ( sampling_method == "optimised" & target == "NA" ) {
    stop("Error in active_learning. sampling_method = optimised and target = NA not allowed.")
  }
  
  # n_cases_per_iter should be integer between 1 and number of cases in input dataset.
  n_cases_per_iter <- round(n_cases_per_iter)
  if ( n_cases_per_iter < 1 ) {
    stop("Error in active_learning. n_cases_per_iter must be greater than or equal to 1.")
  }
  if ( n_cases_per_iter > length(unique(df$caseID)) ) {
    stop(sprintf("Error in active_learning. n_cases_per_iter must be smaller than or equal to %d (number of cases in input dataset).", 
                 length(unique(df$caseID))))
  }
  
  
  # Load helper functions. ----
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/find_crashes.R")
  source("Rscript/find_max_impact_crashes.R")
  source("Rscript/find_non_crashes.R")
  source("Rscript/initialise_grid.R")
  source("Rscript/KL.R")
  source("Rscript/safe_caret_train.R")
  source("Rscript/safe_UPmaxentropy.R")
  source("Rscript/update_predictions.R")
  
  
  # Set some parameters. ----
  
  res <- NULL # To store results.
  n_cases <- length(unique(df$caseID)) # Number of cases in input dataset.
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  n_seq <- cumsum(rep(n_cases_per_iter, niter)) # Cumulative number of baseline scenario simulations. 

  # For optimised sampling:
  # Prediction models will be updated every when n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "optimised" ) {

    n_update <- c(seq(10, 100, 10), seq(150, 500, 50), seq(600, 1000, 100), seq(1500, 5000, 500), seq(6000, 10000, 1000))
    model_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, 0) > n_update[ix] & c(0, n_seq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    model_update_iterations <- as.numeric(na.omit(model_update_iterations))
    model_update_iterations <- unique(model_update_iterations[model_update_iterations > 1])

    if ( verbose ) {
      print(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[model_update_iterations - 1], collapse = ", ")))
    }
    
  }
  
  
  # If bootstrap is used.
  if ( nboot > 0 & niter * n_cases_per_iter >= 10) {
    
    n_update <- seq(10, niter * n_cases_per_iter, 10)
    boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
    
    if ( verbose ) {
      print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[boot_update_iterations], collapse = ", ")))
    }
    
  }
  
 
  # Initialise labelled and unlabelled datasets. ----
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid, use_logic)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 

  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { print(sprintf("Iteration %d", i)) }
    
    
    # If use_logic = TRUE:
    if ( use_logic & nrow(new_sample) > 0 ) {
      
      # Find all known non-crashes in unlabelled dataset.
      ix <- find_non_crashes(new_sample, unlabelled)
      
      unlabelled %<>% 
        mutate(non_crash0 = ifelse(row_number() %in% ix$non_crashes0, 1, non_crash0),
               non_crash1 = ifelse(row_number() %in% ix$non_crashes1, 1, non_crash1),
               sim_count0 = ifelse(row_number() %in% ix$non_crashes0, 0, sim_count0),
               sim_count1 = ifelse(row_number() %in% ix$non_crashes1, 0, sim_count1)) %>%
        filter(!(row_number() %in% ix$non_crashes0)) # Remove certainty non-crashes from unlabelled set.
      
      # Find all known crashes in unlabelled dataset.
      ix <- find_crashes(new_sample, unlabelled)
      
      unlabelled %<>%
        mutate(crash0 = ifelse(row_number() %in% ix$crashes0, 1, crash0),
               crash1 = ifelse(row_number() %in% ix$crashes0, 1, crash1)) 
      
      # Find all known maximal impact speed crashes in unlabelled dataset.
      ix <- find_max_impact_crashes(new_sample, labelled, unlabelled)
      
      unlabelled %<>%
        mutate(max_impact0 = ifelse(row_number() %in% ix$max_impact_crashes0, 1, max_impact0),
               max_impact1 = ifelse(row_number() %in% ix$max_impact_crashes1, 1, max_impact1),
               sim_count0 = ifelse(row_number() %in% ix$max_impact_crashes0, 0, sim_count0),
               sim_count1 = ifelse(row_number() %in% ix$max_impact_crashes1, 0, sim_count1)) 
      
    } # End use_logic.
    
    
    # Update predictions.
    if ( sampling_method == "optimised" && i %in% model_update_iterations ) {
      
      if ( verbose ) { print("Update predictions.") }
      
      # Calculated predictions.
      pred <- update_predictions(labelled, unlabelled, verbose, plot = FALSE) 

      # Add to unlabelled dataset.
      unlabelled %<>% 
        mutate(collision_prob0_pred = pred$collision_prob0,
               collision_prob1_pred = pred$collision_prob1,
               impact_speed0_pred = pred$impact_speed0_pred, 
               impact_speed_reduction_pred = pred$impact_speed_reduction_pred,
               injury_risk_reduction_pred = pred$injury_risk_reduction_pred,
               rmse_log_impact_speed0 = pred$rmse_log_impact_speed0,
               rmse_impact_speed_reduction = pred$rmse_impact_speed_reduction,
               rmse_injury_risk_reduction = pred$rmse_injury_risk_reduction,
               r2_impact_speed0 = pred$r2_impact_speed0,
               r2_impact_speed_reduciton = pred$r2_impact_speed_reduction,
               r2_injury_risk_reduciton = pred$r2_injury_risk_reduction)
      
    }  # End update predictions.
    
    
    # Calculate sampling probabilities. 
    if ( sampling_method %in% c("SRS", "importance sampling") && 
         use_logic == FALSE ) { # Simple random sampling or ordinary importance sampling.
      
      prob <- calculate_sampling_scheme(unlabelled, 
                                        labelled, 
                                        sampling_method = sampling_method, 
                                        proposal_dist = proposal_dist, 
                                        n_cases = 1)
      
      prob$case_probability <- NULL
      prob$sampling_probability <- sampling::inclusionprobabilities(prob$sampling_probability, 
                                                                    n_cases_per_iter * i)
   
      if ( sampling_method == "SRS" ) { # Simple random sampling.
        
        ix <- rep(0, nrow(unlabelled)) # Binary selection indicator.
        jx <- sample(length(prob$sampling_probability), sum(prob$sampling_probability))
        ix[jx] <- 1
        
      } else if ( sampling_method == "importance sampling" ) { # Importance sampling.

        # Use Multinomial sampling with replacement. 
        # Sampling without replacement (conditional Poisson sampling using 
        # sampling::UPmaxentropy) takes too much time when number of samples get large. 
        ix <- as.numeric(rmultinom(n = 1, 
                                   size = n_cases_per_iter * i, 
                                   prob = prob$sampling_probability))
        
      }
      
      # Note: Multiply by i to correct for division by i under "Update labelled set".
      new_wt <- ix / prob$sampling_probability * i 
      
      labelled <- init$labelled
      
    } else { # Optimised sampling, or simple random sampling/importance sampling with logic.
      
      # Before active learning can start: use importance sampling.
      if ( sampling_method == "optimised" && !exists("pred") ) {
        
        prob <- calculate_sampling_scheme(unlabelled, 
                                          labelled, 
                                          sampling_method = "importance sampling", 
                                          proposal_dist = "pps, size = prior weight", 
                                          target = "NA", 
                                          n_cases = n_cases_per_iter)
        
        
      } else {
        
        # Set RMSEs to NA if models has not (yet) been fitted.
        if ( !exists("pred") ) {
          unlabelled$rmse_log_impact_speed0 <- NA
          unlabelled$rmse_impact_speed_reduction <- NA
          unlabelled$rmse_injury_risk_reduction <- NA
        } 
        
        
        # Calculate sampling scheme.
        prob <- calculate_sampling_scheme(unlabelled, 
                                          labelled, 
                                          sampling_method, 
                                          proposal_dist, 
                                          target, 
                                          n_cases_per_iter)
        
      } 
      
      if ( plot ) {
        plot(unlabelled$eoff, prob$sampling_probability, 
             col = unlabelled$caseID, 
             pch = match(unlabelled$acc, sort(unique(unlabelled$acc))), 
             main = sprintf("Iteration %d", i), 
             bty = "l")
      }
      
      
      # Sample cases.
      cases <- as.numeric(names(table(unlabelled$caseID)))
      prob$case_probability[prob$case_probability >= (1 - 1e-3)] <- 1
      if ( all(prob$case_probability == 1) ) {
        new_cases <- cases
      } else {
        new_cases <- cases[which(UPmaxentropy(prob$case_probability) == 1)]
      }
      
      
      # Sample variations.
      ix <- rep(0, nrow(unlabelled)) # Binary selection indicator.
      for ( j in seq_along(new_cases) ) {
        
        jx <- which(unlabelled$caseID == new_cases[j]) 
        ix[jx] <- as.numeric(rmultinom(n = 1, size = 1, prob = prob$sampling_probability[jx]))
        
      }
      new_wt <- ix / prob$sampling_probability
      new_wt[is.na(new_wt)] <- 0
      
    }
    

    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(old_weight = 0, 
             new_weight = new_wt) %>% 
      filter(new_weight > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, old_weight, new_weight) %>% 
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
    
    
    # Update labelled set.
    labelled <- labelled %>%
      mutate(old_weight = sampling_weight,
             new_weight = 0) %>% 
      add_row(new_sample) %>%
      mutate(sampling_weight = old_weight + (new_weight - old_weight) / i) %>% # Update sampling weights. 
      dplyr::select(-old_weight, -new_weight) %>% 
      group_by(caseID, eoff, acc, eoff_acc_prob, impact_speed0, impact_speed1, injury_risk0, injury_risk1, impact_speed_reduction, injury_risk_reduction) %>% 
      summarise_all(sum) %>% 
      mutate(final_weight = eoff_acc_prob * sampling_weight) %>% 
      ungroup()

    
    # Estimate target quantities.
    crashes <- labelled %>% filter(impact_speed0 > 0 & final_weight > 0)
    effective_number_simulations0 <- effective_number_simulations1 <- n_seq[i]
    actual_number_simulations0 <- sum(labelled$sim_count0)
    actual_number_simulations1 <- sum(labelled$sim_count1)
    
    if ( nrow(crashes) > 0 ) { # If any crashes have been generated.
      
      boot <- boot(crashes, 
                   statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), 
                   R = ifelse(nboot > 0 && i %in% boot_update_iterations, nboot, 0) ) # Run bootstrap every 10th sample.
      
      est <- boot$t0 # Estimates.
      se <- apply(boot$t, 2 , sd) # Standard error of estimates.
      
    } else {
      
      est <- estimate_targets(crashes) # Returns NaN if crashes is empty set.
      se <- rep(NA, length(est))
      
    }
    sqerr <- (est - ground_truth)^2 # Squared error with respect to ground truth.
    names(se) <- paste0(names(est), "_se")
    names(sqerr) <- paste0(names(est), "_sqerr")
    
    # Prediction R-squares.
    if ( sampling_method == "optimised" && exists("pred") ) {
      
      r2 <- tibble(r2_impact_speed0 = pred$r2_impact_speed0,
                   r2_impact_speed_reduction = pred$r2_impact_speed_reduction,
                   r2_injury_risk_reduction = pred$r2_injury_risk_reduction,
                   accuracy_crash0 = pred$accuracy_crash0,
                   accuracy_crash1 = pred$accuracy_crash1)
      
    } else {
      
      r2 <- tibble(r2_impact_speed0 = NA,
                   r2_impact_speed_reduction = NA,
                   r2_injury_risk_reduction = NA,
                   accuracy_crash0 = NA,
                   accuracy_crash1 = NA)
      
    }
      
    
    # Append results.
    newres <- tibble(sampling_method = sampling_method,
                     proposal_dist = proposal_dist,
                     target = target,
                     use_logic = use_logic,
                     n_cases_per_iter = n_cases_per_iter) %>% # Meta-information.
      add_column(iter = i, 
                 neff0 = effective_number_simulations0, 
                 neff1 = effective_number_simulations1, 
                 neff_tot = effective_number_simulations0 + effective_number_simulations1,
                 nsim0 = actual_number_simulations0, 
                 nsim1 = actual_number_simulations1, 
                 nsim_tot = actual_number_simulations0 + actual_number_simulations1) %>% # Iteration history.
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(se)))  %>% # Standard errors.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(impact_speed0_KLdiv = KL(ground_truth["impact_speed0_logmean"], 
                                          ground_truth["impact_speed0_logSD"],
                                          est["impact_speed0_logmean"], 
                                          est["impact_speed0_logSD"])) %>% 
      add_column(r2)

    
    if ( is.null(res) ) {
      res <- newres
    } else {
      res %<>% 
        add_row(newres)
    }
    
  } # End active learning.
  
  return(list(results = res, 
              labelled = labelled, 
              crashes = labelled %>% filter(impact_speed0 > 0)))
  
}
