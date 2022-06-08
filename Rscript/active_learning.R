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
# sampling_method: importance sampling scheme (uniform, proportional to 'size', or optimised).
#
# target: target of optimisation, only used when sampling_method = "optimised".
#
# reduce_simulations_by_logic: Use logical constraints to infer regions with certainty outcomes 
#                              (no crash or maximal impact speed collision) 
#                              to avoid sampling in those regions.
#
# num_cases_per_iteration: number of cases to sample from per iteration. 
#
# niter: number of iterations.
#
# nboot: number of bootstrap replicates used to calculate confidence intervals.
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
                            reduce_simulations_by_logic = c(TRUE, FALSE), 
                            num_cases_per_iteration = 1,
                            niter = 500, 
                            nboot = 100) {
  
  # Make sure packages are loaded.
  require("boot")
  require("glmnet")
  require("magrittr")
  require("randomForest")
  require("tidyverse")
  
  # Check input parameters.
  sampling_method <- match.arg(sampling_method)
  target <- match.arg(target)
  if ( sampling_method != "optimised " ) { 
    target = "none" 
  }
  
  # num_cases_per_iteration should be integer between 1 and number of cases in input data set.
  num_cases_per_iteration <- round(num_cases_per_iteration)
  num_cases_per_iteration <- max(c(num_cases_per_iteration, 1))
  num_cases_per_iteration <- min(c(num_cases_per_iteration, length(unique(data$caseID))))
  
  
  # Load helper functions.
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/find_crashes.R")
  source("Rscript/find_max_impact_crashes.R")
  source("Rscript/find_non_crashes.R")
  source("Rscript/initialise_grid.R")
  source("Rscript/safe_cv_glmnet.R")
  source("Rscript/safe_random_forest.R")
  source("Rscript/update_predictions.R")
  
  # To store results.
  res <- NULL
  
  # Calculate target quantities on full data.
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob")
  
 
  # Initialise on grid.
  grid <- crossing(eoff = range(data$eoff), acc = range(data$acc)) %>% # Corner points. 
    add_row(eoff = quantile(data$eoff, 0.5, type = 1), 
            acc = quantile(data$acc, 0.5, type = 1)) %>% # Add centre point.
    mutate(sim_count0 = 1, 
           sim_count1 = 1)
  
  init <- initialise_grid(data, grid)
  
  labelled <- init$labelled
  unlabelled <- init$unlabelled

  
  # Iterate
  new_sample <- labelled 
  collision_counter <- 1
  for ( i in 1:niter ) {
    
    print(sprintf("Iteration %d", i))
    
    
    # If reduce_simulations_by_logic = TRUE:
    if ( reduce_simulations_by_logic & nrow(new_sample) > 0 ) {
      
      # Find all known non-crashes in unlabelled dataset.
      ix <- find_non_crashes(new_sample, unlabelled)
      
      unlabelled %<>% 
        mutate(non_crash0 = ifelse(row_number() %in% ix$non_crashes0, 1, non_crash0),
               non_crash1 = ifelse(row_number() %in% ix$non_crashes1, 1, non_crash1),
               sim_count0 = ifelse(row_number() %in% ix$non_crashes0, 0, sim_count0),
               sim_count1 = ifelse(row_number() %in% ix$non_crashes1, 0, sim_count1)) 

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
        
    } # End reduce_simulations_by_logic.
  
  
    # Update predictions for cases with new data.
    for ( j in unique(new_sample$caseID) ) {

      # Baseline scenario.
      pred0 <- update_predictions(labelled %>% filter(caseID == j), 
                                  unlabelled %>% filter(caseID == j),
                                  yvar = "impact_speed0") 
      
      # With counter measure.
      pred1 <- update_predictions(labelled %>% filter(caseID == j), 
                                  unlabelled %>% filter(caseID == j),
                                  yvar = "impact_speed1") 
      
      # Add to unlabelled data set.
      unlabelled_j <- unlabelled %>% 
        filter(caseID == j) %>% 
        mutate(collision_prob0_pred = pred0$collision_prob,
               collision_prob1_pred = pred1$collision_prob,
               impact_speed0_pred = pred0$impact_speed_pred, 
               impact_speed1_pred = pred1$impact_speed_pred,
               injury_risk0_pred = (1 + exp(-(-5.35 + 0.11 * impact_speed0_pred / 2)))^(-1),
               injury_risk1_pred = (1 + exp(-(-5.35 + 0.11 * impact_speed1_pred / 2)))^(-1),
               injury_risk0_pred = ifelse(impact_speed0_pred > 0, injury_risk0_pred, 0), # Set injury risk to zero if no collision.
               injury_risk1_pred = ifelse(impact_speed1_pred > 0, injury_risk1_pred, 0))
    
      ix <- which(unlabelled$caseID == j)
      unlabelled[ix, ] <- unlabelled_j 
      
    } # End update predictions.
    
    
    # Calculate sampling probabilities. 
    prob <- calculate_sampling_scheme(unlabelled, sampling_method, target, num_cases_per_iteration)
    
    # Sample cases.
    cases <- as.numeric(names(table(unlabelled$caseID)))
    new_cases <- cases[which(UPmaxentropy(prob$case_probability) == 1)]
    
    # Sample variations.
    ix <- rep(0, nrow(unlabelled)) # Binary selection indicator.
    for ( j in seq_along(new_cases) ) {
      
      jx <- which(unlabelled$caseID == new_cases[j]) 
      ix[jx] <- as.numeric(rmultinom(n = 1, size = 1, prob = prob$sampling_probability[jx]))
      
    }
    new_wt <- ix / prob$sampling_probability
    new_wt[is.na(new_wt)] <- 0
    
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(sampling_weight = 0, 
             new_wt = new_wt) %>% 
      filter(new_wt > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, sampling_weight, new_wt) %>% 
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
 
    
    # Update labelled and unlabelled sets.
    labelled <- labelled %>%
      mutate(new_wt = 1) %>% # Re-query labelled data points with probability 1. 
      add_row(new_sample) %>%
      mutate(sampling_weight = sampling_weight + (new_wt - sampling_weight) / collision_counter, 
             final_weight = eoff_acc_prob * sampling_weight) %>%
      dplyr::select(-new_wt)
    
    unlabelled %<>%
      mutate(new_wt = new_wt) %>% 
      filter(new_wt <= 0) %>% 
      dplyr::select(-new_wt)
    
    
    # Estimate target quantities.
    crashes <- labelled %>% filter(impact_speed0 > 0)
    effective_sample_size0 <- effective_sample_size1 <- nrow(labelled)
    number_simulations0 <- sum(labelled$sim_count0)
    number_simulations1 <- sum(labelled$sim_count1)
    boot <- boot(crashes, statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), R = nboot)
    est <- boot$t0
    se <- apply(boot$t, 2 , sd) # Standard error of estimator.
    sqerr <- (est - ground_truth)^2 # Squared error with respect to ground truth.
    names(se) <- paste0(names(est), "_se")
    names(sqerr) <- paste0(names(est), "_sqerr")

    newres <- tibble(samping_method = sampling_method,
                     target = target,
                     reduce_simulations_by_logic = reduce_simulations_by_logic,
                     num_cases_per_iteration = num_cases_per_iteration) %>% # Meta-information.
      add_column(iter = i, 
                 neff = effective_sample_size0, 
                 nsim = number_simulations0 + number_simulations1, 
                 nsim0 = number_simulations0, 
                 nsim1 = number_simulations1) %>% # Iteration history.
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(se)))  %>% # Standard errors.
      add_column(as_tibble(as.list(sqerr))) # Squared errors.
    
    if ( is.null(res) ) {
      res <- newres
    } else {
      res %<>% 
        add_row(newres)
    }
    
    
    # Increase counter if at least one new crash has been generated in baseline scenario.
    collision_counter <- collision_counter + (any(new_sample$impact_speed0 > 0))
    
    
  } # End active learning.

  return(list(results = res, 
              labelled = labelled, 
              crashes = labelled %>% filter(impact_speed0 > 0)))
  
}