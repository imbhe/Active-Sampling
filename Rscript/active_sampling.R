################################################################################
#
# active_sampling.R
#
# INPUT:
#
# data: input dataset with variables
#   - 'caseID': ID for original crash event. 
#   - 'eoff': off-road glance after tauinv = 0.2 s (overshot).
#   - 'acc': acceleration (negative value means positive deceleration).
#   - 'eoff_acc_prob': probability of (eoff, acc) pair according to baseline distribution.
#   - 'impact_speed0': impact speed in baseline scenario.
#   - 'impact_speed1': impact speed  in counter factual scenario (i.e., with counter measure such as AEB).                           
#   - 'injury_risk0': injury risk in baseline scenario.
#   - 'injury_risk1': injury risk in counter factual scenario (i.e. with counter measure such as AEB).                           
#
# sampling_method: simple random sampling, importance sampling, or active sampling.
#
# target: target of optimisation, only used when sampling_method = "active sampling".
#
# opt_method: method ("naive", "+ prediction uncertainty", "+ model uncertainty")
#             for finding optimal sampling scheme. 
#             Only used when sampling_method = "active sampling".
#
# use_logic:  Use logical constraints (TRUE or FALSE) to infer regions with certainty outcomes 
#             (no crash or maximal impact speed collision) and avoid sampling in those regions.
#
# batch_size: number of obserations to sample per iteration. 
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


active_sampling <- function(data, 
                            sampling_method = c("simple random sampling", 
                                                "importance sampling", 
                                                "active sampling"), 
                            proposal_dist = c("NA", # Only used when sampling_method = "importance sampling", "NA" otherwise.
                                              "density sampling", 
                                              "severity sampling"), 
                            target = c("NA", # Only used when sampling_method = "active sampling", "NA" otherwise.
                                       "impact speed reduction",
                                       "injury risk reduction", 
                                       "crash avoidance"),
                            opt_method = c("NA", # Only used when sampling_method = "active sampling", "NA" otherwise.
                                           "naive", 
                                           "+ prediction uncertainty", 
                                           "+ model uncertainty"),
                            use_logic = FALSE, # TRUE or FALSE. 
                            batch_size = 1,
                            niter = 500, 
                            nboot = 100, 
                            verbose = FALSE, # TRUE or FALSE.
                            plot = FALSE) { # TRUE or FALSE.
  
  
  # Make sure packages are loaded. ----
  require("boot")
  require("caret")
  require("magrittr")
  require("ranger")
  require("stringr")
  require("tidyverse")
  
  
  # Calculate variables. ----
  maximpact <- data %>% 
    group_by(caseID) %>% 
    summarise(impact_speed_max0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() %>% 
    dplyr::select(caseID, impact_speed_max0)
  
  data %<>% 
    mutate(impact_speed_reduction = impact_speed0 - impact_speed1,
           injury_risk_reduction = injury_risk0 - injury_risk1) %>%  
    left_join(maximpact, by = "caseID")

  
  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)
  opt_method <- match.arg(opt_method)
  
  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and proposal_dist = %s not allowed.", 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "active sampling".
  if ( sampling_method != "active sampling" & target != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and target = %s not allowed.", 
                 sampling_method, target))
  } 
  
  # opt_method should be "NA" when sampling_method not equal to "active sampling".
  if ( sampling_method != "active sampling" & opt_method != "NA") { 
    stop(sprintf("Error in active_learning. sampling_method = %s and opt_method = %s not allowed.", 
                 sampling_method, opt_method))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop("Error in active_learning. sampling_method = importance sampling and proposal_dist = NA not allowed.")
  }

  # opt_method must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & opt_method == "NA" ) {
    stop("Error in active_learning. sampling_method = optimised and opt_method = NA not allowed.")
  }
  
  # target must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & target == "NA" ) {
    stop("Error in active_learning. sampling_method = optimised and target = NA not allowed.")
  }
  
  # batch_size should be integer between 1 and number of cases in input dataset.
  batch_size <- round(batch_size)
  if ( batch_size < 1 ) {
    stop("Error in active_learning. batch_size must be greater than or equal to 1.")
  }

  # Load helper functions. ----
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/find_crashes.R")
  source("Rscript/find_max_impact_crashes.R")
  source("Rscript/find_non_crashes.R")
  source("Rscript/initialise_grid.R")
  source("Rscript/safe_caret_train.R")
  source("Rscript/update_predictions.R")
  
  
  # Set some parameters. ----
  
  res <- NULL # To store results.
  n_cases <- length(unique(df$caseID)) # Number of cases in input dataset.
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  n_seq <- cumsum(rep(batch_size, niter)) # Cumulative number of baseline scenario simulations. 


  # For optimised sampling:
  # Prediction models will be updated every when n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "active sampling" ) {

    n_update <- c(seq(10, 100, 10), seq(125, 500, 25), seq(550, 1000, 50), seq(1100, 2000, 100), seq(2200, 5000, 200), seq(5500, 10000, 500))
    model_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, 0) > n_update[ix] & c(0, n_seq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    model_update_iterations <- as.numeric(na.omit(model_update_iterations))
    model_update_iterations <- unique(model_update_iterations[model_update_iterations > 1])

    if ( verbose ) {
      print(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[model_update_iterations - 1], collapse = ", ")))
      cat("\n")
    }
    
  }
  
  
  # If bootstrap is used.
  if ( nboot > 0 & niter * batch_size >= 10) {
    
    n_update <- seq(10, niter * batch_size, 10)
    boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
    
    if ( verbose ) {
      print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[boot_update_iterations], collapse = ", ")))
      cat("\n")
    }
    
  }
  
  # If plots should be produced.
  if ( plot ) {
    
    n_update <- seq(100, niter * batch_size, 100)
    plot_iter <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    plot_iter <- unique(as.numeric(na.omit(plot_iter)))  }
  
    else {plot_iter <- 0}
  
  if ( verbose ) {
      print(sprintf("Plotting predictions and sampling schemes at iteration %s", paste(plot_iter, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[plot_iter], collapse = ", ")))
      cat("\n")
    }  
    


  
 
  # Initialise labelled and unlabelled datasets. ----
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 

  if ( sampling_method == "active sampling" ) {

    # Certainty selections.
    labelled %<>% 
      mutate(sim_count0 = 1,
             sim_count1 = 1,
             sampling_weight = 1, 
             final_weight = eoff_acc_prob * sampling_weight)

    # Remove certainty selections from sampling frame.
    unlabelled %<>% 
      left_join(grid %>% mutate(init = 1), by = c("eoff", "acc")) %>% 
      filter(is.na(init)) %>% 
      dplyr::select(-init)
    
  } 
  
  if ( sampling_method == "importance sampling" & proposal_dist == "severity sampling" ) {
    
    # Number of simulations needed for initialisation.
    labelled$sim_count0 <- 1

  }
  
  n_seq <- n_seq + sum(labelled$sim_count0)

  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { print(sprintf("Iteration %d", i)) }
    
    
    # Logic. ---
    
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
    
    # Find all known non-crashes in unlabelled dataset.
    ix <- find_non_crashes(new_sample, unlabelled)
    
    unlabelled %<>% 
      mutate(non_crash0 = ifelse(row_number() %in% ix$non_crashes0, 1, non_crash0),
             non_crash1 = ifelse(row_number() %in% ix$non_crashes1, 1, non_crash1),
             sim_count0 = ifelse(row_number() %in% ix$non_crashes0, 0, sim_count0),
             sim_count1 = ifelse(row_number() %in% ix$non_crashes1, 0, sim_count1)) 
    
    # If use_logic (elimination) = TRUE. ----
    if ( use_logic ) {
      
      # Remove certainty non-crashes from unlabelled set.
      unlabelled %<>% 
        filter(!(row_number() %in% ix$non_crashes0)) 
      
    } 
    
    
    # Update predictions. ----
    if ( sampling_method == "active sampling" && i %in% model_update_iterations ) {
      
      if ( verbose ) { print("Update predictions.") }
      
      # Calculated predictions.
      pred <- update_predictions(labelled, unlabelled, target, plot = plot & i %in% plot_iter, iter = i) 
      
      # Prediction R-squares and accuracies.
      r2 <- list(impact_speed0 = pred$r2_impact_speed0,
                 impact_speed_reduction = pred$r2_impact_speed_reduction,
                 injury_risk_reduction = pred$r2_injury_risk_reduction,
                 accuracy_crash0 = pred$accuracy_crash0,
                 accuracy_crash1 = pred$accuracy_crash1)

      # Add to unlabelled dataset.
      unlabelled %<>% 
        mutate(collision_prob0_pred = pred$collision_prob0,
               collision_prob1_pred = pred$collision_prob1,
               impact_speed0_pred = pred$impact_speed0_pred, 
               impact_speed_reduction_pred = pred$impact_speed_reduction_pred,
               injury_risk_reduction_pred = pred$injury_risk_reduction_pred,
               sigma_impact_speed_reduction = pred$rmse_impact_speed_reduction,
               sigma_injury_risk_reduction = pred$rmse_injury_risk_reduction,
               sigma_collision1 = sqrt(collision_prob1_pred * (1 - collision_prob1_pred)))
      
    }  # End update predictions.
    
    
    # Calculate sampling probabilities. ----

    # Set R-squares to NA if sampling method is not equal to "active sampling" 
    # or if prediction models for optimised sampling has not (yet) been fitted.
    if ( sampling_method != "active sampling" | !exists("pred") ) {
      r2 <- list(impact_speed0 = NA_real_,
                 impact_speed_reduction = NA_real_,
                 injury_risk_reduction = NA_real_,
                 accuracy_crash0 = NA_real_,
                 accuracy_crash1 = NA_real_)
    }  
    
    
    # Sets estimates to NA if target quantities have not (yet) been estimated.
    if ( !exists("est") ) {
      est <- estimate_targets(labelled, weightvar = "eoff_acc_prob")
      est[1:length(est)] <- NA
    }
    
    # Calculate sampling scheme.
    prob <- calculate_sampling_scheme(unlabelled, 
                                      labelled, 
                                      sampling_method, 
                                      proposal_dist, 
                                      target,
                                      opt_method, 
                                      est = as.list(est),
                                      r2 = r2)
    
    # If predictions have been updated: update previous estimate of 'size'.
    # Only use with optimised sampling.
    if ( sampling_method == "active sampling" && i %in% (model_update_iterations[-1] - 1) ) {
      unlabelled$size <- prob$size
    }
    

    if ( sampling_method == "active sampling" & plot & i %in% c(1, plot_iter) ) {
     
      unlabelled %>% 
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = prob$sampling_probability)) +
        scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
        labs(x = "OEOFF (s)",
             y = bquote('Maximal deceleration '(km/s^2)),
             fill = "Sampling probability") +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ActiveSamplingScheme_%s_%s_2D_Iter%d.png", 
                          target %>% str_to_title() %>% str_remove_all(" "), 
                          opt_method %>% str_to_title() %>% str_remove_all(" "), 
                          i)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      unlabelled %>% 
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_point(aes(x = eoff, y = prob$sampling_probability, colour = -acc)) +
        scale_colour_continuous(type = "viridis") +
        labs(x = "OEOFF (s)",
             y = "Sampling probability",
             colour = bquote('Maximal deceleration '(km/s^2))) +
        facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Output/ActiveSamplingScheme_%s_%s_1D_Iter%d.png", 
                          target %>% str_to_title() %>% str_remove_all(" "), 
                          opt_method %>% str_to_title() %>% str_remove_all(" "), 
                          i)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
      
    }
    
 
    # Sample new instances
    new_wt <- as.numeric(rmultinom(n = 1, size = batch_size, prob = prob$sampling_probability)) / 
      (batch_size * prob$sampling_probability)

    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(old_weight = 0, 
             new_weight = new_wt) %>% 
      filter(new_weight > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, old_weight, new_weight, iter) %>% 
      mutate(iter = i)%>%
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
   
    # Update labelled set.
    bwt <- 1 / i # Batch weight is 1/i since all n_t are equal. 
    labelled %<>% 
      mutate(old_weight = sampling_weight,
             new_weight = ifelse(sampling_weight == 1, 1, 0)) %>% # Sampling weights set to 1 for certainty selection in initialisation step.
      add_row(new_sample) %>% # Add new sample.
      mutate(sampling_weight = old_weight + bwt * (new_weight - old_weight)) %>% # Update sampling weights. 
      dplyr::select(-old_weight, -new_weight) %>% 
      group_by(caseID, eoff, acc, eoff_acc_prob, impact_speed0, impact_speed1, injury_risk0, injury_risk1, impact_speed_reduction, injury_risk_reduction, impact_speed_max0) %>% 
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
      lower <- est - qnorm(0.975) * se # Confidence limits
      upper <- est + qnorm(0.975) * se
      
    } else {
      
      est <- estimate_targets(crashes) # Returns NaN if crashes is empty set.
      se <- lower <- upper <- rep(NA, length(est))
      
    }
    sqerr <- (est - ground_truth)^2 # Squared error with respect to ground truth.
    cov <- as.numeric(lower < ground_truth & ground_truth < upper)
    names(se) <- paste0(names(est), "_se")
    names(sqerr) <- paste0(names(est), "_sqerr")
    names(cov) <- paste0(names(est), "_ci_cover")
    
    # Prediction R-squares.
    r2_tbl <- as_tibble(r2)
    names(r2_tbl) <- c("r2_impact_speed0", "r2_impact_speed_reduction", "r2_injury_risk_reduction", "accuracy_crash0", "accuracy_crash1")

    # Append results.
    newres <- tibble(sampling_method = sampling_method, # Meta-information.
                     proposal_dist = proposal_dist,
                     target = target,
                     opt_method = opt_method,
                     use_logic = use_logic,
                     batch_size = batch_size,
                     labelled_mean_impact_speed0 = sum(labelled$impact_speed0*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_impact_speed1 = sum(labelled$impact_speed1*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_injury_risk0 = sum(labelled$injury_risk0*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_injury_risk1 = sum(labelled$injury_risk1*labelled$eoff_acc_prob)/sum(labelled$eoff_acc_prob),
                     labelled_mean_crash_avoidance = sum(crashes$impact_speed1 == 0)*sum(crashes[crashes$impact_speed1 == 0,]$eoff_acc_prob)/
                       sum(crashes$impact_speed0 > 0)/sum(crashes[crashes$impact_speed0 > 0,]$eoff_acc_prob)) %>% 
      add_column(iter = i, # Iteration history.
                 neff0 = effective_number_simulations0, 
                 neff1 = effective_number_simulations1, 
                 neff_tot = effective_number_simulations0 + effective_number_simulations1,
                 nsim0 = actual_number_simulations0, 
                 nsim1 = actual_number_simulations1, 
                 nsim_tot = actual_number_simulations0 + actual_number_simulations1) %>% 
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(se)))  %>% # Standard errors.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(as_tibble(as.list(cov))) %>% # Confidence interval coverage.
      add_column(r2_tbl) # Prediction R-squared and accuracy.

    
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
