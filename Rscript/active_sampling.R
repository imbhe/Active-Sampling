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
# paper: Alters a few options that are specific for the "stats" and "applied" paper. 
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
                            paper = c("stats", "applied"),
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
  
  
  # Calculate some variables. ----
  maximpact <- data %>% 
    group_by(caseID) %>% 
    summarise(impact_speed_max0 = max(impact_speed0, na.rm = TRUE), .groups = "keep") %>% 
    ungroup() %>% 
    dplyr::select(caseID, impact_speed_max0)
  
  data %<>% 
    mutate(impact_speed_reduction = impact_speed0 - impact_speed1,
           injury_risk_reduction = injury_risk0 - injury_risk1,
           crash_avoidance = as.numeric( (impact_speed0 > 0) * (impact_speed1 == 0)) ) %>%  
    left_join(maximpact, by = "caseID")
  
  
  # Plot baseline impact speed distribution. ----
  
  # 1D.
  if (plot) {
    ggplot(data %>% filter(caseID <= 42)) + # Plot 42 cases on 7x6 grid.
      geom_point(aes(x = eoff, y = impact_speed0, colour = -acc)) +
      scale_colour_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = "Baseline impact speed (km/h)",
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
    
    filename <- sprintf("Output/BaselinImpactSpeed_1D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(data %>% filter(caseID <= 42)) + # Plot 42 cases on 7x6 grid.
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed0)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline impact speed (km/h)") +
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
    
    filename <- sprintf("Output/BaselinImpactSpeed_2D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
  }
  
  # Check input parameters. ----
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)
  opt_method <- match.arg(opt_method)
  paper <- match.arg(paper)
  
  # proposal_dist should be "NA" when sampling_method not equal to "importance sampling".
  if ( sampling_method != "importance sampling" & proposal_dist != "NA") { 
    stop(sprintf('Sampling_method = "%s" and proposal_dist = "%s" not allowed.', 
                 sampling_method, proposal_dist))
  } 
  
  # target should be "NA" when sampling_method not equal to "active sampling".
  if ( sampling_method != "active sampling" & target != "NA") { 
    stop(sprintf('Sampling_method = "%s" and target = "%s" not allowed.', 
                 sampling_method, target))
  } 
  
  # opt_method should be "NA" when sampling_method not equal to "active sampling".
  if ( sampling_method != "active sampling" & opt_method != "NA") { 
    stop(sprintf('Sampling_method = "%s" and opt_method = "%s" not allowed.', 
                 sampling_method, opt_method))
  } 
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop('Sampling_method = "importance sampling" and proposal_dist = "NA" not allowed.')
  }
  
  # opt_method must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & opt_method == "NA" ) {
    stop('Sampling_method = "optimised" and opt_method = "NA" not allowed.')
  }
  
  # target must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & target == "NA" ) {
    stop('Sampling_method = "optimised"" and target = "NA" not allowed.')
  }
 
  # batch_size should be integer between 1 and number of cases in input dataset.
  batch_size <- round(batch_size)
  if ( batch_size < 1 ) {
    stop("Batch_size must be greater than or equal to 1.")
  }
  
  # Load helper functions. ----
  source("Rscript/calculate_sampling_scheme.R")
  source("Rscript/estimate_targets.R")
  source("Rscript/estimate_totals.R")
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
  totals <- matrix(0, nrow = niter, ncol = 4)  # To store estimates of totals per iteration.
  t_y <- rep(0, 4) # To store pooled estimates of totals.
  covest_classic <- matrix(0, nrow = 4, ncol = 4) # To store covariance matrix estimates per iteration.
  
  
  # For optimised sampling:
  # Prediction models will be updated n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "active sampling" & niter * batch_size >= 10 ) {
    
    n_update <- c(seq(10, 100, 10), seq(125, 500, 25), seq(550, 1000, 50), seq(1100, 2000, 100), seq(2200, 5000, 200), seq(5500, 10000, 500))
    model_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, 0) > n_update[ix] & c(0, n_seq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    model_update_iterations <- as.numeric(na.omit(model_update_iterations))
    model_update_iterations <- unique(model_update_iterations[model_update_iterations > 1])
    
    if ( verbose ) {
      print(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[model_update_iterations - 1], collapse = ", ")))
      cat("\n")
    }
    
  } else {
    model_update_iterations <- NA
  }
  
  
  # If bootstrap is used: run every 10th new observation. Find corresponding iterations.
  if ( nboot > 0 & niter * batch_size >= 10) {
    
    n_update <- seq(0, niter * batch_size, 10)[-1]
    boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
    
    if ( verbose & length(n_update) > 0 ) {
      print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[boot_update_iterations], collapse = ", ")))
      cat("\n")
    } 
  } else {
    boot_update_iterations <- NA
  }
  

  # If plots should be produced.
  plot_iter <- NA
  if ( plot ) {
    
    n_update <- seq(0, niter * batch_size, 100)[-1]
    plot_iter <- vapply(1:length(n_update), function(ix) which(c(n_seq, max(n_seq) + 1) >= n_update[ix] & c(0, n_seq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    plot_iter <- unique(as.numeric(na.omit(plot_iter))) 
    
    if ( verbose & length(n_update) > 0 ) {
      print(sprintf("Plotting predictions and sampling schemes at iteration %s", paste(plot_iter, collapse = ", ")))
      print(sprintf("after %s observations", paste(n_seq[plot_iter], collapse = ", ")))
      cat("\n")
    }  
  } 
  plot_iter <- ifelse(length(plot_iter) == 0, NA, plot_iter) # Make sure not empty.

  
  # Initialise labelled and unlabelled datasets. ----
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 
  
  # Update simulation counts.
  if ( sampling_method == "active sampling" | 
       (paper == "stats" 
        & sampling_method == "importance sampling" 
        & proposal_dist == "severity sampling") ) {
    
    # Certainty selections.
    labelled %<>% 
      mutate(sim_count0 = 1,
             sim_count1 = 1)
 
  } else if ( use_logic == TRUE | 
              (sampling_method == "importance sampling" & 
              proposal_dist == "severity sampling") ) {
    
    # Number of simulations needed for initialisation.
    labelled$sim_count0 <- 1
    
  }
  
  n_seq0 <- n_seq + sum(labelled$sim_count0)
  n_seq1 <- n_seq + sum(labelled$sim_count1)

  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { print(sprintf("Iteration %d", i)) }
    
    
    # Logic (only for applied paper). ---
    if ( paper == "applied" ) {
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
    }
    
    
    # Update predictions. ----
    if ( sampling_method == "active sampling" && i %in% model_update_iterations ) {
      
      if ( verbose ) { print("Update predictions.") }
      
      # Calculated predictions.
      pred <- update_predictions(labelled, unlabelled, target, use_logic, plot = plot & i %in% plot_iter, iter = i) 
      
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
      est <- estimate_targets(labelled)
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
    
    
    # Plot. ----
    if ( sampling_method == "active sampling" & plot & i %in% c(1, plot_iter) ) {
      
      unlabelled %>% 
        mutate(sampling_probability = prob$sampling_probability)%>%
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = sampling_probability)) +        
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
        mutate(sampling_probability = prob$sampling_probability)%>%
        filter(caseID <= 42) %>% # Plot 42 cases on 7x6 grid. 
        ggplot() +
        geom_point(aes(x = eoff, y = sampling_probability, colour = -acc)) +
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
    
    
    # Sample new instances. ----
    
    # Sample from multinomial distribution.
    n_hits <- as.numeric(rmultinom(n = 1, size = batch_size, prob = prob$sampling_probability))
    
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(batch_size = batch_size,
             pi = prob$sampling_probability,
             mu = batch_size * pi,
             n_hits = n_hits,
             sampling_weight = n_hits / mu, 
             batch_weight = batch_size / n_seq[i]) %>% 
      filter(n_hits > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, sim_count0, sim_count1, iter, batch_size, batch_weight, pi, mu, n_hits, sampling_weight) %>% 
      mutate(iter = i)%>%
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))

    # Update labelled set.
    labelled %<>% 
      mutate(batch_weight = batch_size / n_seq[i]) %>% # Update batch-weights.
      add_row(new_sample) %>% # Add new sample.
      mutate(final_weight = eoff_acc_prob * batch_weight * sampling_weight) 
 
    
    # Estimate target quantities. ----
    
    bwt <- batch_size / n_seq[i] # Batch weight in current iteration.
    bwts <- diff(c(0, n_seq[1:i])) / n_seq[i] # All batch weights.
    rewt <- c(n_seq[1], n_seq)[i] / n_seq[i] # Re-weight old batch weights by n_1 + ... n_{k-1} / (n_1 + ... + n_k).

    # Estimate totals in current iteration.
    totals[i, ] <- estimate_totals(new_sample %>% 
                                  mutate(final_weight = eoff_acc_prob * sampling_weight), 
                                "final_weight")
    
    # Pooled estimate of totals.
    t_y <- rewt * t_y + bwt * totals[i, ]

    # Pooled estimate of "means among relevant instances".
    est <- estimate_targets(labelled, "final_weight")

    
    # Variance estimation using martingale method. ----
    X <- t(t(totals[1:i,, drop = FALSE]) - t_y)
    cov <- t(X) %*% diag(bwts^2) %*% X
    
    G <- matrix(data = c(1 / t_y[4], 0, 0,-t_y[1] / t_y[4]^2,
                         0, 1 / t_y[4], 0,-t_y[2] / t_y[4]^2,
                         0, 0, 1 / t_y[4],-t_y[3] / t_y[4]^2), 
                byrow = FALSE, nrow = 4, ncol = 3)
    
    se_mart <- sqrt(diag(t(G) %*% cov %*% G))
    
    if ( all(se_mart == 0) ) { # Zero at first iteration. Set to NA. 
      se_mart <- rep(NA, 3)
    }
    
    
    # Variance estimation using classical survey sampling method (Hansen-Hurwitz estimator). ----
    Y <- new_sample %>% 
      mutate(baseline_crash = impact_speed0 > 0) %>% 
      dplyr::select(impact_speed_reduction, injury_risk_reduction, crash_avoidance, baseline_crash) %>%
      as.matrix()
    X <- t((t(Y / new_sample$pi) - t_y)) / batch_size
    n <- batch_size
    
    covest_classic <- rewt^2 * covest_classic + 
      bwt^2 * n / (n - 1) * t(X) %*% diag(new_sample$n_hits * new_sample$eoff_acc_prob^2) %*% X
    
    G <- matrix(data = c(1 / t_y[4], 0, 0,-t_y[1] / t_y[4]^2,
                         0, 1 / t_y[4], 0,-t_y[2] / t_y[4]^2,
                         0, 0, 1 / t_y[4],-t_y[3] / t_y[4]^2), 
                byrow = FALSE, nrow = 4, ncol = 3)
    
    se_classic <- sqrt(diag(t(G) %*% covest_classic %*% G))
  
    
    # Variance estimation using bootstrap method. ----
   
    # If an element is selected multiple times: split into multiple observations.
    # Only counts as one simulation.
    ix <- rep(1:nrow(labelled), labelled$n_hits) # To repeat rows.
    reps <- which(c(1, diff(ix)) == 0) # Find duplicate rows, set corresponding simulation counts to 0.
    crashes <- labelled[ix, ] %>%
      mutate(sampling_weight = 1 / mu, 
             final_weight = eoff_acc_prob * sampling_weight) %>% 
      filter(impact_speed0 > 0 & final_weight > 0)

    # If any crashes have been generated.
    # Run bootstrap at selected iterations (every 10th new observation).
    if ( nrow(crashes) > 0 & i %in% boot_update_iterations ) { 
      boot <- boot(crashes, 
                   statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), 
                   R = nboot) 
      se_boot <- apply(boot$t, 2 , sd) # Standard error of estimates.
    } else {
      se_boot <- rep(NA, length(est))
    }

    # Confidence intervals.
    lower_mart <- est - qnorm(0.975) * se_mart 
    upper_mart <- est + qnorm(0.975) * se_mart
    lower_classic <- est - qnorm(0.975) * se_classic 
    upper_classic <- est + qnorm(0.975) * se_classic
    lower_boot <- est - qnorm(0.975) * se_boot 
    upper_boot <- est + qnorm(0.975) * se_boot
    
    # Confidence intervals cover true value?
    cov_mart <- as.numeric(lower_mart < ground_truth & ground_truth < upper_mart)
    cov_classic <- as.numeric(lower_classic < ground_truth & ground_truth < upper_classic)
    cov_boot <- as.numeric(lower_boot < ground_truth & ground_truth < upper_boot)
    
 
    # Append results. ----
    
    # Squared error from ground truth. 
    sqerr <- (est - ground_truth)^2 
    
    # Prediction R-squares.
    r2_tbl <- as_tibble(r2)
    
    # Add names.
    names(se_mart) <- paste0(names(est), "_se_mart")
    names(se_classic) <- paste0(names(est), "_se_classic")
    names(se_boot) <- paste0(names(est), "_se_boot")
    names(cov_mart) <- paste0(names(est), "_ci_cover_mart")
    names(cov_classic) <- paste0(names(est), "_ci_cover_classic")
    names(cov_boot) <- paste0(names(est), "_ci_cover_boot")
    names(sqerr) <- paste0(names(est), "_sqerr")
    names(r2_tbl) <- c("r2_impact_speed0", "r2_impact_speed_reduction", "r2_injury_risk_reduction", "accuracy_crash0", "accuracy_crash1")
    
    newres <- tibble(sampling_method = sampling_method, # Meta-information.
                     proposal_dist = proposal_dist,
                     target = target,
                     opt_method = opt_method,
                     use_logic = use_logic,
                     batch_size = batch_size) %>% 
      add_column(iter = i, # Iteration history.
                 neff0 = n_seq0[i], 
                 neff1 = n_seq1[i], 
                 neff_tot = n_seq0[i] + n_seq1[i],
                 nsim0 = sum(labelled$sim_count0), 
                 nsim1 = sum(labelled$sim_count1), 
                 nsim_tot = sum(labelled$sim_count0) + sum(labelled$sim_count1),
                 n_crashes = nrow(crashes)) %>% 
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(as_tibble(as.list(se_mart)))  %>% # Standard errors.
      add_column(as_tibble(as.list(se_classic)))  %>% 
      add_column(as_tibble(as.list(se_boot)))  %>% 
      add_column(as_tibble(as.list(cov_mart))) %>% # Confidence interval coverage.
      add_column(as_tibble(as.list(cov_classic))) %>% 
      add_column(as_tibble(as.list(cov_boot))) %>% 
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
