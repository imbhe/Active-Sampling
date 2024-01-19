################################################################################
#
# active_sampling.R
#
# INPUT:
#
# data: input dataset with variables
#   - 'caseID': ID for original crash event. 
#   - 'eoff': off-road glance duration.
#   - 'acc': minimal acceleration during braking (negative maximal deceleration).
#   - 'eoff_acc_prob': probability of (eoff, acc) pair according to baseline distribution.
#   - 'impact_speed0': impact speed in baseline scenario.
#   - 'impact_speed1': impact speed  in counter factual scenario (i.e., with counter measure such as AEB).
#   - 'impact_speed_max0': maximal possible impact speed per caseID.
#
# sampling_method: simple random sampling, importance sampling, or active sampling.
#
# target: target of optimisation. Only used when sampling_method = "active sampling".
#
# opt_method: method for finding optimal sampling scheme. Only used when sampling_method = "active sampling".
#
# batch_size: number of observations to sample per iteration. 
#
# niter: number of iterations.
#
# nboot: number of bootstrap replicates used to calculate bootstrap confidence intervals.
#
# verbose: should iteration progress be printed to console? (TRUE/FALSE).
#
# plot: should plots of predicted outcomes and sampling probabilities be produced? (TRUE/FALSE).
#
#
# OUTPUT: 
#
# List of three datasets:
#   - results: meta information of simulations, iteration history, estimates with standard errors and squared errors, etc.
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
                                       "crash avoidance"),
                            opt_method = c("default", 
                                           "naive"),
                            batch_size = 1,
                            niter = 500, 
                            nboot = 500, 
                            verbose = FALSE, # TRUE or FALSE.
                            plot = FALSE,# TRUE or FALSE.
                            prediction_model_type = c("rg", "xg_boost", "knn")) { # Only used when sampling_method = "active sampling".
  
  
  # Make sure packages are loaded. ----
  require("boot")
  require("caret")
  require("magrittr")
  require("ranger")
  require("stringr")
  require("tidyverse")
  require("xgboost")
  require("ROSE")
  require("Matrix")
  
  
  # Check input parameters. ----
  
  sampling_method <- match.arg(sampling_method)
  proposal_dist <- match.arg(proposal_dist)
  target <- match.arg(target)
  opt_method <- match.arg(opt_method)
  prediction_model_type <- match.arg(prediction_model_type)
  
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
  
  # proposal_dist must be specified if sampling_method = "importance sampling".
  if ( sampling_method == "importance sampling" & proposal_dist == "NA" ) {
    stop('Sampling_method = "importance sampling" and proposal_dist = "NA" not allowed.')
  }
  
  # target must be specified if sampling_method = "active sampling".
  if ( sampling_method == "active sampling" & target == "NA" ) {
    stop('Sampling_method = "active sampling"" and target = "NA" not allowed.')
  }
  
  # batch_size should be integer between 1 and number of cases in input dataset.
  batch_size <- round(batch_size)
  if ( batch_size < 1 ) {
    stop("Batch_size must be greater than or equal to 1.")
  }
  
  
  # Load helper functions. ----
  
  source("Application/Rscript/calculate_sampling_scheme.R")
  source("Application/Rscript/estimate_targets.R")
  source("Application/Rscript/estimate_totals.R")
  source("Application/Rscript/initialise_grid.R")
  source("Application/Rscript/safe_caret_train.R")
  source("Application/Rscript/update_predictions.R")
  
  
  # Set some parameters. ----
  
  res <- NULL # To store results.
  ground_truth <- estimate_targets(data, weightvar = "eoff_acc_prob") # Calculate target quantities on full data.
  nseq <- cumsum(rep(batch_size, niter)) # Cumulative number of baseline scenario simulations. 
  totals <- matrix(0, nrow = niter, ncol = 3)  # To store estimates of totals per iteration.
  t_y <- rep(0, 3) # To store pooled estimates of totals.
  covest_classic <- matrix(0, nrow = 3, ncol = 3) # To store covariance matrix estimates per iteration.
  se_boot <- rep(NA, 2)# To store bootstrap standard errors.
  
  # For optimised sampling:
  # Prediction models will be updated n_update observations have been collected.
  # Find corresponding iteration indices model_update_iterations.
  if ( sampling_method == "active sampling" & niter * batch_size >= 10 ) {
    
    n_update <- c(seq(10, 100, 10), seq(125, 500, 25), seq(550, 1000, 50), seq(1100, 2000, 100), seq(2200, 5000, 200), seq(5500, 10000, 500))
    model_update_iterations <- vapply(1:length(n_update), function(ix) which(c(nseq, 0) > n_update[ix] & c(0, nseq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    model_update_iterations <- as.numeric(na.omit(model_update_iterations))
    model_update_iterations <- unique(model_update_iterations[model_update_iterations > 1])
    
    if ( verbose ) {
      print(sprintf("Predictions updated at iterations %s", paste(model_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(nseq[model_update_iterations - 1], collapse = ", ")))
      cat("\n")
    }
    
  } else {
    model_update_iterations <- NA
  }
  
  
  # If bootstrap is used: run every 10th/25th/50th/100th new observation, etc. 
  # Find corresponding iterations.
  if ( nboot > 0 & niter * batch_size >= 10) {
    
    n_update <- c(seq(10, 50, 10), seq(75, 200, 25), seq(250, 500, 50), seq(600, 1000, 100), seq(1250, 2500, 250), seq(3000, 5000, 500), seq(6000, 10000, 1000))
    boot_update_iterations <- vapply(1:length(n_update), function(ix) which(c(nseq, max(nseq) + 1) >= n_update[ix] & c(0, nseq) >= n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    boot_update_iterations <- unique(as.numeric(na.omit(boot_update_iterations)))
    
    if ( verbose & length(n_update) > 0 ) {
      print(sprintf("Bootstrap standard error updated at iterations %s", paste(boot_update_iterations, collapse = ", ")))
      print(sprintf("after %s observations", paste(nseq[boot_update_iterations], collapse = ", ")))
      cat("\n")
    } 
  } else {
    boot_update_iterations <- NA
  }
  
  
  # If plots should be produced.
  plot_iter <- NA
  if ( plot ) {
    
    n_update <- seq(0, niter * batch_size, 100)[-1]
    plot_iter <- vapply(1:length(n_update), function(ix) which(c(nseq, 0) > n_update[ix] & c(0, nseq) > n_update[ix])[1] - 1, FUN.VALUE = numeric(1))
    plot_iter <- as.numeric(na.omit(plot_iter))
    plot_iter <- unique(plot_iter[plot_iter > 1])
    
    if ( verbose & length(n_update) > 0 ) {
      print(sprintf("Plotting predictions and sampling schemes at iteration %s", paste(plot_iter, collapse = ", ")))
      print(sprintf("after %s observations", paste(nseq[plot_iter - 1], collapse = ", ")))
      cat("\n")
    }  
  } 
  plot_iter <- ifelse(length(plot_iter) == 0, NA, plot_iter) # Make sure not empty.

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
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Application/Figures/BaselinImpactSpeed_1D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
    
    # 2D.
    ggplot(data %>% filter(caseID <= 42)) + # Plot 42 cases on 7x6 grid.
      geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed0)) +
      scale_fill_continuous(type = "viridis") +
      labs(x = "OEOFF (s)",
           y = bquote('Maximal deceleration '(km/s^2)),
           fill = "Baseline impact speed (km/h)") +
      facet_wrap(~caseID, ncol = 7, nrow = 6, labeller = labeller(caseID = function(x) "")) + 
      theme_classic() + 
      theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
            legend.direction = "horizontal",
            legend.position = "top",
            legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
            legend.key.width = unit(2, "cm")) +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    filename <- sprintf("Application/Figures/BaselinImpactSpeed_2D.png")
    ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
  }
  
  
  # Initialise labelled and unlabelled datasets. ----
  grid <- tibble(eoff = max(data$eoff), acc = max(data$acc)) 
  init <- initialise_grid(data, grid)
  labelled <- init$labelled 
  unlabelled <- init$unlabelled 
  
  # Cumulative number of observations, including initialisation.
  ntot <- nseq
  if ( sampling_method == "active sampling" | 
       (sampling_method == "importance sampling" 
        & proposal_dist == "severity sampling") ) {
    
    ntot <- ntot + nrow(labelled) # Count initialisation.
  } 
  
  
  # Iterate. ----
  new_sample <- labelled 
  for ( i in 1:niter ) {
    
    # Print iteration number if verbose = TRUE.
    if ( verbose ) { print(sprintf("Iteration %d", i)) }
    
    
    # Update predictions. ----
    if ( sampling_method == "active sampling" && i %in% model_update_iterations ) {
      
      if ( verbose ) { print("Update predictions.") }
      
      # Calculated predictions.
      pred <- update_predictions(labelled, unlabelled, target, verbose = verbose, plot = plot & i %in% plot_iter, iter = i, prediction_model = prediction_model_type) 
      
      # Prediction R-squares and accuracies.
      r2 <- list(impact_speed_reduction = pred$r2_impact_speed_reduction,
                 accuracy_crash0 = pred$accuracy_crash0,
                 accuracy_crash1 = pred$accuracy_crash1)
      
      # Add to unlabelled dataset.
      unlabelled %<>% 
        mutate(collision_prob0_pred = pred$collision_prob0,
               collision_prob1_pred = pred$collision_prob1,
               impact_speed_reduction_pred = pred$impact_speed_reduction_pred,
               sigma_impact_speed_reduction = pred$rmse_impact_speed_reduction,
               sigma_collision1 = sqrt(collision_prob1_pred * (1 - collision_prob1_pred)))
      
    }  # End update predictions.
    
    
    # Calculate sampling probabilities. ----
    
    # Set R-squares to NA if sampling method is not equal to "active sampling" 
    # or if prediction models for optimised sampling has not (yet) been fitted.
    if ( sampling_method != "active sampling" | !exists("pred") ) {
      r2 <- list(impact_speed_reduction = NA_real_,
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
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Application/Figures/ActiveSamplingScheme_%s_%s_2D_Iter%d.png", 
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
        theme_classic() + 
        theme(panel.spacing = unit(0.1, "cm"), # Increase/decrease to increase/reduce horizontal white space between cases
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 0), # Change size to increase/reduce vertical white space between cases. Set to element_blank() for no white space.
              legend.direction = "horizontal",
              legend.position = "top",
              legend.key.height = unit(0.3, "cm"), # Change width and height as necessary.
              legend.key.width = unit(2, "cm")) +
        guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      filename <- sprintf("Application/Figures/ActiveSamplingScheme_%s_%s_1D_Iter%d.png", 
                          target %>% str_to_title() %>% str_remove_all(" "), 
                          opt_method %>% str_to_title() %>% str_remove_all(" "), 
                          i)
      ggsave(filename, width = 160, height = 100, unit = "mm", dpi = 1000)
      
    }
    
    
    # Sample new instances. ----
    
    # Sample from multinomial distribution.
    nhits <- as.numeric(rmultinom(n = 1, size = batch_size, prob = prob$sampling_probability))
    
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(batch_size = batch_size, 
             nhits = nhits,
             pi = prob$sampling_probability,
             mu = batch_size * pi,
             sampling_weight = 1 / mu, 
             batch_weight = batch_size / nseq[i],
             final_weight = eoff_acc_prob * nhits * sampling_weight) %>% 
      filter(nhits > 0) %>% 
      dplyr::select(caseID, eoff, acc, eoff_acc_prob, iter, batch_size, nhits, pi, mu, sampling_weight, batch_weight, final_weight) %>% 
      mutate(iter = i)%>%
      left_join(data, by = c("caseID", "eoff", "acc", "eoff_acc_prob"))
    
    # Update labelled set.
    labelled %<>% 
      mutate(batch_weight = batch_size / nseq[i]) %>% # Update batch-weights.
      add_row(new_sample) %>% # Add new sample.
      mutate(final_weight = eoff_acc_prob * batch_weight * nhits * sampling_weight) 
    
    
    # Estimate target quantities. ----
    
    bwt <- batch_size / nseq[i] # Batch weight in current iteration.
    bwts <- diff(c(0, nseq[1:i])) / nseq[i] # All batch weights.
    rewt <- c(nseq[1], nseq)[i] / nseq[i] # Re-weight old batch weights by n_1 + ... n_{k-1} / (n_1 + ... + n_k).
    
    # Estimate totals in current iteration.
    totals[i, ] <- estimate_totals(new_sample, "final_weight")
    
    # Pooled estimate of totals.
    t_y <- rewt * t_y + bwt * totals[i, ]
    
    # Pooled estimate of "means among relevant instances".
    est <- estimate_targets(labelled, "final_weight")
    
    
    # Variance estimation using martingale method. ----
    X <- t(t(totals[1:i,, drop = FALSE]) - t_y)
    covest_mart <- t(X) %*% diag(bwts^2) %*% X
    
    G <- matrix(data = c(1 / t_y[3], 0, -t_y[1] / t_y[3]^2,
                         0, 1 / t_y[3], -t_y[2] / t_y[3]^2), 
                byrow = FALSE, nrow = 3, ncol = 2)
    
    se_mart <- sqrt(diag(t(G) %*% covest_mart %*% G))

    if ( all(se_mart == 0) | any(is.na(se_mart)) ) { # Zero at first iteration. Set to NA. 
      se_mart <- rep(NA, 2)
    }
    
    # Small sample (few iterations) correction. 
    if ( i > 1 ) {
      se_mart_corr <- se_mart * i / (i - 1)
    } else {
      se_mart_corr <- rep(NA, 2)
    }
    
    
    # Variance estimation using classical survey sampling method (Sen-Yates-Grundy estimator). ----
    Y <- new_sample %>% 
      mutate(baseline_crash = impact_speed0 > 0) %>% 
      dplyr::select(impact_speed_reduction, crash_avoidance, baseline_crash) %>%
      as.matrix()
    n <- batch_size
    X <- with(new_sample, t((t(eoff_acc_prob * Y / mu) - totals[i, ] / n)))
    W <- with(new_sample, diag(nhits, nrow = nrow(X), ncol = nrow(X)))

    covest_classic <- rewt^2 * covest_classic + bwt^2 * n / (n - 1) * t(X) %*% W %*% X 

    G <- matrix(data = c(1 / t_y[3], 0, -t_y[1] / t_y[3]^2,
                         0, 1 / t_y[3], -t_y[2] / t_y[3]^2), 
                byrow = FALSE, nrow = 3, ncol = 2)
    
    se_classic <- sqrt(diag(t(G) %*% covest_classic %*% G))

    
    # Variance estimation using bootstrap method. ----
    
    # If an element is selected multiple times: split into multiple observations/rows.
    ix <- rep(1:nrow(labelled), labelled$nhits) # To repeat rows.
    crashes <- labelled[ix, ] %>%
      mutate(final_weight = eoff_acc_prob * batch_weight * sampling_weight) %>%
      filter(impact_speed0 > 0 & final_weight > 0)
    
    # If any crashes have been generated.
    # Run bootstrap at selected iterations (every 10th new observation).
    if ( nrow(crashes) > 0 & i %in% boot_update_iterations ) { 
      boot <- boot(crashes, 
                   statistic = function(data, ix) estimate_targets(data[ix, ], weightvar = "final_weight"), 
                   R = nboot) 
      se_boot <- apply(boot$t, 2 , sd) # Standard error of estimates.
    }  

    
    # Confidence intervals.
    lower_mart <- est - qnorm(0.975) * se_mart 
    upper_mart <- est + qnorm(0.975) * se_mart
    lower_mart_corr <- est - qnorm(0.975) * se_mart_corr 
    upper_mart_corr <- est + qnorm(0.975) * se_mart_corr
    lower_classic <- est - qnorm(0.975) * se_classic 
    upper_classic <- est + qnorm(0.975) * se_classic
    lower_boot <- est - qnorm(0.975) * se_boot 
    upper_boot <- est + qnorm(0.975) * se_boot
    
    # Confidence intervals cover true value?
    cov_mart <- as.numeric(lower_mart < ground_truth & ground_truth < upper_mart)
    cov_mart_corr <- as.numeric(lower_mart_corr < ground_truth & ground_truth < upper_mart_corr)
    cov_classic <- as.numeric(lower_classic < ground_truth & ground_truth < upper_classic)
    cov_boot <- as.numeric(lower_boot < ground_truth & ground_truth < upper_boot)
    
    
    # Append results. ----
    
    # Squared error from ground truth. 
    sqerr <- (est - ground_truth)^2 
    
    # Prediction R-squares.
    r2_tbl <- as_tibble(r2)
    
    # Add names.
    names(se_mart) <- paste0(names(est), "_se_mart")
    names(se_mart_corr) <- paste0(names(est), "_se_mart_corr")
    names(se_classic) <- paste0(names(est), "_se_classic")
    names(se_boot) <- paste0(names(est), "_se_boot")
    names(cov_mart) <- paste0(names(est), "_ci_cover_mart")
    names(cov_mart_corr) <- paste0(names(est), "_ci_cover_mart_corr")
    names(cov_classic) <- paste0(names(est), "_ci_cover_classic")
    names(cov_boot) <- paste0(names(est), "_ci_cover_boot")
    names(sqerr) <- paste0(names(est), "_sqerr")
    names(r2_tbl) <- c("r2_impact_speed_reduction", "accuracy_crash0", "accuracy_crash1")
    
    newres <- tibble(sampling_method = sampling_method, # Meta-information.
                     proposal_dist = proposal_dist,
                     target = target,
                     opt_method = opt_method,
                     batch_size = batch_size) %>% 
      add_column(iter = i, # Iteration history.
                 total_sample_size = ntot[i]) %>% 
      add_column(as_tibble(as.list(est))) %>% # Estimates.
      add_column(as_tibble(as.list(sqerr))) %>% # Squared errors.
      add_column(as_tibble(as.list(se_mart)))  %>% # Standard errors.
      add_column(as_tibble(as.list(se_mart_corr)))  %>% 
      add_column(as_tibble(as.list(se_classic)))  %>% 
      add_column(as_tibble(as.list(se_boot)))  %>% 
      add_column(as_tibble(as.list(cov_mart))) %>% # Confidence interval coverage.
      add_column(as_tibble(as.list(cov_mart_corr))) %>% 
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
              crashes = labelled %>% filter(impact_speed0 > 0), 
              prob = prob$sampling_probability))
  
}