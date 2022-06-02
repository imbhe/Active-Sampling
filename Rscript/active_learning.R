active_learning <- function(data, optimizefactor = 'Impact_speed' ,niter = 1e2, bsize = 10, crit = "mean", plot = FALSE, plotit = 1:niter, fcount = 1) {
  # INPUT
  # data: input data set with variables 
  #   - 'after': glance duration after tauinv = 0.2 s.
  #   - 'before': (glance duration before tauinv = 0.2 s.
  #   - 't': total glance duration (s).
  #   - 'glance_prob': probability of glance.
  #   - 'ID': ID for specific case. 
  # optimizefactor = 'Impact_speed', 'Injury_reduction' or 'Speed_reduction'
  # niter: number of iterations.
  # bsize: number of datapoints to sample.
  # crit: optimisation criterion. Should be one of "uniform", "mean", "prob", "moments" or "std_moments".
  # plot: should plots be saved (TRUE/FALSE). 
  # plotit: iteration numbers for which plots will be generated.
  # fcount: starting index for figure numbers.
  #
  # OUTPUT: list of three datasets:
  #   - all: all 'labelled' observations (i.e. for which the outcome has been observed).
  #   - crashes: all generated crashes. 
  #   - noncrashes: all generated non-crashes.
  #   - figures stored in Output folder.
  
  
  # Start grid.
  # four corner points grid
  # grid <- tibble(after = c(0.1), before = dec$dec[1]) %>%
  #   add_row(after = c(0.1), before = dec$dec[length(dec$dec)]) %>%
  #   add_row(after = c(6.2), before = dec$dec[1]) %>%
  #   add_row(after = c(6.2), before = dec$dec[length(dec$dec)]) %>%
  #   mutate(selected = 1)
  # 3*4 grid 
  # grid <- tibble(after = c(0.1), before = dec$dec) %>%
  #   add_row(after = c(3.1), before = dec$dec) %>%
  #   add_row(after = c(6.2), before = dec$dec) %>%
  #   mutate(selected = 1)
  grid <- tibble(after = c(0.1), before = dec$dec) %>%
    add_row(after = c(1.1), before = dec$dec) %>%
    add_row(after = c(2.1), before = dec$dec) %>%
    add_row(after = c(3.1), before = dec$dec) %>%
    add_row(after = c(4.1), before = dec$dec) %>%
    add_row(after = c(5.1), before = dec$dec) %>%
    add_row(after = c(6.2), before = dec$dec) %>%
    mutate(selected = 1)
  # Labelled set.
  labelled <- data %>% 
    left_join(grid, by = c("after", "before")) %>%
    filter(selected == 1) %>% 
    mutate(y = get_y(ID, before, after,optimizefactor),
           w = 1)
  if(optimizefactor == 'Injury_reduction'){
    groundtruth_pick = df_with_y[df_with_y$R>0,]
    groundtruth <- data.frame(as.numeric(groundtruth_pick$R),groundtruth_pick$glance_prob,as.factor(rep('Ground truth',length(groundtruth_pick$R))))
  }
  else if(optimizefactor == 'Speed_reduction'){
    groundtruth_pick = df_with_y[df_with_y$dy>0,]
    groundtruth <- data.frame(as.numeric(groundtruth_pick$dy),groundtruth_pick$glance_prob,as.factor(rep('Ground truth',length(groundtruth_pick$dy))))
  }
  else if(optimizefactor == 'Impact_speed'){
    groundtruth_pick = df_with_y[df_with_y$y>0,]
    groundtruth <- data.frame(as.numeric(groundtruth_pick$y),groundtruth_pick$glance_prob,as.factor(rep('Ground truth',length(groundtruth_pick$y))))
  }
  colnames(groundtruth) <- c('y','glance_prob','group')
  
  # Unlabelled set.
  unlabelled <- data %>% 
    left_join(grid, by = c("after", "before")) %>%
    filter(is.na(selected)) %>% 
    mutate(w = 0,
           selected = 0,
           yhat = NA,
           prob_positive = NA)
  # Iterate
  new_sample <- labelled 
  k <- 1
  prediction_mse = c()
  for ( i in 1:niter ) {
    
    print(sprintf("Iteration %d", i))
    
    # Add data-points with certain outcomes (no crash or max impact speed crash) 
    # to labelled set, remove from unlabelled set.
    update <- add_certainty_outcomes(new_sample, labelled, unlabelled)
    labelled <- update$labelled
    unlabelled <- update$unlabelled
    crashes <- labelled %>% 
      filter(y > 0) %>% 
      mutate(w_final = glance_prob *w)
    
    sample_impact <- data.frame(crashes$y,crashes$w_final,as.factor(rep('sampling',length(crashes$y))))
    colnames(sample_impact) <- c('y','glance_prob','group')
    out_hist_impact = rbind(groundtruth,sample_impact)
    
    # First iteration: set all weights to 1 for labelled data points.
    if ( i == 1) {
      labelled$w <- 1
    }
    
    # Update predictions for cases with new data.
    for ( j in unique(new_sample$ID) ) {
      
      print(sprintf("Case = %s", j))
      
      # Update predictions.
      pred <- update_predictions(labelled %>% filter(ID == j), 
                                 unlabelled %>% filter(ID == j)) %>% 
        dplyr::select(ID, before, after, yhat, prob_positive)
      
      # Merge to unlabelled dataset.
      unlabelled %<>%
        left_join(pred, by = c("ID", "before", "after")) %>%
        mutate(yhat = ifelse(!is.na(yhat.y), yhat.y, yhat.x),
               prob_positive = ifelse(!is.na(prob_positive.y), prob_positive.y, prob_positive.x)) %>%
        dplyr::select(-yhat.x, -yhat.y, -prob_positive.x, -prob_positive.y)
      
    } # End for j in unique(new_sample$ID)
    
    
    # Fit model to impact speed distribution. Currently not used.
    #fit <- fitdist(labelled)
    
    # Calculate sampling probabilities. 
    pi <- calculate_sampling_scheme(unlabelled, crit, fit)
    
    # Error for unlabelled prediction
    check_error = unlabelled %>%
      mutate(y = get_y(ID, before, after,optimizefactor),
             selected = 0)
    e =  check_error$y - check_error$yhat
    prediction_mse[i] = sum(e^2)/length(e)
    error_prob = data.frame(e,check_error$glance_prob)
    colnames(error_prob) = c('e','glance_prob')
    
    # Plots.
    if ( plot & i %in% plotit ) {
      
      if ( length(unique(labelled$ID)) == 1 ) {
        if ( i == 1) {
          p1 <- ggplot(data = labelled %>% filter(selected == 1)) +
            geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
            scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
            coord_equal() +
            expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
            expand_limits(y = 6.1) +
            labs(title = sprintf("Iteration %d", i),
                 x = "EOFF after tauinv = 0.2 (s)",
                 y = "Deceleration [m/s^2]",
                 fill = "Impact speed (km/h)") +
            theme(legend.direction = "horizontal",
                  legend.justification = 0.5,
                  legend.position = "bottom",
                  legend.key.height = unit(0.3, "cm"),
                  legend.key.width = unit(0.6, "cm")) +
            ylim(-11, -3)                 +xlim(0, 6)
          
          p2 <- ggplot(data = labelled) +
            geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
            scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
            coord_equal() +
            expand_limits(y = 6.1) +
            expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
            labs(title = sprintf("Iteration %d", i),
                 x = "EOFF after tauinv = 0.2 (s)",
                 y = "Deceleration [m/s^2]",
                 fill = "Impact speed (km/h)") +
            theme(legend.direction = "horizontal",
                  legend.justification = 0.5,
                  legend.position = "bottom",
                  legend.key.height = unit(0.3, "cm"),
                  legend.key.width = unit(0.6, "cm")) +             
            ylim(-11, -3)                 +xlim(0, 6) 
        } else {
          
          p2 <- ggplot(data = labelled) +
            geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
            geom_rect(data = new_sample, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "firebrick") +
            scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
            coord_equal() +
            expand_limits(y = 6.1) +
            expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
            labs(title = sprintf("Iteration %d", i),
                 x = "EOFF after tauinv = 0.2 (s)",
                 y = "Deceleration [m/s^2]",
                 fill = "Impact speed (km/h)") +
            theme(legend.direction = "horizontal",
                  legend.justification = 0.5,
                  legend.position = "bottom",
                  legend.key.height = unit(0.3, "cm"),
                  legend.key.width = unit(0.6, "cm")) +             
            ylim(-11, -3)                 +xlim(0, 6) 
          
        }
        
        p3 <- ggplot() +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y>0), colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
          expand_limits(y = 7) +
          coord_equal() +
          labs(title = sprintf("Iteration %d", i),
               subtitle = "Anticipated collision probability",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = c(0, 0.925),
                legend.key.height = unit(0.3, "cm"),
                legend.key.width = unit(1.19, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        p4 <- ggplot(data = labelled) +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y), colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
          expand_limits(y = 7) +
          coord_equal() +
          expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
          labs(title = "",
               subtitle = "Anticipated impact speed (km/h)",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = c(0, 0.925),
                legend.key.height = unit(0.3, "cm"),
                legend.key.width = unit(1.19, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        p5 <- ggplot() +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40", colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
          expand_limits(y = 7) +
          coord_equal() +
          labs(title = "",
               subtitle = "Sampling probability",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = c(0, 0.925),
                legend.key.height = unit(0.3, "cm"),
                legend.key.width = unit(1.19, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        
        p6 = ggplot(out_hist_impact, aes(x = y, fill = group, weight = glance_prob)) +
          geom_histogram(bins = 30,alpha = 0.5, position = "identity")+
          labs(title = sprintf("Iteration %d", i),
               x = optimizefactor,
               y = "Weight",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) 
        
        if ( i == 1 ) {
          ggsave(sprintf("Output/fig%d.png", fcount), p1, dpi = 1000, width = 90, height = 90, unit = "mm")
          fcount <- fcount + 1 
        }
        ggsave(sprintf("Output/fig%d.png", fcount), p2, dpi = 1000, width = 90, height = 90, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), plot_grid(p3, p4, p5, ncol = 3, align = "vh", axis = "tblr"), 
               dpi = 1000, width = 270, height = 100, unit = "mm")
        fcount <- fcount + 1 
        
        ggsave(sprintf("Output/fig%d.png", fcount), p6, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        
      } else {
        
        ptsize <- 16
        ptsmall <- 12
        theme_set(theme_bw()) 
        theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "sans"),
                     axis.line = element_line(colour = "black", size = 0.25), 
                     axis.ticks = element_line(colour = "black", size = 0.25), 
                     legend.key.width = unit(2, "cm"),
                     legend.key.height = unit(0.4, "cm"),
                     legend.margin = ggplot2::margin(t = -0.25, unit = 'cm'),
                     legend.spacing =  unit(0, "cm"),
                     legend.position = "bottom",
                     legend.text = element_text(size = ptsmall, colour = "black", family = "sans"),
                     legend.title = element_text(size = ptsmall, colour = "black", family = "sans"),
                     strip.background.x = element_blank(),
                     panel.grid = element_blank(),
                     plot.subtitle = element_text(size = ptsmall, colour = "black", family = "sans", face = "plain", hjust = 0),
                     plot.title = element_text(size = ptsize, colour = "black", family = "sans", face = "plain", hjust = 0),
                     text = element_text(size = ptsize, colour = "black", family = "sans"))
        
        if ( i == 1) {
          
          p2 <- ggplot(data = labelled) +
            geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
            scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
            facet_wrap(~ID, ncol =6, nrow = 8, labeller = labeller(ID = function(x) sprintf("ID = %s", x))) + 
            coord_equal() +
            expand_limits(y = 6.1) +
            expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
            labs(title = sprintf("Iteration %d", i),
                 x = "EOFF after tauinv = 0.2 (s)",
                 y = "Deceleration [m/s^2]",
                 fill = "Impact speed (km/h)") +
            theme(legend.direction = "horizontal",
                  legend.justification = 0.5,
                  legend.position = "bottom",
                  legend.key.height = unit(0.5, "cm"),
                  legend.key.width = unit(2, "cm")) +             
            ylim(-11, -3)                 +xlim(0, 6) 
        } else {
          
          p2 <- ggplot(data = labelled) +
            geom_rect(aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y)) +
            geom_rect(data = new_sample, aes(xmin = after - 0.1, xmax = after + 0.1, ymin = before - 0.1, ymax = before + 0.1), fill = "firebrick") +
            scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
            facet_wrap(~ID, ncol =6, nrow = 8, labeller = labeller(ID = function(x) sprintf("ID = %s", x))) + 
            coord_equal() +
            expand_limits(y = 6.1) +
            expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
            labs(title = sprintf("Iteration %d", i),
                 x = "EOFF after tauinv = 0.2 (s)",
                 y = "Deceleration [m/s^2]",
                 fill = "Impact speed (km/h)") +
            theme(legend.direction = "horizontal",
                  legend.justification = 0.5,
                  legend.position = "bottom",
                  legend.key.height = unit(0.5, "cm"),
                  legend.key.width = unit(2, "cm")) +             
            ylim(-11, -3)                 +xlim(0, 6) 
          
        }
        
        p3 <- ggplot() +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = prob_positive)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y>0), colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
          facet_wrap(~ID, ncol =6, nrow = 8, labeller = labeller(ID = function(x) sprintf("ID = %s", x))) + 
          coord_equal() +
          labs(title = sprintf("Iteration %d", i),
               subtitle = "Anticipated collision probability",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        p4 <- ggplot(data = labelled) +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = yhat)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = y), colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", labels = scales::number_format(digits = 1)) +
          facet_wrap(~ID, ncol =6, nrow = 8, labeller = labeller(ID = function(x) sprintf("ID = %s", x))) + 
          coord_equal() +
          expand_limits(fill = c(0, ceiling(2 * max(labelled$y)) / 2)) +
          labs(title = sprintf("Iteration %d", i),
               subtitle = "Anticipated impact speed (km/h)",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        p5 <- ggplot() +
          geom_rect(data = unlabelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05, fill = pi)) +
          geom_rect(data = labelled, aes(xmin = after - 0.05, xmax = after + 0.05, ymin = before - 0.05, ymax = before + 0.05), fill = "grey40", colour = "black", size = 0.01) +
          scale_fill_continuous(type = "viridis", trans = "log10", labels = scales::scientific_format(scale = 1)) +
          facet_wrap(~ID, ncol =6, nrow = 8, labeller = labeller(ID = function(x) sprintf("ID = %s", x))) + 
          coord_equal() +
          labs(title = sprintf("Iteration %d", i),
               subtitle = "Sampling probability",
               x = "EOFF after tauinv = 0.2 (s)",
               y = "Deceleration [m/s^2]",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) +             
          ylim(-11, -3)                 +xlim(0, 6) 
        
        
        p6 = ggplot(out_hist_impact, aes(x = y, fill = group, weight = glance_prob)) +
          geom_histogram(bins = 30,alpha = 0.5, position = "identity")+
          labs(title = sprintf("Iteration %d", i),
               x = optimizefactor,
               y = "Weight",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) 
        # P7 probably not really needed, and instead MSE for the unlabelled predicted error maybe more relevant
        p7 = ggplot(error_prob, aes(x = e, weight = glance_prob)) +
          geom_histogram(bins = 30,alpha = 0.5, position = "identity")+
          labs(title = sprintf("Iteration %d", i),
               x = "Estimation error",
               y = "Weight",
               fill = NULL) +
          theme(legend.direction = "horizontal",
                legend.justification = -0.025,
                legend.position = "top",
                legend.key.height = unit(0.5, "cm"),
                legend.key.width = unit(2, "cm")) 
        
        ggsave(sprintf("Output/fig%d.png", fcount), p2, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), p3, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), p4, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), p5, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), p6, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1 
        ggsave(sprintf("Output/fig%d.png", fcount), p7, dpi = 1000, width = 270, height = 270, unit = "mm")
        fcount <- fcount + 1
      }
    }
    # Sample new observations.
    ix <- as.numeric(rmultinom(n = 1, size = bsize, prob = pi))
    new_wt <- ix / (bsize * pi)
    
    # Get data for sampled observations.
    new_sample <- unlabelled %>% 
      mutate(new_wt = new_wt) %>% 
      filter(new_wt > 0) %>% 
      mutate(y = get_y(ID, before, after,optimizefactor),
             selected = 1) %>% 
      dplyr::select(-prob_positive, -yhat)
    
    # Update labelled and unlabelled sets.
    labelled <- labelled %>%
      mutate(new_wt = 1) %>% # Re-query labelled data points with probability 1. 
      add_row(new_sample) %>%
      mutate(w = w + (new_wt - w) / k) %>%
      dplyr::select(-new_wt)
    
    unlabelled %<>%
      mutate(new_wt = new_wt) %>% 
      filter(new_wt <= 0) %>% 
      dplyr::select(-new_wt)
    
    # Increase counter if at least one new crash has been generated.
    k <- k + (any(new_sample$y > 0))
    
  } # End active learning.
  
  labelled <- labelled 
  
  crashes <- labelled %>% 
    filter(y > 0) %>% 
    mutate(w_final = glance_prob *w)
  
  noncrashes <- labelled %>% 
    filter(y == 0)
  # could include MSE for labelled data here as well
  MSE <- prediction_mse
  return(list(all = labelled, crashes = crashes, noncrashes = noncrashes,MSE = MSE))
  
}