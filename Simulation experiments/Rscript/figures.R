##################################################
## Project: Active Sampling
## Description: Figures and illustrations for simulation experiments
## Date: 9 Jan 2024
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())
gc()


# Load packages -----------------------------------------------------------

library("BSDA")
library("ggrepel")
library("magrittr")
library("MASS")
library("tidyverse")


# Plot settings. ----------------------------------------------------------

ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.2, b = 0, l = 0, unit = 'cm')),
             legend.key.width = unit(1.65, "cm"),
             legend.key.height = unit(0.5, "cm"),
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.position = "bottom",
             strip.background = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.spacing.x = unit(0.3, "cm"),
             panel.spacing.y = unit(0.3, "cm"),
             panel.grid = element_blank(),  
             plot.margin = margin(t = 0, r = 0.2, b = 0, l = 0, unit = 'cm'),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 1))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("text", list(size = ptsize / .pt, family = "serif"))

rm(ptsize)



# Plot examples of simulated datasets  ------------------------

rm(list = ls())
params <- crossing(bandwidth = c(0.1, 1, 10),
                   r2 = 0.75)

for ( i in 1:nrow(params) ) {
  load(sprintf("Simulation experiments/Data/SimData_Bandwidth_%.2f_R2_%.2f_strictly_positive.RData", params$bandwidth[i], params$r2[i]))

  dta <- dta[seq(1, 1000, 10), ] %>% 
    mutate(bandwidth = params$bandwidth[i],
           r2 = params$r2[i])
  
  if (i == 1) {
    plt <- dta
  } else {
    plt %<>% 
      add_row(dta)
  }
}

p <- ggplot(plt, aes(x = z, y = y)) + 
  geom_point() + 
  facet_wrap(~bandwidth) + 
  expand_limits(y = 0) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) + 
  scale_y_continuous(breaks = c(0, 2.5, 5)) + 
  facet_wrap(~bandwidth, labeller = labeller(bandwidth = c(`0.1` = "Non-linear (σ = 0.1)",
                                                           `1` = "Polynomial  (σ = 1)",
                                                           `10` = "Linear (σ = 10)"))) 

print(p)

ggsave("Simulation experiments/Figures/example_simulated_data.png", width = 160, height = 60, unit = "mm", dpi = 1000)

rm(p)



# All results, generalized additive model --------

# Clean-up.
rm(list = ls())

# Parameters. Make sure corresponding experiments have been executed. 
params <- tibble(model = c("const", "pps", "lm", "gam", "rf", "gbt", "gpr")) %>% # Auxiliary models. 
  crossing(bandwidth = c(0.1, 1, 10),
           r2 = c(0.1, 0.5, 0.75, 0.9),
           normalization = c("zero_mean", "strictly_positive"),
           naive = c(TRUE, FALSE), # Ignore prediction uncertainty (TRUE) or account for prediction uncertainty (FALSE)
           bsize = c(10, 50), # Small or large batch size. 
           estimator = c("default", "Hajek")) # Estimator for the finite population mean. 
drop_rows <- NULL

# Load results.
for (i in 1:nrow(params) ) {

  # Save. 
  options(warn = -1)
  x <- try(load(file = sprintf("Simulation experiments/Results/Bandwidth_%s_R2_%s_%s_Model_%s_BatchSize_%s_Naive_%s_estimator_%s.RData", 
                               params$bandwidth[i],
                               params$r2[i], 
                               params$normalization[i],
                               params$model[i],
                               params$bsize[i],
                               params$naive[i],
                               params$estimator[i])), 
           silent = TRUE)
  options(warn = 0)
  
  if ( class(x) == "try-error" ) {
    drop_rows <- c(drop_rows, i)
    rm(x)
    next
  }
  
  if ( params$model[i] == "const" ) {
    
    # Control variate estimator. 
    res_cv <- res %>% 
      mutate(model = "cv", 
             mse = mse_cv, 
             sd_mse = sd_mse_cv) 
    
    # Ratio estimator.
    res_ratio <- res %>% 
      mutate(model = "ratio", 
             mse = mse_ratio, 
             sd_mse = sd_mse_ratio)
    
    res %<>% 
      add_row(res_cv) %>% 
      add_row(res_ratio) %>% 
      dplyr::select(names(res)[1:11])
    
    rm(res_cv, res_ratio)
  }

  res %<>%
    mutate(mse_low = pmax(0, mse - 1.96 * sd_mse / sqrt(nreps)),
           mse_high = mse + 1.96 * sd_mse / sqrt(nreps),
           rmse = sqrt(mse),
           rmse_low = sqrt(mse_low),
           rmse_high = sqrt(mse_high), 
           star = "")
  
  if ( i == 1) {
    allres <- res
  } else {
    allres %<>% 
      add_row(res)
  }
  
  rm(res, x)
}
params <- params[-drop_rows, ]
rm(i, drop_rows)


# Test performance gain vs simple random sampling.
for ( i in 1:nrow(params) ) {
  if ( !(params$model[i] %in% c("const", "pps", "cv", "ratio")) ) {
    
    as <- allres %>%
      filter( model == params$model[i]
              & bsize == params$bsize[i]
              & naive == params$naive[i]
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    ref <- allres %>%
      filter( model == "const"
              # & bsize == params$bsize[i]
              # & naive == params$naive[i]
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    nseq <- sort(unique(as$n))
    nn <- length(nseq)
    p <- rep(NA, nn)
    
    for ( j in 1:nn ) {
      
      x <- as[which(as$n == nseq[j]), ]
      y <- ref[which(ref$n == nseq[j]), ]
      
      test <- tsum.test(mean.x = x$mse, s.x = x$sd_mse, n.x = x$nreps, 
                mean.y = y$mse, s.y = y$sd_mse, n.y = y$nreps)
      
      p[j] <- test$p.value

    }
    
    star <- rep("", nn)
    ix <- which(vapply(1:nn, function(ix) all(p[ix:nn] < 0.05), logical(1)))[1]
    star[ix] <- "*"
    
    # Sample size reduction. 
    sample_size_ratio <- rep(NA, nn)
    n_ref <- ref$n[length(ref$rmse)]
    rmse_ref <- ref$rmse[length(ref$rmse)]
    as_rmse <- approx(as$n, as$rmse, xout = min(nseq):max(nseq))
    n_as <- as_rmse$x[which(as_rmse$y < rmse_ref)[1]]
    sample_size_ratio[nn] <- n_as / n_ref
    
    allres[which(allres$model == params$model[i]
                 & allres$bsize == params$bsize[i]
                 & allres$naive == params$naive[i]
                 & allres$bandwidth == params$bandwidth[i]
                 & allres$r2 == params$r2[i]
                 & allres$normalization == params$normalization[i]
                 & allres$estimator == params$estimator[i]), c("star", "sample_size_ratio")] <- data.frame(star, sample_size_ratio)
        
    rm(list = setdiff(ls(), c("allres", "params")))
  }
}
rm(params)


# Plot results.
plot_this <- function(bsize_, naive_, estimator_, normalization_, show_stars) {
  
  cols <- palette.colors(palette = as.vector("Tableau 10"))
  names(cols) <- NULL
  
  plt <- allres
  
  if ( !show_stars ) {
    plt <- allres %>%
      mutate(star = "")
  } 
  
  p <- plt %>% 
    filter( (bsize == bsize_ & naive == naive_ & estimator == estimator_ & normalization == normalization_) 
            | (model == "const" & estimator == estimator_ & normalization == normalization_ ) ) %>% 
    mutate(model_num = as.numeric(factor(model, levels = unique(model))),
           xstar = n, 
           ystar = max(rmse) - 0.3 * 1.2^(-2.5 * model_num)) %>% 
           # rmse_low = ifelse(model %in% c("pps", "ratio", "cv"), NA, rmse_low), 
           # rmse_high = ifelse(model %in% c("pps", "ratio", "cv"), NA, rmse_high)) %>% 
    ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), color = NA, show.legend = FALSE, alpha = 0.5) + 
    geom_line(aes(y = rmse)) +
    geom_text(aes(x = xstar, y = ystar, label = star), show.legend = FALSE, size = 16 / .pt, vjust = 1) + 
    facet_grid(bandwidth ~ r2) + 
    # scale_colour_brewer(palette = "Set1", breaks = breaks, label = labels) +
    # scale_fill_brewer(palette = "Pastel1", breaks = breaks, label = labels) +
    # scale_colour_manual(values = cols, breaks = breaks, label = labels) +
    # scale_fill_manual(values = cols, breaks = breaks, label = labels) +
    scale_colour_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_fill_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    # scale_colour_viridis_d(breaks = breaks, label = labels) +
    # scale_fill_viridis_d(breaks = breaks, label = labels) +
    scale_linetype_discrete(breaks = breaks, label = labels) +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2~bandwidth, labeller = labeller(bandwidth = c(`0.1` = "σ = 0.1", 
                                                               `1` = "σ = 1", 
                                                               `10` = "σ = 10"),
                                                 r2 = c(`0.1` = "R2 = 0.10", 
                                                        `0.5` = "R2 = 0.50", 
                                                        `0.75` = "R2 = 0.75", 
                                                        `0.9` = "R2 = 0.90"))) +
    labs(x = "Total sample size",
         y = expression(paste("RMSE of ", hat(theta))),
         colour = NULL,
         fill = NULL,
         linetype = NULL) 
  
  # print(p)
  
  ggsave(sprintf("Simulation experiments/Figures/Results_Naive_%s_Batchsize_%s_Estimator_%s_Normalization_%s.png", 
                 naive_, bsize_, estimator_, normalization_), 
         width = 160, height = 130, unit = "mm", dpi = 1000)
  
  rm(p, plt)
}

breaks <- c("const", "cv", "ratio", "pps", "lm", "gam", "gbt", "gpr", "rf")
labels <- c("SRS", "CV", "Rest", "PPS", "AS+LM", "AS+GAM", "AS+GBT", "AS+GPR", "AS+RF")

# There was a substantial performance gain with active sampling already at small samples.
# The performance gain increased with the signal-to-noise ratio. 
# In the linear case (y_i linearly related to z_i) there was a slight loss of efficiency 
# when applying flexible surrogate model, as compared to a linear model. 
# Importantly, the performance of active sampling was never worse than simple random sampling, 
# even for a misspecified model. 
plot_this(10, FALSE, "default", "strictly_positive", TRUE) 

# The performance gain was somewhat smaller and when the study variable attained 
# both positive and negative values and when a non-linear estimator was used. 
plot_this(10, FALSE, "default", "zero_mean", TRUE) 
plot_this(10, FALSE, "Hajek", "zero_mean", TRUE) 

# Similar results were observed also for other machine learning algorithms. 
plot_this(50, FALSE, "default", "strictly_positive", FALSE)
# plot_this(50, FALSE, "default", "zero_mean")
# plot_this(50, FALSE, "Hajek", "strictly_positive")
# plot_this(50, FALSE, "Hajek", "zero_mean")

# In contrast, a naive implementation of the active sampling algorithm resulted in worse performance 
# compared to simple random sampling, particularly in low signal-to-noise ratio settings, 
# non-positive data, and for misspecified models.  
plot_this(10, TRUE, "Hajek", "zero_mean", FALSE)  
plot_this(10, TRUE, "default", "strictly_positive", FALSE) 

# Effect of batch size on performance. 
plot_this <- function(naive_, estimator_, normalization_, model_) {
  
  cols <- palette.colors(palette = as.vector("Tableau 10"))
  names(cols) <- NULL
  
  p <- allres %>% 
    filter(naive == naive_ & estimator == estimator_ & normalization == normalization_ & (model == model_ | (model == "const" & bsize == 10))) %>% 
    mutate(group = paste(model, bsize)) %>% 
    ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), color = NA, show.legend = FALSE, alpha = 0.5) + 
    geom_line(aes(y = rmse)) +
    facet_grid(bandwidth ~ r2) + 
    # scale_colour_brewer(palette = "Set1", breaks = breaks, label = labels) +
    # scale_fill_brewer(palette = "Pastel1", breaks = breaks, label = labels) +
    # scale_colour_manual(values = cols, breaks = breaks, label = labels) +
    # scale_fill_manual(values = cols, breaks = breaks, label = labels) +
    scale_colour_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_fill_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    # scale_colour_viridis_d(breaks = breaks, label = labels) +
    # scale_fill_viridis_d(breaks = breaks, label = labels) +
    scale_linetype_discrete(breaks = breaks, label = labels) +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2~bandwidth, labeller = labeller(bandwidth = c(`0.1` = "σ = 0.1", 
                                                               `1` = "σ = 1", 
                                                               `10` = "σ = 10"),
                                                 r2 = c(`0.1` = "R2 = 0.10", 
                                                        `0.5` = "R2 = 0.50", 
                                                        `0.75` = "R2 = 0.75", 
                                                        `0.9` = "R2 = 0.90"))) +
    labs(x = "Total sample size",
         y = expression(paste("RMSE of ", hat(theta))),
         colour = NULL,
         fill = NULL,
         linetype = NULL) 
  
  # print(p)
  
  ggsave(sprintf("Simulation experiments/Figures/Results_Naive_%s_Estimator_%s_Normalization_%s_Model_%s.png", 
                 naive_, estimator_, normalization_, model_), 
         width = 160, height = 130, unit = "mm", dpi = 1000)
  
  rm(p)
}

breaks <- c("const 10", "gam 10", "gam 50")
labels <- c("SRS", "AS, batch size = 10", "AS, batch size = 50")

# Better performance was observed with a small batch size but difference were 
# attenuated as the sample size increased.  
plot_this(FALSE, "default", "strictly_positive", "gam")

rm(breaks, labels)

# Sample size reductions.
allres %>% 
  filter(model == "gam" & n == 250 & bsize == 10 & !naive & r2 >= 0.5) %>% 
  group_by(normalization, estimator) %>% 
  summarize(min = 1 - max(sample_size_ratio, na.rm = TRUE),
            max = 1 - min(sample_size_ratio, na.rm = TRUE),
            range = sprintf("%.1f–%.1f", 100 * min, 100 * max), .groups = "keep")

