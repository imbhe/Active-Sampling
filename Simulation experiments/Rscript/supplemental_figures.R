##################################################
# Project: Active Sampling
# Description: Figures and illustrations for active sampling experiments on simulated data, Section 5 in the paper. 
#              This script generates Supplemental Figure S1–S6. 
# Date: 27 May 2024
# Author: Henrik Imberg
##################################################


# Initialize --------------------------------------------------------------

# Clean-up.
cat("\14")
rm(list = ls())
gc()


# Load packages -----------------------------------------------------------

source("Application/Rscript/load_required_packages.R")
load_required_packages(c("BSDA", "magrittr", "MASS", "tidyverse"))


# Plot settings -----------------------------------------------------------

ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.2, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1.25, "cm"),
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


# Post-processing ---------------------------------------------------------

# Clean-up.
rm(list = ls())

# Experiment parameters. 
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

  # Try load. 
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
  
  # If simulation corresponding to current parameter setting not run: 
  # drop this row from params list.
  if ( class(x) == "try-error" ) {
    drop_rows <- c(drop_rows, i)
    rm(x)
    next
  }
  
  # Additional estimators for SRS. 
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
    
    # Append to results. 
    res %<>% 
      add_row(res_cv) %>% 
      add_row(res_ratio) %>% 
      dplyr::select(names(res)[1:11])
    
    rm(res_cv, res_ratio)
  }

  # Add 95% CI for RMSE.
  res %<>%
    mutate(mse_low = pmax(0, mse - 1.96 * sd_mse / sqrt(nreps)),
           mse_high = mse + 1.96 * sd_mse / sqrt(nreps),
           rmse = sqrt(mse),
           rmse_low = sqrt(mse_low),
           rmse_high = sqrt(mse_high), 
           star = "")
  
  # Append to results. 
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
    
    # Active sampling performance. 
    as <- allres %>%
      filter( model == params$model[i]
              & bsize == params$bsize[i]
              & naive == params$naive[i]
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    # Simple random sampling performance. 
    ref <- allres %>%
      filter( model == "const"
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    # To store results.     
    nseq <- sort(unique(as$n))
    nn <- length(nseq)
    p <- rep(NA, nn)
    
    # Test if significant improvement with active sampling vs SRS.
    for ( j in 1:nn ) {
      
      x <- as[which(as$n == nseq[j]), ]
      y <- ref[which(ref$n == nseq[j]), ]
      
      test <- tsum.test(mean.x = x$mse, s.x = x$sd_mse, n.x = x$nreps, 
                mean.y = y$mse, s.y = y$sd_mse, n.y = y$nreps)
      
      p[j] <- test$p.value

    }
    
    # "Star" (asterisk) for significant improvement. 
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
    
    # Add to results. 
    allres[which(allres$model == params$model[i]
                 & allres$bsize == params$bsize[i]
                 & allres$naive == params$naive[i]
                 & allres$bandwidth == params$bandwidth[i]
                 & allres$r2 == params$r2[i]
                 & allres$normalization == params$normalization[i]
                 & allres$estimator == params$estimator[i]), c("star", "sample_size_ratio")] <- data.frame(star, sample_size_ratio)
        
    # Clean-up.
    rm(list = setdiff(ls(), c("allres", "params")))
  }
}
rm(params)

# For nice display of R2 in figures. 
allres$r2lab = allres$r2
allres$r2lab <- as.factor(allres$r2lab)
levels(allres$r2lab) <- c(expression(paste("R"^2, " = 0.10")), 
                          expression(paste("R"^2, " = 0.50")), 
                          expression(paste("R"^2, " = 0.75")),
                          expression(paste("R"^2, " = 0.90")))


# For nice display of sigma in figures. 
allres$sigma = allres$bandwidth
allres$sigma <- as.factor(allres$sigma)
levels(allres$sigma) <- c(expression(paste(sigma, " = 0.1")), 
                          expression(paste(sigma, " = 1")), 
                          expression(paste(sigma, " = 10")))


# Supplemental Figure S1 and S3-S6 ----------------------------------------

plot_this <- function(bsize_, naive_, estimator_, normalization_, show_stars, figname) {

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
    ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), color = NA, show.legend = FALSE, alpha = 0.5) + 
    geom_line(aes(y = rmse)) +
    geom_text(aes(x = xstar, y = ystar, label = star), show.legend = FALSE, size = 16 / .pt, vjust = 1) + 
    scale_colour_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_fill_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_linetype_discrete(breaks = breaks, label = labels) +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2lab~sigma, labeller = label_parsed) +
    labs(x = "Sample size",
         y = "eRMSE",
         colour = NULL,
         fill = NULL,
         linetype = NULL) 
  
  print(p)
  
  ggsave(sprintf("Simulation experiments/Figures/%s.png", figname), 
         width = 160, height = 130, unit = "mm", dpi = 1000)
  
  rm(p, plt)
}

# Set breaks and labels. 
breaks <- c("const", "cv", "ratio", "pps", "lm", "gam", "gbt", "gpr", "rf")
labels <- c("Simple random sampling", "Control variates", "Ratio estimator", "Importance sampling", "Active sampling+LM", "Active sampling+GAM", "Active sampling+GBT", "Active sampling+GPR", "Active sampling+RF")

# All ML models.  
plot_this(50, FALSE, "default", "strictly_positive", FALSE, "Figure S1")

# Linear (default) and non-linear estimator. 
plot_this(10, FALSE, estimator_ = "default", "zero_mean", TRUE, "Figure S3") 
plot_this(10, FALSE, estimator_ = "Hajek", "zero_mean", TRUE, "Figure S4") 

# Naive active sampling (ignoring prediction uncertainty).
plot_this(10, naive = TRUE, "default", "strictly_positive", FALSE, "Figure S5") 
plot_this(10, naive = TRUE, "Hajek", "zero_mean", FALSE, "Figure S6")  

# Clean-up.
rm(breaks, labels)


# Supplemental Figure S2 --------------------------------------------------

plot_this <- function(naive_, estimator_, normalization_, model_, figname) {
  
  p <- allres %>% 
    filter(naive == naive_ & estimator == estimator_ & normalization == normalization_ & (model == model_ | (model == "const" & bsize == 10))) %>% 
    mutate(group = paste(model, bsize)) %>% 
    ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), color = NA, show.legend = FALSE, alpha = 0.5) + 
    geom_line(aes(y = rmse)) +
    facet_grid(bandwidth ~ r2) + 
    scale_colour_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_fill_brewer(palette = "Dark2", breaks = breaks, label = labels) +
    scale_linetype_discrete(breaks = breaks, label = labels) +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2lab~sigma, labeller = label_parsed) +
    labs(x = "Sample size",
         y = "eRMSE",
         colour = NULL,
         fill = NULL,
         linetype = NULL) 
  
  print(p)
  
  ggsave(sprintf("Simulation experiments/Figures/%s.png", figname), 
         width = 160, height = 130, unit = "mm", dpi = 1000)
  
  rm(p)
}

# Set breaks and labels. 
breaks <- c("const 10", "gam 10", "gam 50")
labels <- c("Simple random sampling", "AS, batch size = 10", "AS, batch size = 50")

# Effect of batch size on performance. 
plot_this(FALSE, "default", "strictly_positive", "gam", "Figure S2")

# Clean-up.
rm(breaks, labels)


# Sample size reductions --------------------------------------------------

allres %>% 
  filter(model == "gam" & n == 250 & bsize == 10 & !naive & r2 >= 0.5) %>% 
  group_by(normalization, estimator) %>% 
  summarize(min = 1 - max(sample_size_ratio, na.rm = TRUE),
            max = 1 - min(sample_size_ratio, na.rm = TRUE),
            range = sprintf("%.1f–%.1f", 100 * min, 100 * max), .groups = "keep")


# Clean-up ----------------------------------------------------------------

# rm(list = ls())
