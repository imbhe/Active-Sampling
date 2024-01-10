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
             axis.title.y = element_text(margin = margin(t = 0, r = 0.0, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1, "cm"),
             legend.key.height = unit(0.5, "cm"),
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 1))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("text", list(size = ptsize / .pt, family = "serif"))

rm(ptsize)


# Load functions ----------------------------------------------------------

source("Simulation experiments/Rscript/functions.R")


# Plot mean trend, varying degree of non-linearity ------------------------

# Clean-up.
rm(list = setdiff(ls(), lsf.str()))

# Parameters. 
alldta <- NULL
nreps <- 3
bseq <- c(10^seq(-1, 0, 0.5), 10)

# Set seed, for reproducibility. 
set.seed(1234)  

# Simulate data. 
for ( i in seq_along(bseq) ) {
  for ( j in 1:nreps ) {
    dta <- sim_data(bandwidth = bseq[i]) %>% 
      mutate(bandwidth = sprintf("%.2f", bseq[i]), 
             rep = j)
    
    if ( i == 1 & j == 1 ) {
      alldta <- dta
    } else {
      alldta %<>% add_row(dta)  
    }
  }
}

ggplot(alldta) + 
  geom_line(aes(x = z, y = yhat, colour = as.factor(rep), linetype = as.factor(rep)), lwd = 1) + 
  facet_wrap(~bandwidth, labeller = labeller(bandwidth = c(`0.10` = "Highly non-linear (σ = 0.1)",
                                                           `0.32`  = "Non-linear (σ = 0.32)",
                                                           `1.00` = "Polynomial  (σ = 1)",
                                                           `10.00` = "Roughly linear (σ = 10)"))) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "z", 
       y = expression(hat(y))) + 
  theme(legend.position = "none")

ggsave("Simulation experiments/Figures/example_simulated_data.png", width = 160, height = 120, unit = "mm", dpi = 1000)



# All results, generalized additive model --------

# Clean-up.
rm(list = setdiff(ls(), lsf.str()))

# Parameters. Make sure corresponding experiments have been executed. 
params <- tibble(model = c("const", "lm", "gam")) %>% # Auxiliary models. 
  crossing(bandwidth = c(0.1, 0.32, 1, 10),
           r2 = c(0.1, 0.25, 0.5, 0.75, 0.9),
           normalization = c("zero_mean", "strictly_positive"),
           naive = c(TRUE, FALSE), # Ignore prediction uncertainty (TRUE) or account for prediction uncertainty (FALSE)
           bsize = c(10, 50), # Small or large batch size. 
           estimator = c("default", "Hajek")) # Estimator for the finite population mean. 

# Load results.
for (i in 1:nrow(params) ) {

  # Save. 
  load(file = sprintf("Simulation experiments/Results/Bandwidth_%s_R2_%s_%s_Model_%s_BatchSize_%s_Naive_%s_estimator_%s.RData", 
                      params$bandwidth[i],
                      params$r2[i], 
                      params$normalization[i],
                      params$model[i],
                      params$bsize[i],
                      params$naive[i],
                      params$estimator[i]))
  
  res %<>%
    mutate(mse_low = pmax(1e-6, mse - 1.96 * sd_mse / sqrt(nreps)),
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
  
  rm(res)
}
rm(i)

# Test performance gain vs simple random sampling.
for ( i in 1:nrow(params) ) {
  if ( params$model[i] != "const" ) {
    
    tmp <- allres %>%
      filter( model %in% c("const", params$model[i])
              & bsize == params$bsize[i]
              & naive == params$naive[i]
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    nseq <- sort(unique(tmp$n))
    
    p <- rep(NA, length(nseq))
    
    for ( j in 1:length(nseq) ) {
      
      x <- tmp[which(tmp$model == "const" & tmp$n == nseq[j]), ]
      y <- tmp[which(tmp$model != "const" & tmp$n == nseq[j]), ]
      
      test <- tsum.test(mean.x = x$mse, s.x = x$sd_mse, n.x = x$nreps, 
                mean.y = y$mse, s.y = y$sd_mse, n.y = y$nreps)
      
      p[j] <- test$p.value

    }
    
    star <- rep("", length(nseq))
    ix <- which(vapply(1:length(p), function(ix) all(p[ix:length(p)] < 0.05), logical(1)))[1]
    star[ix] <- "*"
    
    allres[which(allres$model == params$model[i]
                 & allres$bsize == params$bsize[i]
                 & allres$naive == params$naive[i]
                 & allres$bandwidth == params$bandwidth[i]
                 & allres$r2 == params$r2[i]
                 & allres$normalization == params$normalization[i]
                 & allres$estimator == params$estimator[i]), "star"] <- star
    
  }
}

#   mutate(star = ifelse(pval < 0.05, "*", ""),
#          model_num = as.numeric(factor(model, levels = unique(params$model))),
#          ystar = max(rmse_high) - (max(rmse_high) - min(rmse_low)) * model_num * 0.025)


# Plot results.
plot_this <- function(bsize_, naive_, estimator_, normalization_) {
  allres %>% 
    filter(bandwidth %in% c(0.1, 1, 10) & r2 %in% c(0.1, 0.5, 0.75, 0.90)) %>% 
    filter(bsize == bsize_ & naive == naive_ & estimator == estimator_ & normalization == normalization_) %>% 
    mutate(model_num = as.numeric(factor(model, levels = unique(model))),
           ystar = max(rmse_high) + (max(rmse_high) - min(rmse_low)) * model_num * 0.1) %>% 
    ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5, color = NA) + 
    geom_line(aes(y = rmse)) +
    geom_text(aes(y = ystar, label = star), show.legend = FALSE) + 
    facet_grid(bandwidth ~ r2) + 
    scale_colour_brewer(palette = "Dark2", breaks = models, label = labels) +
    scale_fill_brewer(palette = "Dark2", breaks = models, label = labels) +
    scale_linetype_discrete(breaks = models, label = labels) +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2~bandwidth, scales = "free", labeller = labeller(bandwidth = c(`0.1` = "σ = 0.1", 
                                                                                `1` = "σ = 1", 
                                                                                `10` = "σ = 10"),
                                                                  r2 = c(`0.1` = "R2 = 0.10", 
                                                                         `0.5` = "R2 = 0.50", 
                                                                         `0.75` = "R2 = 0.75", 
                                                                         `0.9` = "R2 = 0.90"))) +
    labs(x = "Sample size",
         y = "RMSE",
         colour = NULL,
         fill = NULL,
         linetype = NULL) +
    theme(legend.position = "bottom")  
  
  ggsave(sprintf("Simulation experiments/Figures/Results_Naive_%s_Batchsize_%s_Estimator_%s_Normalization_%s.png", 
                 naive_, bsize_, estimator_, normalization_), 
         width = 160, height = 150, unit = "mm", dpi = 1000)
}

models <- c("const", "lm", "gam")
labels <- c("SRS", "LM", "GAM")

# There was a substantial performance gain with active sampling already at small samples.
# The performance gain increased with the signal-to-noise ratio. 
# In the linear case (y_i linearly related to z_i) there was a slight low of efficiency when applying flexible surrogate model, as compared to a linear model. 
# Importantly, the performance of active sampling was never worse than simple random sampling, even for a misspecified model. 
plot_this(10, FALSE, "default", "strictly_positive") 

# In contrast, a naive implementation of the active sampling algorithm resulted in worse performance 
# compared to simple random sampling, particularly in low signal-to-noise ratio settings, non-positive data, and for misspecified models.  
plot_this(10, TRUE, "Hajek", "zero_mean")  
plot_this(10, TRUE, "default", "strictly_positive") 

# The performance gain was somewhat smaller and when the study variable attained both positive and negative values 
# and when a non-linear estimator was used. 
plot_this(10, FALSE, "default", "zero_mean") 
plot_this(10, FALSE, "Hajek", "zero_mean") 



plot_this <- function(naive_, estimator_, normalization_, model_) {
  allres %>% 
    filter(bandwidth %in% c(0.1, 1, 10) & r2 %in% c(0.5, 0.75, 0.90)) %>% 
    filter(naive == naive_ & estimator == estimator_ & normalization == normalization_ & (model == model_ | (model == "const" & bsize == 10))) %>% 
    mutate(group = paste(bsize, model),
           group_num = as.numeric(factor(group, levels = unique(group))),
           ystar = max(rmse_high) + (max(rmse_high) - min(rmse_low)) * group_num * 0.025) %>% 
    ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
    geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5, color = NA) + 
    geom_line(aes(y = rmse)) +
    geom_text(aes(y = ystar, label = star), show.legend = FALSE) + 
    facet_grid(bandwidth ~ r2) + 
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    scale_linetype_discrete() +
    scale_y_continuous(trans = "log10") +
    facet_grid(r2~bandwidth, scales = "free", labeller = labeller(bandwidth = c(`0.1` = "σ = 0.1", 
                                                                                `1` = "σ = 1", 
                                                                                `10` = "σ = 10"),
                                                                  r2 = c(`0.1` = "R2 = 0.10", 
                                                                         `0.5` = "R2 = 0.50", 
                                                                         `0.75` = "R2 = 0.75", 
                                                                         `0.9` = "R2 = 0.90"))) +
    labs(x = "Sample size",
         y = "RMSE",
         colour = NULL,
         fill = NULL,
         linetype = NULL) +
    theme(legend.position = "bottom")  
  
  ggsave(sprintf("Simulation experiments/Figures/Results_Naive_%s_Estimator_%s_Normalization_%s_Model_%s.png", 
                 naive_, estimator_, normalization_, model_), 
         width = 160, height = 150, unit = "mm", dpi = 1000)
}

# Better performance was observed with a small batch size but difference were attenuated as the sample size increased.  
plot_this(FALSE, "default", "strictly_positive", "gam")