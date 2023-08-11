##################################################
## Project: Active Sampling
## Description: Active sampling experiments for toy data example
## Date: 11 Aug 2023
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())

# Set working directory. 
# setwd(getSrcDirectory(function(){})[1]) # If file is sourced. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Using RStudio.


# Load packages -----------------------------------------------------------

library("kernlab")
library("magrittr")
library("MASS")
library("mboost")
library("mgcv")
library("purrr")
library("ranger")
library("tidyverse")


# Plot settings. ----------------------------------------------------------

ptsize <- 8
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.0, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1, "cm"),
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background.x = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 0.25))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("text", list(size = ptsize / .pt, family = "serif"))


# Load functions ----------------------------------------------------------

source("functions.R")


# Active sampling ---------------------------------------------------------

# Clean-up.
rm(list = setdiff(ls(), lsf.str()))
cat("\14")

# Set parameters.
set.seed(7083974) # For reproducibility. 
N <- 1e3
nreps <- 1e3
params <- tibble(model = c("const", "lm", "gam", "gbt", "gpr", "rf"),
                 label = c("SRS", "Linear", "Thin plate spline", "Gradient boosting tree", "Gaussian process regression", "Random forest")) 

# Simulate data. 
dta <- sim_data(N = N) %>% 
  dplyr::select(-yhat) # Drop column yhat.

# Experiments.
allres <- NULL
for ( i in 1:nrow(params) ) {
  set.seed(8990219)
  for ( j in 1:nreps ) {
    res <- active_sampling(dta, model = params$model[i]) %>% 
      mutate(model = params$model[i], 
             rep = j)
    if ( i == 1 & j == 1 ) {
      allres  <- res 
    } else {
      allres %<>%
        add_row(res)
    }
  }
}

# Aggregate results. 
aggres <- allres %>% 
  group_by(model, n) %>% 
  summarise(rmse = sqrt(mean(sqerr)),
            mse_low = max(0, mean(sqerr) - 1.96 * sd(sqerr) / sqrt(length(sqerr))),
            mse_high = mean(sqerr) + 1.96 * sd(sqerr) / sqrt(length(sqerr)),
            rmse_low = sqrt(mse_low),
            rmse_high = sqrt(mse_high),
            rsq = mean(rsq, na.rm = TRUE), 
            sdsize = sqrt(mean(sdsize^2, na.rm = TRUE)), 
            sdw = sqrt(mean(sdw^2, na.rm = TRUE)), .groups = "keep")

save(file = "ActiveSamplingResults_230811.R", aggres)

# Plot. 
ggplot() + 
  geom_line(data = aggres, aes(x = n, y = rmse, colour = model, linetype = model), lwd = 0.25) + 
  geom_errorbar(data = aggres, aes(x = n, ymin = rmse_low, max = rmse_high, colour = model), width = 10, position = position_dodge(width = 5), show.legend = FALSE, lwd = 0.25) + 
  scale_colour_brewer(palette = "Dark2", breaks = params$model, labels = params$label) + 
  scale_linetype_discrete(breaks = params$model, labels = params$label) +
  scale_y_continuous(trans = "log10") + 
  labs(x = "Sample size", 
       y = "RMSE",
       colour = NULL, 
       linetype = NULL) +
  theme(legend.position = "right")

# Save.
ggsave("Figure_ActiveSampling.png", width = 120, height = 60, unit = "mm", dpi = 1000)
