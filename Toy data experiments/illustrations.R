##################################################
## Project: Active Sampling
## Description: Illustrations for toy data example
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

ptsize <- 10
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


# Plot mean trend, varying degree of non-linearity ------------------------

set.seed(1234) # For reproducibility. 
alldta <- NULL
nreps <- 3
bseq <- c(10^seq(-1, 0, 0.5), 10)

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
  facet_wrap(~bandwidth, labeller = labeller(bandwidth = c(`0.10` = "Highly non-linear (sigma = 0.1)",
                                                           `0.32`  = "Non-linear (sigma = 0.32)",
                                                           `1.00` = "Polynomial  (sigma = 1)",
                                                           `10.00` = "Roughly linear (sigma = 10)"))) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "z", 
       y = expression(hat(y))) + 
  theme(legend.position = "none")

ggsave("Figure_Nonlinear.png", width = 160, height = 120, unit = "mm", dpi = 1000)



# Plot realization, varying signal to noise ratio -------------------------

set.seed(1234) # For reproducibility. 
alldta <- NULL
r2 <- seq(0.1, 0.9, length.out = 4)

for ( i in seq_along(r2) ) {
  dta <- sim_data(r2 = r2[i]) %>% 
    mutate(r2 = sprintf("%.2f", r2[i]))
  if ( i == 1 ) {
    alldta <- dta
  } else {
    alldta %<>% add_row(dta)  
  }
}

ggplot(alldta) + 
  geom_line(aes(x = z, y = yhat), colour = "steelblue", lwd = 1) + 
  geom_point(aes(x = z, y = y)) + 
  facet_wrap(~r2, scales = "free", labeller = labeller(r2 = c(`0.10` = "Weak",
                                                              `0.37`  = "Moderate",
                                                              `0.63` = "Strong",
                                                              `0.90` = "Very strong"))) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "z", 
       y = "y") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("Figure_SignalToNoiseRatio.png", width = 160, height = 120, unit = "mm", dpi = 1000)



# Illustrate prediction models. -------------------------------------------

# Simulate data.
set.seed(12341) # For reproducibility. 
dta <- sim_data() 

model <- c("lm", "gam", "gbt", "gpr", "rf")
label <- c("Linear", "Thin plate spline", "Gradient boosting tree", "Gaussian process regression", "Random forest")
rmse <- rsq <- rep(NA, length = length(model)) 
allres <- dta %>% 
  mutate(model = "truth")

for ( i in seq_along(model) ) {
  
  fit <- train(dta, model[i])
  
  res <- tibble(model = model[i], 
                z = dta$z,
                y = dta$y,
                yhat = my_predict(fit, dta))
  
  rmse[i] <- get_rmse(fit)
  rsq[i] <- get_rsquared(fit, dta)
  
  allres %<>% 
    add_row(res)
}

labels <- vapply(1:length(model), function(ix) sprintf("%s (%.2f)", label[ix], rsq[ix]), character(1))
levels <- c("truth", model)
labels <- c(sprintf("Ground truth (%.2f)", summary(lm(y~yhat, data = dta))$r.squared), labels)
names(labels) <- paste(1:length(labels))

allres %<>% mutate(mod_num = as.character(as.numeric(factor(model, levels = levels, labels = labels))))

ggplot(data = allres) + 
  geom_line(aes(x = z, y = yhat, colour = mod_num), lwd = 1) + 
  geom_point(aes(x = z, y = y), size = 0.5) + 
  scale_color_brewer(palette = "Dark2", breaks = 1:6) + 
  facet_wrap(~mod_num, labeller = labeller(mod_num = labels)) + 
  labs(x = "z", 
       y = "y",
       colour = NULL, 
       linetype = NULL) +
  theme(legend.position = "none")

ggsave("Figure_PredictionModels.png", width = 180, height = 120, unit = "mm", dpi = 1000)
