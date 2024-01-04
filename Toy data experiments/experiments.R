##################################################
## Project: Active Sampling
## Description: Active sampling experiments for toy data example
## Date: 11 Aug 2023
## Author: Henrik Imberg
##################################################

# Clean-up.
rm(list = ls())
gc()

# Set working directory. 
# setwd(getSrcDirectory(function(){})[1]) # If file is sourced. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Using RStudio.


# Load packages -----------------------------------------------------------

library("bigstatsr")
library("doParallel")
library("foreach")
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
N <- 1e3
nreps <- 1e4
ninit <- 50
bsize <- 50
niter <- 5
params <- tibble(model = c("const", "lm", "gam", "rf"), # "gbt", "gpr", # 
                 label = c("SRS", "Linear", "Thin plate spline", "Random forest"))  %>% # "Gradient boosting tree", "Gaussian process regression", 
  crossing(naive = c(TRUE, FALSE), 
           bandwidth = c(0.1, 10),
           r2 = c(0.5, 0.9)) 

# Simulate data.
set.seed(7083974) # For reproducibility. 
r2_seq <- unique(params$r2)
bandwidth_seq <- unique(params$bandwidth)
for ( i in seq_along(r2_seq) ) {
  for ( j in seq_along(bandwidth_seq) ) {
    
    dta <- sim_data(N = N) %>% 
      dplyr::select(-yhat) # Drop column yhat.
    
    # Store.
    if ( i == 1 & j == 1 ) {
      alldta <- tibble(r2 = r2_seq[i], 
                       bandwidth = bandwidth_seq[j], 
                       dta = list(dta))
    } else {
      alldta %<>% 
        add_row(tibble(r2 = r2_seq[i], 
                       bandwidth = bandwidth_seq[j], 
                       dta = list(dta)))
      
    }
  }
}
rm(r2_seq, bandwidth_seq)


# Setup parallel backend.
# cl <- makeCluster(10) 
# registerDoParallel(cl)


# Experiments.
for ( i in 1:nrow(params) ) {

  # Fetch data.
  dta <- alldta[alldta$r2 == params$r2[i] & alldta$bandwidth == params$bandwidth[i], "dta"][[1]] %>% 
    as.data.frame()
  
  # To store results. 
  sqerr <- FBM(nrow = niter, ncol = nreps, init = NA) 
  
  # Run repetitions in parallel.
  # val <- foreach(j = 1:nreps, .combine = rbind, .packages = c("purrr")) %dopar% {
  for ( j in 1:nreps ) {
    
    # Active sampling.
    res <- active_sampling(data = dta, 
                           ninit = ninit, 
                           bsize = bsize, 
                           niter = niter, 
                           model = params$model[i],
                           naive = params$naive[i]) 
    
    # Store results 
    sqerr[, j] <- res$sqerr
    
    1 # foreach return value
  }
  
  res <- tibble(rep = rep(1:nreps, each = niter), 
                n = rep(cumsum(c(ninit, rep(bsize, niter - 1))), nreps),
                model = params$model[i],
                label = params$label[i],
                naive = params$naive[i],
                bandwidth = params$bandwidth[i],
                r2 = params$r2[i],
                sqerr = as.numeric(sqerr[])) 
  
  save(res, file = sprintf("%s_%s_%s_%s.RData", 
                           params$model[i],
                           params$naive[i],
                           params$bandwidth[i],
                           params$r2[i]))
  
  rm(res, sqerr)
  
}
# stopCluster(cl)
# rm(cl, dta, bsize, i, j, N, ninit, niter, nreps)
rm(dta, bsize, i, j, N, ninit, niter, nreps)



# Post-processing ---------------------------------------------------------


# Load results datasets and calculate summary statistics. 
allres <- aggres <- NULL
for (i in 1:nrow(params) ) {
  load(file = sprintf("%s_%s_%s_%s.RData", 
                      params$model[i],
                      params$naive[i],
                      params$bandwidth[i],
                      params$r2[i]))
  
  summary <- res %>%
    group_by(n) %>%
    summarise(rmse = sqrt(mean(sqerr)),
              mse_low = max(0, mean(sqerr) - 1.96 * sd(sqerr) / sqrt(length(sqerr))),
              mse_high = mean(sqerr) + 1.96 * sd(sqerr) / sqrt(length(sqerr)),
              rmse_low = sqrt(mse_low),
              rmse_high = sqrt(mse_high), .groups = "keep") %>%
    mutate(pval = NA_real_) %>%
    ungroup() %>% 
    mutate(model = params$model[i],
           naive = params$naive[i],
           bandwidth = params$bandwidth[i],
           r2 = params$r2[i])
    
  if ( i == 1) {
    allres <- res
    aggres <- summary
  } else {
    allres %<>% 
      add_row(res)
    
    aggres %<>% 
      add_row(summary)
  }
}
rm(i)


# Test performance gain vs simple random sampling.
nseq <- sort(unique(aggres$n))
for ( i in 1:nrow(params) ) {
  if ( params$model[i] != "const" ) {
    for ( j in 1:length(nseq) ) {
      
      tmp <- allres %>% 
        filter( model %in% c("const", params$model[i])
                & naive == params$naive[i] 
                & bandwidth == params$bandwidth[i]
                & r2 == params$r2[i]
                & n == nseq[j])
      
      test <- t.test(sqerr~model, var.equal = FALSE, data = tmp)
      
      aggres[which(aggres$model == params$model[i]
                   & aggres$naive == params$naive[i] 
                   & aggres$bandwidth == params$bandwidth[i]
                   & aggres$r2 == params$r2[i]
                   & aggres$n == nseq[j]), "pval"] <- test$p.value
    }
    
  }
}
  

# Prepare for plotting.
plt <- aggres %>%
  mutate(star = ifelse(pval < 0.05, "*", ""),
         model_num = as.numeric(factor(model, levels = unique(params$model))),
         ystar = max(rmse_high) - (max(rmse_high) - min(rmse_low)) * model_num * 0.025)


# Plot.
ggplot(plt %>% filter(!naive)) +
  geom_line(aes(x = n, y = rmse, colour = model, linetype = model), lwd = 0.25) +
  geom_errorbar(aes(x = n, ymin = rmse_low, max = rmse_high, colour = model), width = 5, position = position_dodge(width = 2.5), show.legend = FALSE, lwd = 0.25) +
  geom_text(aes(x = n, y = ystar, label = star, colour = model), show.legend = FALSE) +
  scale_colour_brewer(palette = "Dark2", breaks = params$model, labels = params$label) +
  scale_linetype_discrete(breaks = params$model, labels = params$label) +
  scale_x_continuous(breaks = c(0, nseq)) +
  scale_y_continuous(trans = "log10") +
  facet_grid(r2~bandwidth) +
  labs(x = "Sample size",
       y = "RMSE",
       colour = NULL,
       linetype = NULL) +
  theme(legend.position = "right")

ggsave("Figure_ActiveSampling1.png", width = 120, height = 60, unit = "mm", dpi = 1000)

# # Naive. 
# ggplot(plt %>% filter(!naive)) +
#   geom_line(aes(x = n, y = rmse, colour = model, linetype = model), lwd = 0.25) +
#   geom_errorbar(aes(x = n, ymin = rmse_low, max = rmse_high, colour = model), width = 5, position = position_dodge(width = 2.5), show.legend = FALSE, lwd = 0.25) +
#   geom_text(aes(x = n, y = ystar, label = star, colour = model), show.legend = FALSE) +
#   scale_colour_brewer(palette = "Dark2", breaks = params$model, labels = params$label) +
#   scale_linetype_discrete(breaks = params$model, labels = params$label) +
#   scale_x_continuous(breaks = c(0, nseq)) +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        linetype = NULL) +
#   theme(legend.position = "right")
# 
# # Save.
# ggsave("Figure_ActiveSampling2.png", width = 120, height = 60, unit = "mm", dpi = 1000)