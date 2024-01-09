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

library("magrittr")
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
             strip.background = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 0.25))
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
  crossing(bandwidth = 0.32, #c(0.1, 0.32, 1, 10),
           r2 = 0.5, #c(0.1, 0.25, 0.5, 0.75, 0.9),
           normalization = c("zero_mean", "strictly_positive"),
           naive = c(TRUE, FALSE), # Ignore prediction uncertainty (TRUE) or account for prediction uncertainty (FALSE)
           bsize = 50, # c(10, 50), # Small or large batch size. 
           estimator = c("default", "Hajek")) # Estimator for the finite population mean. 
  
models <- c("const", "lm", "gam")
labels <- c("Simple random sampling", "Linear model", "Generalized additive model")
  

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
    mutate(naive = ifelse(naive, "naive", "EMSE") , 
           mse_low = pmax(1e-6, mse - 1.96 * sd_mse / sqrt(nreps)),
           mse_high = mse + 1.96 * sd_mse / sqrt(nreps),
           rmse = sqrt(mse),
           rmse_low = sqrt(mse_low),
           rmse_high = sqrt(mse_high))
  
  if ( i == 1) {
    allres <- res
  } else {
    allres %<>% 
      add_row(res)
  }
  
  rm(res)
}
rm(i)


# 
allres %>% 
  # filter(bsize == 10 & !naive) %>% 
  ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
  geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
  geom_line(aes(y = rmse)) +
  facet_grid(bandwidth ~ r2) + 
  scale_colour_brewer(palette = "Dark2", breaks = models, label = labels) +
  scale_fill_brewer(palette = "Dark2", breaks = models, label = labels) +
  scale_linetype_discrete(breaks = models, label = labels) +
  scale_y_continuous(trans = "log10") +
  facet_grid(naive~normalization+estimator) +
  labs(x = "Sample size",
       y = "RMSE",
       colour = NULL,
       fill = NULL,
       linetype = NULL) +
  theme(legend.position = "bottom")


allres %>% 
  mutate(group = paste(normalization, estimator)) %>% 
  # filter(bsize == 10 & !naive) %>% 
  ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
  geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
  geom_line(aes(y = rmse)) +
  facet_grid(bandwidth ~ r2) + 
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_linetype_discrete() +
  scale_y_continuous(trans = "log10") +
  facet_grid(naive~model) +
  labs(x = "Sample size",
       y = "RMSE",
       colour = NULL,
       fill = NULL,
       linetype = NULL) +
  theme(legend.position = "bottom")

# allres %>% 
#   filter(bsize == 10 & naive) %>% 
#   ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
#   # geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
#   geom_line(aes(y = rmse)) +
#   facet_grid(bandwidth ~ r2) + 
#   scale_colour_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_fill_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_linetype_discrete(breaks = models, label = labels) +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth+normalization) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        fill = NULL,
#        linetype = NULL) +
#   theme(legend.position = "bottom")
# 
# allres %>% 
#   filter(bsize == 50 & !naive) %>% 
#   ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
#   # geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
#   geom_line(aes(y = rmse)) +
#   facet_grid(bandwidth ~ r2) + 
#   scale_colour_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_fill_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_linetype_discrete(breaks = models, label = labels) +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        fill = NULL,
#        linetype = NULL) +
#   theme(legend.position = "bottom")
# 
# allres %>% 
#   filter(bsize == 50 & naive) %>% 
#   ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
#   # geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
#   geom_line(aes(y = rmse)) +
#   facet_grid(bandwidth ~ r2) + 
#   scale_colour_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_fill_brewer(palette = "Dark2", breaks = models, label = labels) +
#   scale_linetype_discrete(breaks = models, label = labels) +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        fill = NULL,
#        linetype = NULL) +
#   theme(legend.position = "bottom")
# 
# allres %>% 
#   filter( ((model == "const" & bsize == 10) | model == "gam") & !naive ) %>% 
#   mutate(group = paste(model, bsize)) %>% 
#   ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
#   # geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
#   geom_line(aes(y = rmse)) +
#   facet_grid(bandwidth ~ r2) + 
#   scale_colour_brewer(palette = "Dark2") +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_linetype_discrete() +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        fill = NULL,
#        linetype = NULL) +
#   theme(legend.position = "bottom")
# 
# allres %>% 
#   filter( ((model == "const" & !naive) | model %in% c("lm", "gam")) & bsize == 10 ) %>% 
#   mutate(group = paste(model, naive)) %>% 
#   ggplot(aes(x = n, color = group, fill = group, linetype = group)) +
#   # geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), alpha = 0.5) + 
#   geom_line(aes(y = rmse)) +
#   facet_grid(bandwidth ~ r2) + 
#   scale_colour_brewer(palette = "Dark2") +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_linetype_discrete() +
#   scale_y_continuous(trans = "log10") +
#   facet_grid(r2~bandwidth) +
#   labs(x = "Sample size",
#        y = "RMSE",
#        colour = NULL,
#        fill = NULL,
#        linetype = NULL) +
#   theme(legend.position = "bottom")


# # Test performance gain vs simple random sampling.
# for ( i in 1:nrow(params) ) {
#   if ( params$model[i] != "const" ) {
#     
#     tmp <- allres %>% 
#       filter( model %in% c("const", params$model[i])
#               & bsize == params$bsize[i]
#               & naive == params$naive[i] 
#               & bandwidth == params$bandwidth[i]
#               & r2 == params$r2[i])
#     
#     nseq <- sort(unique(tmp$n))
#     
#     for ( j in 1:length(nseq) ) {
# 
#       test <- t.test(sqerr~model, var.equal = FALSE, data = tmp %>% filter(n == nseq[j]))
#       
#       aggres[which(aggres$model == params$model[i]
#                    & aggres$bsize == params$bsize[i]
#                    & aggres$naive == params$naive[i] 
#                    & aggres$bandwidth == params$bandwidth[i]
#                    & aggres$r2 == params$r2[i]
#                    & aggres$n == nseq[j]), "pval"] <- test$p.value
#     }
#     
#   }
# }
#   
# 
# # Prepare for plotting.
# plt <- aggres %>%
#   mutate(star = ifelse(pval < 0.05, "*", ""),
#          model_num = as.numeric(factor(model, levels = unique(params$model))),
#          ystar = max(rmse_high) - (max(rmse_high) - min(rmse_low)) * model_num * 0.025)
# 
# 
# # Plot.
# plot_this <- function(naive_, bsize_) {
#   
#   ggplot(plt %>% filter(naive == naive_ & bsize == bsize_)) +
#     geom_line(aes(x = n, y = rmse, colour = model, linetype = model), lwd = 0.25) +
#     geom_errorbar(aes(x = n, ymin = rmse_low, max = rmse_high, colour = model), width = 5, position = position_dodge(width = 2.5), show.legend = FALSE, lwd = 0.25) +
#     geom_text(aes(x = n, y = ystar, label = star, colour = model), show.legend = FALSE) +
#     scale_colour_brewer(palette = "Dark2", breaks = models, labels = labels) +
#     scale_linetype_discrete(breaks = models, labels = labels) +
#     scale_x_continuous(breaks = c(0, nseq)) +
#     scale_y_continuous(trans = "log10") +
#     facet_grid(r2~bandwidth) +
#     labs(x = "Sample size",
#          y = "RMSE",
#          colour = NULL,
#          linetype = NULL) +
#     theme(legend.position = "right")
#   
#   ggsave(sprintf("Figure_ActiveSampling_Naive%s_Bsize%d.png", naive_, bsize_), width = 120, height = 200, unit = "mm", dpi = 1000)
# }
# 
# plot_this(TRUE, 10)
# plot_this(TRUE, 50)
# plot_this(FALSE, 10)
# plot_this(FALSE, 50)
