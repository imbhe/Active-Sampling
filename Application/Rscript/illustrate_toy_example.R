################################################################################
#
# File name: figures.R
#
# Author: Henrik Imberg
#
# Last edited: 2023-04-05
#
# Description: Generate Figure S1 and S2 in Appendix B.
# 
# INPUT: Virtual simulation data stored in the Data folder.
#
# OUTPUT: is stored in the Figures folder.
#
################################################################################

# Clean up. ----
rm(list = ls())
cat("\14")


# Load packages. ----

library("cowplot")
library("RColorBrewer")
library("tidyverse")


# ggplot theme. ----

ptsize <- 10
theme_set(theme_classic()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.2), 
             axis.ticks = element_line(colour = "black", linewidth = 0.2), 
             axis.title.x = element_text(margin = margin(t = 0.25, r = 0, b = 0, l = 0, unit = 'cm')),
             axis.title.y = element_text(margin = margin(t = 0, r = 0.25, b = 0, l = 0, unit = 'cm')),
             legend.background = element_blank(),
             legend.key.width = unit(0.5, "cm"),
             legend.key.height = unit(0.5, "cm"),
             legend.margin = margin(0, 0, 0, 0.5, "cm"),
             legend.spacing =  unit(0, "cm"),
             legend.position = "right",
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0.5),
             text = element_text(size = ptsize, colour = "black", family = "serif"))
update_geom_defaults("text", list(size = ptsize / ggplot2:::.pt))


# Plot. ----

load("Application/Data/data.R")
source("Application/Rscript/active_sampling.R")
source("Application/Rscript/estimate_targets.R")

set.seed(4361) # For reproducibility. 

df %<>% 
  filter(caseID == 22 & eoff <= 2) %>% 
  mutate(eoff_acc_prob = 1)

estimate_targets(df, weightvar = "eoff_acc_prob")

as <- active_sampling(df, 
                      sampling_method = "active sampling", 
                      target = "impact speed reduction", 
                      batch_size = 100, 
                      niter = 100, 
                      nboot = 0)

plt <- df %>% 
  mutate(prob = as$prob)

fig1 <- ggplot(df) + 
  geom_rect(aes(xmin = -acc - 0.25, xmax = -acc + 0.25, ymin = eoff - 0.05, ymax = eoff + 0.05, fill = impact_speed0 - impact_speed1)) +
  scale_fill_continuous(type = "viridis", breaks = c(0, 25, 50)) + 
  labs(x = bquote('Maximal deceleration '(m/s^2)),
       y = "Off-road glance duration (s)",
       fill = "Impact speed reduction (km/h)") + 
  expand_limits(fill = 50) + 
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0)) +   
  theme(legend.position = "top", 
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

fig2 <- ggplot(plt) + 
  geom_rect(aes(xmin = -acc - 0.25, xmax = -acc + 0.25, ymin = eoff - 0.05, ymax = eoff + 0.05, fill = prob)) +
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, "Spectral")))(11), breaks = c(0.001, 0.005, 0.01), labels = c("small", "moderate", "large")) + 
  labs(x = bquote('Maximal deceleration '(m/s^2)),
       y = "Off-road glance duration (s)",
       fill = "Sampling probability") + 
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0)) + 
  theme(legend.position = "top", 
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

plot_grid(fig1, fig2, labels = c("A", "B"), label_size = ptsize)

ggsave("Application/Figures/active sampling example.png", dpi = 1000, width = 159, height = 70, unit = "mm")
