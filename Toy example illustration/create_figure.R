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
library("tidyverse")


# ggplot theme. ----

ptsize <- 8
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


# Prepare data. ----

load("Toy example illustration/data.R")
df %<>% 
  filter(caseID == 22 & eoff <= 2) %>% 
  mutate(caseID = as.numeric(caseID)) %>% 
  gather(impact_speed0, impact_speed1, key = "scenario", value = impact_speed)

ggplot(df) + 
  geom_rect(aes(xmin = eoff - 0.05, xmax = eoff + 0.05, ymin = -acc - 0.25, ymax = -acc + 0.25, fill = impact_speed)) +
  facet_wrap(~scenario, 
             labeller = labeller(scenario = c(impact_speed0 = "Baseline manual driving scenario", 
                                              impact_speed1 = "With automatic emergency braking system"))) + 
  scale_fill_continuous(type = "viridis") +
  labs(x = "Off-road glance duration (s)",
       y = bquote('Maximal deceleration '(km/s^2)),
       fill = "Impact speed (km/h)") 

ggsave("Toy example illustration/toy example illustration.png", dpi = 1000, width = 159, height = 60, unit = "mm")
