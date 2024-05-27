##################################################
# Project: Active Sampling
# Description: Figures and illustrations for active sampling experiments on simulated data, Section 5 in the paper. 
#              This script generates Figure 2.
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


# Figure 2 ----------------------------------------------------------------

# Clean-up.
rm(list = ls())

# Initialize.
params <- crossing(bandwidth = c(0.1, 1, 10), r2 = 0.75)

# Load data. 
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

# Plot.
p <- ggplot(plt, aes(x = z, y = y)) + 
  geom_point() + 
  facet_wrap(~bandwidth) + 
  expand_limits(y = 0) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) + 
  scale_y_continuous(breaks = c(0, 2.5, 5)) + 
  facet_wrap(~bandwidth, labeller = labeller(bandwidth = c(`0.1` = "Non-linear (σ = 0.1)",
                                                           `1` = "Polynomial  (σ = 1)",
                                                           `10` = "Linear (σ = 10)"))) 

# Print.
print(p)

# Save.
ggsave("Simulation experiments/Figures/Figure 2.png", width = 160, height = 60, unit = "mm", dpi = 1000)

# Clean-up.
rm(p)
