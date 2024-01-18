# Function to check and install missing packages, then load them
load_required_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      message(paste("Package", package, "is not installed. Attempting to install now."))
      install.packages(package)
    }
    # Load the package after installation or if it was already installed
    library(package, character.only = TRUE)
  }
}

# List of packages required for the script
required_packages <- c("ggplot2", "ggpubr", "scales", "dplyr","RColorBrewer")

# Load required packages
load_required_packages(required_packages)

source("Application/Rscript/CI_calculation.R")

# Define a color-blind friendly palette
color_name = "Dark2"
seed_number = 5
load_color = brewer.pal(8, color_name)
set.seed(seed_number)
cb_palette <- sample(load_color)

# load the result
load("Application/Results/result_500groups_eRMSE.R")

simlist = list()
for (i in 1:length(res_total[[1]])) {
  simlist[[i]] <- res_total[[1]][[i]]$res
}

total_sum = do.call(rbind, simlist)
selected_total_sum <- subset(total_sum, opt_method != "naive")

selected_total_sum <- selected_total_sum %>%
  mutate(
    crash_avoidance_lower_ci = 0,
    mean_impact_speed_reduction_lower_ci = 0,
    crash_avoidance_upper_ci = 0,
    mean_impact_speed_reduction_upper_ci = 0,
    crash_avoidance_RMSE = 0,
    mean_impact_speed_reduction_RMSE = 0,
    crash_avoidance_err_vector = 0,
    mean_impact_speed_reduction_err_vector = 0
  )


groups = c("Simple random sampling",
           "Density importance sampling",
           "Severity importance sampling",
           "Active sampling, target = mean impact speed reduction",
           "Active sampling, target = crash avoidance rate")

selected_total_sum$new_group = NA
selected_total_sum[selected_total_sum$target == "impact speed reduction",]$new_group = groups[4]
selected_total_sum[selected_total_sum$target == "crash avoidance",]$new_group = groups[5]
selected_total_sum[selected_total_sum$proposal_dist == "density sampling",]$new_group = groups[2]
selected_total_sum[selected_total_sum$proposal_dist == "severity sampling",]$new_group = groups[3]
selected_total_sum[selected_total_sum$sampling_method == "simple random sampling",]$new_group = groups[1]
selected_total_sum$new_group <- factor(selected_total_sum$new_group, levels = 
                                         groups)
k = 0
ci_df = selected_total_sum[FALSE,]
for (i in unique(selected_total_sum$new_group)){
  k = k+1
  for (j in unique(selected_total_sum$iter)){
    selected_one_group = selected_total_sum %>% filter(new_group ==i,
                                                       iter == j)
    one_out_crash_avoidance = CI_calculation(data = selected_one_group,
                                             parameter = "crash_avoidance") #"mean_impact_speed_reduction"
    
    selected_one_group$crash_avoidance_lower_ci = one_out_crash_avoidance$ci_lower
    selected_one_group$crash_avoidance_upper_ci = one_out_crash_avoidance$ci_upper
    selected_one_group$crash_avoidance_err_vector = list(one_out_crash_avoidance$errors)
    selected_one_group$crash_avoidance_RMSE = one_out_crash_avoidance$rmse
    
    one_out_mean_impact_speed_reduction = CI_calculation(data = selected_one_group,
                                                         parameter = "mean_impact_speed_reduction") #"mean_impact_speed_reduction"
    selected_one_group$mean_impact_speed_reduction_lower_ci = one_out_mean_impact_speed_reduction$ci_lower
    selected_one_group$mean_impact_speed_reduction_upper_ci = one_out_mean_impact_speed_reduction$ci_upper
    selected_one_group$mean_impact_speed_reduction_err_vector = list(one_out_mean_impact_speed_reduction$errors)
    selected_one_group$mean_impact_speed_reduction_RMSE = one_out_mean_impact_speed_reduction$rmse
    
    ci_df = rbind(ci_df, selected_one_group[1,])
  }
}

ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.0, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1.65, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin= margin(0,200,0,200),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             strip.background.x = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0.5, vjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"),
             plot.margin = margin(t = -0.3, r = 5.5, b = 5.5, l = 5, unit = "points"))

# Plot
g1 <- ggplot() +
  geom_line(data = ci_df, aes(x =  iter* batch_size, y = mean_impact_speed_reduction_RMSE,
                              colour = new_group, linetype = new_group), size = 1) +
  geom_ribbon(data = ci_df, aes(x =  iter* batch_size, 
                                                          ymin = mean_impact_speed_reduction_lower_ci, 
                                                          ymax = mean_impact_speed_reduction_upper_ci, 
                                                          fill = new_group), alpha = 0.3,show.legend = FALSE) +  # Adjust alpha for transparency
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +  # Adjust upper limit
  labs(x = "Sample size", 
       y = expression("eRMSE"),  # Updated y-axis label
       colour = NULL, linetype = NULL) +
  ggtitle("Mean impact speed reduction")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 5)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_fill_manual(values = cb_palette) +  # Set custom CI area colors
  theme(legend.position = 'bottom')


# Plot
g2 <- ggplot() +
  geom_line(data = ci_df, aes(x =  iter* batch_size, y = crash_avoidance_RMSE,
                              colour = new_group, linetype = new_group), size = 1) +
  geom_ribbon(data = ci_df, aes(x =  iter* batch_size, 
                                                   ymin = crash_avoidance_lower_ci, 
                                                   ymax = crash_avoidance_upper_ci, 
                                                   fill = new_group), alpha = 0.3,show.legend = FALSE) +  # Adjust alpha for transparency
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Sample size", y = "",
       colour = NULL, linetype = NULL) +
  ggtitle("Crash avoidance rate")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 5)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values =  c(1, 2, 3, 4, 5))  +
  scale_fill_manual(values = cb_palette) +  # Set custom CI area colors
  theme(legend.position = 'bottom')
c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

ggsave(sprintf(paste("Application/Figures/","CI_active_sampling_vs_importance_sampling.png",sep = "")), c1, dpi = 1000, width =160, height =75, unit = "mm")
