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

# Define a color-blind friendly palette
color_name = "Dark2"
seed_number = 5
load_color = brewer.pal(8, color_name)
set.seed(seed_number)
cb_palette <- sample(load_color)

# Load the result
load("Application/Results/result_500repetitions_coverage.R")
simlist = list()
for (i in 1:length(res_total[[1]])) {
  sim <- res_total[[1]][[i]]$res
  simlist[[i]] <- sim
}
batch_size = 10
total_sum = do.call(rbind, simlist)
impact_speed_reduction_sum <- subset(total_sum, target == "impact speed reduction")
crash_avoidance_sum <- subset(total_sum, target == "crash avoidance")

impact_speed_reduction_sum <- impact_speed_reduction_sum %>%
  mutate(
    impact_speed_reduction_cover = 0,
    estimation_methods = NA
  )

crash_avoidance_sum <- crash_avoidance_sum %>%
  mutate(
    crash_avoidance_cover = 0,
    estimation_methods = NA
  )
estimation_methods = c("Bootstrap estimator",
                       "Pooled Sen-Yates-Grundy estimator",
                       "Martingale estimator")
k = 0
impact_speed_reduction_cover_df = impact_speed_reduction_sum[FALSE,]
for (i in unique(estimation_methods)){
  k = k+1
  for (j in unique(impact_speed_reduction_sum$iter)){
    selected_one_group = impact_speed_reduction_sum %>% filter(iter == j)
    selected_one_group$estimation_methods = i
    if (i == "Bootstrap estimator"){
      selected_one_group$impact_speed_reduction_cover =  sum(selected_one_group$mean_impact_speed_reduction_ci_cover_boot, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Pooled Sen-Yates-Grundy estimator"){
      selected_one_group$impact_speed_reduction_cover =  sum(selected_one_group$mean_impact_speed_reduction_ci_cover_classic, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Martingale estimator"){
      selected_one_group$impact_speed_reduction_cover =  sum(selected_one_group$mean_impact_speed_reduction_ci_cover_mart, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Martingale estimator (corrected)"){
      selected_one_group$impact_speed_reduction_cover =  sum(selected_one_group$mean_impact_speed_reduction_ci_cover_mart_corr, na.rm = TRUE)/length(selected_one_group$iter)
    }
    impact_speed_reduction_cover_df = rbind(impact_speed_reduction_cover_df, selected_one_group[1,])
  }
}

k = 0
crash_avoidance_cover_df = crash_avoidance_sum[FALSE,]
for (i in unique(estimation_methods)){
  k = k+1
  for (j in unique(crash_avoidance_sum$iter)){
    selected_one_group = crash_avoidance_sum %>% filter(iter == j)
    selected_one_group$estimation_methods = i
    if (i == "Bootstrap estimator"){
      selected_one_group$crash_avoidance_cover =  sum(selected_one_group$crash_avoidance_rate_ci_cover_boot, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Pooled Sen-Yates-Grundy estimator"){
      selected_one_group$crash_avoidance_cover =  sum(selected_one_group$crash_avoidance_rate_ci_cover_classic, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Martingale estimator"){
      selected_one_group$crash_avoidance_cover =  sum(selected_one_group$crash_avoidance_rate_ci_cover_mart, na.rm = TRUE)/length(selected_one_group$iter)
    }else if(i == "Martingale estimator (corrected)"){
      selected_one_group$crash_avoidance_cover =  sum(selected_one_group$crash_avoidance_rate_ci_cover_mart_corr, na.rm = TRUE)/length(selected_one_group$iter)
    }
    crash_avoidance_cover_df = rbind(crash_avoidance_cover_df, selected_one_group[1,])
  }
}

impact_speed_reduction_cover_df$estimation_methods = factor(impact_speed_reduction_cover_df$estimation_methods, levels =
                                                              c("Pooled Sen-Yates-Grundy estimator",
                                                                "Martingale estimator",
                                                                "Martingale estimator (corrected)",
                                                                "Bootstrap estimator"))

crash_avoidance_cover_df$estimation_methods = factor(crash_avoidance_cover_df$estimation_methods, levels =
                                                       c("Pooled Sen-Yates-Grundy estimator",
                                                         "Martingale estimator","Martingale estimator (corrected)",
                                                         "Bootstrap estimator"))
ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 5.0, r = 0.2, l = 0.25, unit = 'cm')),
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
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0.5,vjust = -1.5),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

# With mean impact speed reduction target active sampling plot for coverage
g1 <- ggplot(impact_speed_reduction_cover_df,
             aes(x = iter*batch_size, y = impact_speed_reduction_cover,
                 colour = estimation_methods, linetype = estimation_methods)) +
  geom_line(size = 1) +
  xlim(0, 1000) +
  scale_y_continuous(limits = c(0.7,1),labels = scales::label_percent(scale = 100), breaks = seq(0.7, 1, by = 0.1)) +  # Format y-axis labels as percentages
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") + 
  labs(x = "Sample size", y = "Coverage rate",  # Theta as a Greek letter
       colour = NULL, linetype = NULL, fill = NULL) +
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 4)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c(2, 1, 3,4)) +
  theme(legend.position = c(1, -1))

# With crash avoidance rate target active sampling plot for coverage
g2 <- ggplot(crash_avoidance_cover_df,
             aes(x = iter*batch_size, y = crash_avoidance_cover,
                 colour = estimation_methods, linetype = estimation_methods)) +
  geom_line(size = 1) +
  xlim(0, 1000) +
  scale_y_continuous(limits = c(0.7,1),labels = scales::label_percent(scale = 100), breaks = seq(0.7, 1, by = 0.1)) +  # Format y-axis labels as percentages
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") + 
  labs(x = "Sample size", y = "",  # Remove y-axis label
       colour = NULL, linetype = NULL, fill = NULL) +
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 4)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c(2, 1, 3,4)) +
  theme(legend.position = c(1, -1))

# Arrange g1 and g2 with a common legend
c1 <- ggarrange(g1, g2,
                ncol = 2, nrow = 1, common.legend = TRUE, align = "hv", labels = c('A', 'B'),
                font.label = list(size = 10, color = "black"), legend = "bottom")
ggsave(sprintf(paste("Application/Figures/","95_coverage_plot.png",sep = "")), c1, dpi = 1000, width =160, height =70, unit = "mm")