################################################################################
# Replication of Figure4.R
#
# INPUT:
#
# data: input dataset with variables and data loaded by load("Application/Data/Data.R")
#
# load sampling method input and it is loaded by sampling_input <- read_excel("Application/Input/sampling_method_input_coverage_example.xlsx")
#   - sampling_method: simple random sampling, importance sampling, or active sampling.
#   - proposal_dist: proposal distribution for importance sampling. 
#   - target: target of optimisation. Only used when sampling_method = "active sampling".
#   - opt_method: method for finding optimal sampling scheme. Only used when sampling_method = "active sampling".
# load simulation parameter input by param_input <- read_excel("Application/Input/parameter_input_coverage_example.xlsx")
#   - n_repetition: number of repeated simulations with the same sampling method and different seeds. 
#   - batch_size: number of observations to sample per iteration. 
#   - niter: number of iterations.
#   - nboot: number of bootstrap replicates used to calculate bootstrap confidence intervals.
#
# OUTPUT:
#
# Replication of Figure 4 is saved under "Replication of main results" folder with name "Replication_Figure4.png".
#
# SUMMARY
#
# This replication file first load data and all sampling method and parameter input and call active sampling method.
# Then analyze based on the simulation results and produce and save the Figure4 under "Replication of main results" folder
# with name "Replication_Figure4.png".
# The process bar with estimated time is printed when the simulations are running.
################################################################################

# source the sampling scripts
source("Application/Rscript/active_sampling.R")
source("Application/Rscript/load_required_packages.R")
# List of packages required for the script
required_packages <- c("ggplot2", "ggpubr", "scales", "dplyr","RColorBrewer","progress","readxl")
# Load required packages
load_required_packages(required_packages)
library("readxl")
# load the experiment data
load("Application/Data/Data.R")
# load which sampling methods to run
sampling_input <- read_excel("Application/Input/sampling_method_input_coverage_example.xlsx")

# load how many simulations to run
param_input <- read_excel("Application/Input/parameter_input_coverage_example.xlsx")
max_sample_size = 2000
niter = ceiling(max_sample_size/param_input$batch_size)
res_total= replicate(length(param_input$n_repetition), data.frame())
prediction_model_type = "rg" # "xg_boost" "rg" "knn" "Gaussian"
# Set up progress bar.
pb <- progress_bar$new(format = "[:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = param_input$n_repetition*tail(sampling_input$sim_order, 1),
                       clear = FALSE)      
for(j in 1:length(param_input$n_repetition)){
  inputparameter <- data.frame(param_input$batch_size[j],niter[j],param_input$nboot[j],param_input$n_repetition[j])
  colnames(inputparameter) <- c("batch_size","niter", "nboot","n_repetition")
  res_top_loop = replicate(tail(sampling_input$sim_order, 1), data.frame())
  # Update progress bar.
  for(i in 1:tail(sampling_input$sim_order, 1)){
    
    res_list <- replicate(inputparameter$n_repetition, data.frame())
    for (k in 1:inputparameter$n_repetition){
      pb$tick()
      set.seed(k)
      out <- active_sampling (df, 
                              sampling_input[i,]$sampling_method, 
                              sampling_input[i,]$proposal_dist,
                              sampling_input[i,]$target, 
                              sampling_input[i,]$opt_method,
                              inputparameter$batch_size,
                              inputparameter$niter,
                              inputparameter$nboot,
                              verbose = FALSE,
                              plot = FALSE,
                              prediction_model_type)
      res_list[[k]] <- out$results
    }
    res <- do.call(rbind, res_list)
    res_in_loop <- list(res = res)
    res_top_loop[[i]] <- res_in_loop
  }
  res_total[[j]] <- res_top_loop
}  

# Define a color-blind friendly palette
color_name = "Dark2"
seed_number = 5
load_color = brewer.pal(8, color_name)
set.seed(seed_number)
cb_palette <- sample(load_color)

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
ggsave(sprintf(paste("Replication of main results/","Replication_Figure4.png",sep = "")), c1, dpi = 1000, width =160, height =70, unit = "mm")

