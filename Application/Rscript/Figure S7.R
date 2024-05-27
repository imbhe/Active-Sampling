ci_df_calculation <- function(file = "")
{
  load(paste("Application/Results/",file,sep = ""))
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
      mean_impact_speed_reduction_err_vector = 0,
      crash_avoidance_lower_sd_ci = 0,
      mean_impact_speed_reduction_lower_sd_ci = 0,
      crash_avoidance_upper_sd_ci = 0,
      mean_impact_speed_reduction_upper_sd_ci = 0
    )
  
  selected_total_sum$new_group = NA
  selected_total_sum[selected_total_sum$target == "impact speed reduction",]$new_group = "Active sampling, target = mean impact speed reduction"
  selected_total_sum[selected_total_sum$target == "crash avoidance",]$new_group = "Active sampling, target = crash avoidance rate"
  selected_total_sum[selected_total_sum$proposal_dist == "density sampling",]$new_group = "Density importance sampling"
  selected_total_sum[selected_total_sum$proposal_dist == "severity sampling",]$new_group = "Severity importance sampling"
  selected_total_sum[selected_total_sum$sampling_method == "simple random sampling",]$new_group = "Simple random sampling"
  
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
      selected_one_group$crash_avoidance_lower_sd_ci = one_out_crash_avoidance$ci_sd_lower
      selected_one_group$crash_avoidance_upper_sd_ci = one_out_crash_avoidance$ci_sd_upper
      selected_one_group$crash_avoidance_err_vector = list(one_out_crash_avoidance$errors)
      selected_one_group$crash_avoidance_RMSE = one_out_crash_avoidance$rmse
      
      one_out_mean_impact_speed_reduction = CI_calculation(data = selected_one_group,
                                                           parameter = "mean_impact_speed_reduction") #"mean_impact_speed_reduction"
      selected_one_group$mean_impact_speed_reduction_lower_ci = one_out_mean_impact_speed_reduction$ci_lower
      selected_one_group$mean_impact_speed_reduction_upper_ci = one_out_mean_impact_speed_reduction$ci_upper
      selected_one_group$mean_impact_speed_reduction_lower_sd_ci = one_out_mean_impact_speed_reduction$ci_sd_lower
      selected_one_group$mean_impact_speed_reduction_upper_sd_ci = one_out_mean_impact_speed_reduction$ci_sd_upper
      selected_one_group$mean_impact_speed_reduction_err_vector = list(one_out_mean_impact_speed_reduction$errors)
      selected_one_group$mean_impact_speed_reduction_RMSE = one_out_mean_impact_speed_reduction$rmse
      
      ci_df = rbind(ci_df, selected_one_group[1,])
    }
  }
  ci_df <- ci_df %>%
    select(-any_of(c("mean_impact_speed_reduction_se_mart_corr",
                     "mean_impact_speed_reduction_ci_cover_mart_corr",
                     "crash_avoidance_rate_se_mart_corr",
                     "crash_avoidance_rate_ci_cover_mart_corr")))
  return(ci_df)
}


source("Application/Rscript/CI_calculation.R")
library("dplyr")
prediction_models <- NULL
rf = ci_df_calculation(file = "result_100repetitions_rg.R")

rf <- rf %>% 
  filter(sampling_method %in% "active sampling" &
           opt_method != "naive") %>%
  mutate(prediction_type = "Random forests")

knn = ci_df_calculation(file = "result_100repetitions_knn.R")
knn <- knn %>% 
  filter(sampling_method %in% "active sampling" &
           opt_method != "naive") %>%
  mutate(prediction_type = "k-nearest neighbors")

xgb = ci_df_calculation(file = "result_100repetitions_xgboost.R")
xgb <- xgb %>% 
  filter(sampling_method %in% "active sampling" &
           opt_method != "naive") %>%
  mutate(prediction_type = "Extreme gradient boosting")


prediction_models = rbind(rf,knn,xgb)


# k-nearest neighbors
groups = c("Random forests",
           "Extreme gradient boosting",
           "k-nearest neighbors")
prediction_models$prediction_type <- factor(prediction_models$prediction_type, levels = 
                                              groups)
cb_palette = c("#7570B3","#D95F02","#1B9E77" )
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
  geom_line(data = prediction_models %>% filter (target %in% "impact speed reduction"), aes(x =  iter* batch_size, y = mean_impact_speed_reduction_RMSE,
                                                                                            colour = prediction_type, linetype = prediction_type), size = 1) +
  geom_ribbon(data = prediction_models %>% filter (target %in% "impact speed reduction"), aes(x =  iter* batch_size, 
                                                                                              ymin = mean_impact_speed_reduction_lower_ci, 
                                                                                              ymax = mean_impact_speed_reduction_upper_ci, 
                                                                                              fill = prediction_type), alpha = 0.4,show.legend = FALSE) +  # Adjust alpha for transparency
  
  xlim(0, 2000) +
  scale_y_continuous(trans = 'log10', labels = scales::label_number()) +  # Log scale with decimal labels
  # scale_y_continuous(trans = log10_trans(),
  #                    breaks = trans_breaks("log10", function(x) 10^x),
  #                    labels = trans_format("log10", math_format(10^.x))) +  # Adjust upper limit
  labs(x = "Sample size", 
       y = expression("eRMSE"),  # Updated y-axis label
       colour = NULL, linetype = NULL) +
  ggtitle("Mean impact speed reduction")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 3, ncol = 1)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c(1,2,3,4))+  
  scale_fill_manual(values = cb_palette) +  # Set custom CI area colors
  theme(legend.position = 'bottom')


# Plot
g2 <- ggplot() +
  geom_line(data = prediction_models %>% filter (target %in% "crash avoidance"), aes(x =  iter* batch_size, y = crash_avoidance_RMSE,
                                                                                     colour = prediction_type, linetype = prediction_type), size = 1) +
  geom_ribbon(data = prediction_models %>% filter (target %in% "crash avoidance"), aes(x =  iter* batch_size, 
                                                                                       ymin = crash_avoidance_lower_ci, 
                                                                                       ymax = crash_avoidance_upper_ci, 
                                                                                       fill = prediction_type), alpha = 0.4,show.legend = FALSE) +  # Adjust alpha for transparency
  xlim(0, 2000) +
  scale_y_continuous(trans = 'log10', labels = scales::label_number()) +  # Log scale with decimal labels
  # scale_y_continuous(trans = log10_trans(),
  #                    breaks = trans_breaks("log10", function(x) 10^x),
  #                    labels = trans_format("log10", math_format(10^.x))) +  # Adjust upper limit
  labs(x = "Sample size", y = "",
       colour = NULL, linetype = NULL) +
  ggtitle("Crash avoidance rate")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 3, ncol = 1)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c(1,2,3,4))+  
  scale_fill_manual(values = cb_palette) +  # Set custom CI area colors
  theme(legend.position = 'bottom')
c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

ggsave(sprintf(paste("Application/Figures/","Figure S7.png",sep = "")), c1, dpi = 1000, width =160, height =75, unit = "mm")

