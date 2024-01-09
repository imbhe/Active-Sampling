library("ggplot2")
library("ggpubr")
library("scales")
library("dplyr")
source("Rscript/CI_calculation.R")
newcolors = c("#DF76A0", "#0000FF","#B2182B","#E2891F","#009999","black")
load("Data/RMSE_analysis.R")

averlist = list()
simlist = list()
for (i in 1:length(res_total[[1]])) {
  aver <- res_total[[1]][[i]]$aver
  averlist[[i]] <- aver
  simlist[[i]] <- res_total[[1]][[i]]$res
}

aver_sum = do.call(rbind, averlist)
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
           "Active sampling, target = impact speed reduction",
           "Active sampling, target = crash avoidance")

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
    # print(k)
    
  }
}

ptsize <- 11
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.0, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1, "cm"),
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
             plot.title = element_text(size = ptsize, colour = "black", family = "serif", face = "plain", hjust = 0),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

g1 <- ggplot(ci_df,
             aes(x = iter,y= mean_impact_speed_reduction_RMSE,
                 colour = new_group,linetype = new_group
                 
             )) +
  geom_line(size = 1) +
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Sample size", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=5))+
  scale_color_manual("",values= c(newcolors)) +
  scale_linetype_manual("",values = c(1,2,3,4,5)) +
  theme(legend.position='bottom')

g2 <- ggplot(ci_df,
             aes(x = iter,y = crash_avoidance_RMSE,
                 colour = new_group,linetype = new_group
             )) +
  geom_line(size = 1) +
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Sample size", y = "RMSE of crash avoidance rate",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=5))+
  scale_color_manual("",values= c(newcolors)) +
  scale_linetype_manual("",values = c(1,2,3,4,5)) +
  theme(legend.position = "bottom")

c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

# ggsave(sprintf("Output/recaluclated_active_sampling_vs_importance_sampling.png"), c1, dpi = 1000, width =160, height =90, unit = "mm")

#####New plots for ci area
# Filter the dataframe for the two groups
filtered_impact_speed_reduction <- ci_df %>% filter(new_group %in% c("Active sampling, target = impact speed reduction", 
                                                                     "Density importance sampling"))
# Plot
g3 <- ggplot() +
  geom_line(data = ci_df, aes(x = iter, y = mean_impact_speed_reduction_RMSE,
                              colour = new_group, linetype = new_group), size = 1) +
  geom_ribbon(data = filtered_impact_speed_reduction, aes(x = iter, 
                                      ymin = mean_impact_speed_reduction_lower_ci, 
                                      ymax = mean_impact_speed_reduction_upper_ci, 
                                      fill = new_group), alpha = 0.3,show.legend = FALSE) +  # Adjust alpha for transparency
  xlim(0, 2000/10) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Iteration", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL) +
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 5)) +
  scale_color_manual(values = c(newcolors)) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_fill_manual(values = c("#0000FF","#E2891F")) +  # Set custom CI area colors
  theme(legend.position = 'bottom')


# Filter the dataframe for the two groups
filtered_crash_avoidance <- ci_df %>% filter(new_group %in% c("Active sampling, target = crash avoidance", 
                                                                     "Severity importance sampling"))
# Plot
g4 <- ggplot() +
  geom_line(data = ci_df, aes(x = iter, y = crash_avoidance_RMSE,
                              colour = new_group, linetype = new_group), size = 1) +
  geom_ribbon(data = filtered_crash_avoidance, aes(x = iter, 
                                                          ymin = crash_avoidance_lower_ci, 
                                                          ymax = crash_avoidance_upper_ci, 
                                                          fill = new_group), alpha = 0.3,show.legend = FALSE) +  # Adjust alpha for transparency
  xlim(0, 2000/10) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Iteration", y = "RMSE of crash avoidance",
       colour = NULL, linetype = NULL) +
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 5)) +
  scale_color_manual(values = c(newcolors)) +
  scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +
  scale_fill_manual(values = c("#B2182B","#009999")) +  # Set custom CI area colors
  theme(legend.position = 'bottom')
c2 <- ggarrange(g3, g4 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

ggsave(sprintf("Output/CI_active_sampling_vs_importance_sampling.png"), c2, dpi = 1000, width =160, height =90, unit = "mm")

# T test
column_names = c("iter",
                 "sample_size",
                 "impact_speed_reduction_p_value",
                 "isr_significance",
                 "crash_avoidance_p_value",
                 "ca_significance")
p_df <- data.frame(matrix(ncol = length(column_names), nrow = max(unique(filtered_impact_speed_reduction$iter))))
colnames(p_df) <- column_names
for(iter_criteria in unique(filtered_impact_speed_reduction$iter)){
  p_df$iter[iter_criteria] = iter_criteria
  # p_df$sample_size[iter_criteria] = filtered_impact_speed_reduction %>% filter (iter == iter_criteria &
  #                                                                                 new_group =="Active sampling, target = impact speed reduction") %>%
  #   select(total_sample_size)
  isr_t = t.test(unlist(filtered_impact_speed_reduction %>% filter (new_group =="Active sampling, target = impact speed reduction" &
                                                    iter == iter_criteria) %>%
         pull(mean_impact_speed_reduction_err_vector)),
       unlist(filtered_impact_speed_reduction %>% filter (new_group =="Density importance sampling" &
                                                     iter == iter_criteria) %>%
         pull(mean_impact_speed_reduction_err_vector)),
       alternative = "two.sided", var.equal = FALSE)
  p_df$impact_speed_reduction_p_value[iter_criteria] = isr_t$p.value
  if (isr_t$p.value >0.05){
    p_df$isr_significance[iter_criteria] = "non significance"
  } else if (isr_t$p.value >0.01) {
    p_df$isr_significance[iter_criteria] = 0.05
  } else if (isr_t$p.value >0.001) {
    p_df$isr_significance[iter_criteria] = 0.01
  } else if (isr_t$p.value >0.0001) {
    p_df$isr_significance[iter_criteria] = 0.001
  } 
  # crash avoidance
  ca_t = t.test(unlist(filtered_crash_avoidance %>% filter (new_group =="Active sampling, target = crash avoidance" &
                                                              iter == iter_criteria) %>%
                         pull(crash_avoidance_err_vector)),
                unlist(filtered_crash_avoidance %>% filter (new_group =="Severity importance sampling" &
                                                              iter == iter_criteria) %>%
                         pull(crash_avoidance_err_vector)),
                alternative = "two.sided", var.equal = FALSE)
  p_df$crash_avoidance_p_value[iter_criteria] = ca_t$p.value
  if (ca_t$p.value >0.05){
    p_df$ca_significance[iter_criteria] = "non significance"
  } else if (ca_t$p.value >0.01) {
    p_df$ca_significance[iter_criteria] = 0.05
  } else if (ca_t$p.value >0.001) {
    p_df$ca_significance[iter_criteria] = 0.01
  } else if (ca_t$p.value >0.0001) {
    p_df$ca_significance[iter_criteria] = 0.001
  } else {
    p_df$ca_significance[iter_criteria] = 0.0001
  }
  
}
