library("ggplot2")
library("ggpubr")
load("Data/estimation_coverage.R")
averlist = list()
simlist = list()
for (i in 1:length(res_total[[1]])) {
  aver <- res_total[[1]][[i]]$aver
  sim <- res_total[[1]][[i]]$res
  averlist[[i]] <- aver
  simlist[[i]] <- sim
}
aver_sum = do.call(rbind, averlist)
sim_combine = do.call(rbind, simlist)
active_sampling_aver= aver_sum
active_sampling_aver$neff0_44 = active_sampling_aver$total_sample_size - 44
active_sampling_aver$mean_impact_speed_reduction_ci_cover_boot1 = NA
active_sampling_aver$crash_avoidance_rate_ci_cover_boot1 = NA
niter = max(unique(sim_combine$iter))
for(p in 1:niter){
  t = sim_combine[sim_combine$iter ==p & sim_combine$target == "impact speed reduction",]$mean_impact_speed_reduction_ci_cover_boot
  active_sampling_aver[active_sampling_aver$iter ==p & active_sampling_aver$group == "impact speed reduction",]$mean_impact_speed_reduction_ci_cover_boot1= sum(t[!is.na(t)])/length(t[!is.na(t)])
  
  t = sim_combine[sim_combine$iter == p & sim_combine$target == "impact speed reduction",]$crash_avoidance_rate_ci_cover_boot
  active_sampling_aver[active_sampling_aver$iter ==p & active_sampling_aver$group == "impact speed reduction",]$crash_avoidance_rate_ci_cover_boot1 = sum(t[!is.na(t)])/length(t[!is.na(t)])
  
  t = sim_combine[sim_combine$iter ==p & sim_combine$target == "crash avoidance",]$mean_impact_speed_reduction_ci_cover_boot
  active_sampling_aver[active_sampling_aver$iter ==p & active_sampling_aver$group == "crash avoidance",]$mean_impact_speed_reduction_ci_cover_boot1= sum(t[!is.na(t)])/length(t[!is.na(t)])
  
  t = sim_combine[sim_combine$iter == p & sim_combine$target == "crash avoidance",]$crash_avoidance_rate_ci_cover_boot
  active_sampling_aver[active_sampling_aver$iter ==p & active_sampling_aver$group == "crash avoidance",]$crash_avoidance_rate_ci_cover_boot1 = sum(t[!is.na(t)])/length(t[!is.na(t)])
  
}
impact_speed_reduction_cover = c(active_sampling_aver$mean_impact_speed_reduction_ci_cover_boot1,
                                 active_sampling_aver$mean_impact_speed_reduction_ci_cover_classic,
                                 active_sampling_aver$mean_impact_speed_reduction_ci_cover_mart)
crash_avoidance_cover = c(active_sampling_aver$crash_avoidance_rate_ci_cover_boot1,
                          active_sampling_aver$crash_avoidance_rate_ci_cover_classic,
                          active_sampling_aver$crash_avoidance_rate_ci_cover_mart)
len = length(active_sampling_aver$group)
estimation_methods = c(rep("Bootstrap method",len),
                       rep("Classical method (Sen-Yates-Grundy estimator)",len),
                       rep("Martingale method",len))
combine_three_estimation = rbind(active_sampling_aver,active_sampling_aver,active_sampling_aver)
combine_three_estimation$estimation_methods = estimation_methods
combine_three_estimation$impact_speed_reduction_cover = impact_speed_reduction_cover
combine_three_estimation$crash_avoidance_cover = crash_avoidance_cover
combine_three_estimation$estimation_methods = factor(combine_three_estimation$estimation_methods, levels =
                                                       c("Classical method (Sen-Yates-Grundy estimator)",
                                                         "Martingale method",
                                                         "Bootstrap method"))
ptsize <- 11
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", size = 0.25), 
             axis.ticks = element_line(colour = "black", size = 0.25), 
             axis.title.y = element_text(margin = margin(t = 5.0, r = 0.2, l = 0.25, unit = 'cm')),
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

g1 <- ggplot(combine_three_estimation[!is.na(combine_three_estimation$impact_speed_reduction_cover) &
                                        combine_three_estimation$group == "impact speed reduction",],
             aes(x = neff0_44,y=impact_speed_reduction_cover,
                 colour = estimation_methods,linetype = estimation_methods
             )) +
  geom_line(size = 1) +
  xlim(0, 300) +
  ylim(0.50, 1) +
  geom_hline(yintercept=0.95, linetype="dashed", color = "black") + 
  labs(x = "Sample size", y = "Coverage rate of impact speed reduction",
       colour = NULL, linetype = NULL,fill = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=3))+
  scale_color_manual("",values= c("black","black","black")) +
  scale_linetype_manual("",values = c(2,3,1)) +
  theme(legend.position=c(1, -1))
g2 <- ggplot(combine_three_estimation[!is.na(combine_three_estimation$crash_avoidance_cover)&
                                        combine_three_estimation$group == "crash avoidance",],
             aes(x = neff0_44,y=crash_avoidance_cover,
                 colour = estimation_methods,linetype = estimation_methods
             )) +
  geom_line(size = 1) +
  xlim(0, 300) +
  ylim(0.50, 1) +
  geom_hline(yintercept=0.95, linetype="dashed", color = "black") + 
  labs(x = "Sample size", y = "Coverage rate of crash avoidance",
       colour = NULL, linetype = NULL,fill = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=3))+
  scale_color_manual(values= c("black","black","black")) +
  scale_linetype_manual(values = c(2,3,1)) +
  theme(legend.position=c(1, -1))

c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom") 
ggsave(sprintf("Output/95_coverage_plot.png"), c1, dpi = 1000, width =160, height =90, unit = "mm")