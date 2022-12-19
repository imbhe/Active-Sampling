library("ggplot2")
library("ggpubr")
newcolors = c("#DF76A0", "#0000FF","#B2182B","#E2891F","#009999","black")
load("Data/RMSE_analysis.R")
averlist = list()
for (i in 1:length(res_total[[1]])) {
  aver <- res_total[[1]][[i]]$aver
  averlist[[i]] <- aver
}
aver_sum = do.call(rbind, averlist)
active_sampling_compare = aver_sum[(aver_sum$group =="impact speed reduction" |  aver_sum$group =="crash avoidance"),]
compare_diff_sampling = rbind(active_sampling_compare[active_sampling_compare$opt_method== "+ prediction uncertainty",],
                       aver_sum[(aver_sum$group == "simple random sampling") |
                                   (aver_sum$group == "density sampling") |
                                   (aver_sum$group == "severity sampling"),])
groups = c("Simple random sampling",
           "Density importance sampling",
           "Severity importance sampling",
           "Active sampling, target = impact speed reduction",
           "Active sampling, target = crash avoidance")

compare_diff_sampling$new_group = NA
compare_diff_sampling[compare_diff_sampling$group == "impact speed reduction",]$new_group = groups[4]
compare_diff_sampling[compare_diff_sampling$group == "crash avoidance",]$new_group = groups[5]
compare_diff_sampling[compare_diff_sampling$group == "density sampling",]$new_group = groups[2]
compare_diff_sampling[compare_diff_sampling$group == "severity sampling",]$new_group = groups[3]
compare_diff_sampling[compare_diff_sampling$group == "simple random sampling",]$new_group = groups[1]
compare_diff_sampling$new_group <- factor(compare_diff_sampling$new_group, levels = 
                                     groups)

active_sampling_compare$plot_opt_method = NA
groups1 = c("Naive",
            "Minimizing anticipated variance"
)
active_sampling_compare[active_sampling_compare$opt_method == "naive" ,]$plot_opt_method = groups1[1]
active_sampling_compare[active_sampling_compare$opt_method == "+ prediction uncertainty",]$plot_opt_method = groups1[2]
active_sampling_compare$plot_opt_method <- factor(active_sampling_compare$plot_opt_method, levels = groups1)

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

g1 <- ggplot(compare_diff_sampling,
             aes(x = neff0,y=mean_impact_speed_reduction_sqerr,
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

g2 <- ggplot(compare_diff_sampling,
             aes(x = neff0,y = crash_avoidance_rate_sqerr,
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

ggsave(sprintf("Output/active_sampling_vs_importance_sampling.png"), c1, dpi = 1000, width =160, height =90, unit = "mm")

g1 <- ggplot(active_sampling_compare[active_sampling_compare$group == "impact speed reduction",],
             aes(x = neff0,y=mean_impact_speed_reduction_sqerr,
                 colour = plot_opt_method,linetype = plot_opt_method
                 
             )) +
  geom_line(size = 1) +
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Sample size", y = "RMSE of impact speed reduction",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=2))+
  scale_color_manual("",values= c(newcolors[6],newcolors[6],newcolors[6])) +
  scale_linetype_manual("",values = c(1,2,3)) +
  theme(legend.position='bottom')

g2 <- ggplot(active_sampling_compare[active_sampling_compare$group == "crash avoidance",],
             aes(x = neff0,y = crash_avoidance_rate_sqerr,
                 colour = plot_opt_method,linetype = plot_opt_method
             )) +
  geom_line(size = 1) +
  xlim(0, 2000) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  
  labs(x = "Sample size", y = "RMSE of crash avoidance rate",
       colour = NULL, linetype = NULL)+
  guides(color = guide_legend(override.aes = list(size = 1),nrow=2))+
  scale_color_manual("",values= c(newcolors[6],newcolors[6],newcolors[6])) +
  scale_linetype_manual("",values = c(1,2,3)) +
  theme(legend.position = c(1.6, 0.7))

c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE, align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

ggsave(sprintf("Output/diff_active_sampling.png"), c1, dpi = 1000, width =160, height =80, unit = "mm")