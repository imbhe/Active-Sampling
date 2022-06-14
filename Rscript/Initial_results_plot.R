library("tidyverse")
library("ggplot2")
load("Data/res_analysis.R")
res1 = res_out[[1]]
res2 = res_out[[2]]
res3 = res_out[[3]]
res4 = res_out[[4]]
res5 = res_out[[5]]
res6 = res_out[[6]]
fcount = 1

g4 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = mean_impact_speed0_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = res1$biggest$mean_impact_speed0_sqerr), color = "firebrick", ) +
  geom_line(aes(y = res1$smallest$mean_impact_speed0_sqerr), color = "firebrick", ) +
  geom_ribbon(aes(ymax=res1$biggest$mean_impact_speed0_sqerr, ymin=res1$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res2$aver$mean_impact_speed0_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$biggest$mean_impact_speed0_sqerr), color = "blue", ) +
  geom_line(aes(y = res2$smallest$mean_impact_speed0_sqerr), color = "blue", ) +
  geom_ribbon(aes(ymax=res2$biggest$mean_impact_speed0_sqerr, ymin=res2$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res3$aver$mean_impact_speed0_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$biggest$mean_impact_speed0_sqerr), color = "green", ) +
  geom_line(aes(y = res3$smallest$mean_impact_speed0_sqerr), color = "green", ) +
  geom_ribbon(aes(ymax=res3$biggest$mean_impact_speed0_sqerr, ymin=res3$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res4$aver$mean_impact_speed0_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$biggest$mean_impact_speed0_sqerr), color = "black", ) +
  geom_line(aes(y = res4$smallest$mean_impact_speed0_sqerr), color = "black", ) +
  geom_ribbon(aes(ymax=res4$biggest$mean_impact_speed0_sqerr, ymin=res4$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res5$aver$mean_impact_speed0_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$biggest$mean_impact_speed0_sqerr), color = "yellow", ) +
  geom_line(aes(y = res5$smallest$mean_impact_speed0_sqerr), color = "yellow", ) +
  geom_ribbon(aes(ymax=res5$biggest$mean_impact_speed0_sqerr, ymin=res5$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res6$aver$mean_impact_speed0_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$biggest$mean_impact_speed0_sqerr), color = "orange", ) +
  geom_line(aes(y = res6$smallest$mean_impact_speed0_sqerr), color = "orange", ) +
  geom_ribbon(aes(ymax=res6$biggest$mean_impact_speed0_sqerr, ymin=res6$smallest$mean_impact_speed0_sqerr), fill="grey", alpha=0.25) +
  
  labs(x = "Iteration", y = "Baseline mean impact speed sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed sd for different sampling methods")

g5 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = absolute_impact_speed_reduction_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = res1$biggest$absolute_impact_speed_reduction_sqerr), color = "firebrick", ) +
  geom_line(aes(y = res1$smallest$absolute_impact_speed_reduction_sqerr), color = "firebrick", ) +
  geom_ribbon(aes(ymax=res1$biggest$absolute_impact_speed_reduction_sqerr, ymin=res1$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res2$aver$absolute_impact_speed_reduction_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$biggest$absolute_impact_speed_reduction_sqerr), color = "blue", ) +
  geom_line(aes(y = res2$smallest$absolute_impact_speed_reduction_sqerr), color = "blue", ) +
  geom_ribbon(aes(ymax=res2$biggest$absolute_impact_speed_reduction_sqerr, ymin=res2$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res3$aver$absolute_impact_speed_reduction_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$biggest$absolute_impact_speed_reduction_sqerr), color = "green", ) +
  geom_line(aes(y = res3$smallest$absolute_impact_speed_reduction_sqerr), color = "green", ) +
  geom_ribbon(aes(ymax=res3$biggest$absolute_impact_speed_reduction_sqerr, ymin=res3$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res4$aver$absolute_impact_speed_reduction_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$biggest$absolute_impact_speed_reduction_sqerr), color = "black", ) +
  geom_line(aes(y = res4$smallest$absolute_impact_speed_reduction_sqerr), color = "black", ) +
  geom_ribbon(aes(ymax=res4$biggest$absolute_impact_speed_reduction_sqerr, ymin=res4$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res5$aver$absolute_impact_speed_reduction_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$biggest$absolute_impact_speed_reduction_sqerr), color = "yellow", ) +
  geom_line(aes(y = res5$smallest$absolute_impact_speed_reduction_sqerr), color = "yellow", ) +
  geom_ribbon(aes(ymax=res5$biggest$absolute_impact_speed_reduction_sqerr, ymin=res5$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  geom_point(aes(y = res6$aver$absolute_impact_speed_reduction_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$biggest$absolute_impact_speed_reduction_sqerr), color = "orange", ) +
  geom_line(aes(y = res6$smallest$absolute_impact_speed_reduction_sqerr), color = "orange", ) +
  geom_ribbon(aes(ymax=res6$biggest$absolute_impact_speed_reduction_sqerr, ymin=res6$smallest$absolute_impact_speed_reduction_sqerr), fill="grey", alpha=0.25) +
  
  labs(x = "Iteration", y = "Impact speed reduction sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed reduction sd for different sampling methods")

g8 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = mean_impact_speed0_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = mean_impact_speed0_sqerr),color = "firebrick", size = 0.5)+
  
  geom_point(aes(y = res2$aver$mean_impact_speed0_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$aver$mean_impact_speed0_sqerr),color = "blue", size = 0.5)+
  
  geom_point(aes(y = res3$aver$mean_impact_speed0_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$aver$mean_impact_speed0_sqerr),color = "green", size = 0.5)+
  
  geom_point(aes(y = res4$aver$mean_impact_speed0_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$aver$mean_impact_speed0_sqerr),color = "black",  size = 0.5)+
  
  geom_point(aes(y = res5$aver$mean_impact_speed0_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$aver$mean_impact_speed0_sqerr),color = "yellow", size = 0.5)+
  
  geom_point(aes(y = res6$aver$mean_impact_speed0_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$aver$mean_impact_speed0_sqerr),color = "orange",  size = 0.5)+
  
  labs(x = "Iteration", y = "Baseline mean impact speed sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed sd for different sampling methods")

g9 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = absolute_impact_speed_reduction_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = absolute_impact_speed_reduction_sqerr),color = "firebrick", size = 0.5)+
  
  geom_point(aes(y = res2$aver$absolute_impact_speed_reduction_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$aver$absolute_impact_speed_reduction_sqerr),color = "blue", size = 0.5)+
  
  geom_point(aes(y = res3$aver$absolute_impact_speed_reduction_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$aver$absolute_impact_speed_reduction_sqerr),color = "green", size = 0.5)+
  
  geom_point(aes(y = res4$aver$absolute_impact_speed_reduction_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$aver$absolute_impact_speed_reduction_sqerr),color = "black",  size = 0.5)+
  
  geom_point(aes(y = res5$aver$absolute_impact_speed_reduction_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$aver$absolute_impact_speed_reduction_sqerr),color = "yellow", size = 0.5)+
  
  geom_point(aes(y = res6$aver$absolute_impact_speed_reduction_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$aver$absolute_impact_speed_reduction_sqerr),color = "orange",  size = 0.5)+
  
  labs(x = "Iteration", y = "Impact speed reduction sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Impact speed reduction sd for different sampling methods")

g10 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = proportion_crashes_avoided_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = proportion_crashes_avoided_sqerr),color = "firebrick", size = 0.5)+
  
  geom_point(aes(y = res2$aver$proportion_crashes_avoided_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$aver$proportion_crashes_avoided_sqerr),color = "blue", size = 0.5)+
  
  geom_point(aes(y = res3$aver$proportion_crashes_avoided_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$aver$proportion_crashes_avoided_sqerr),color = "green", size = 0.5)+
  
  geom_point(aes(y = res4$aver$proportion_crashes_avoided_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$aver$proportion_crashes_avoided_sqerr),color = "black",  size = 0.5)+
  
  geom_point(aes(y = res5$aver$proportion_crashes_avoided_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$aver$proportion_crashes_avoided_sqerr),color = "yellow", size = 0.5)+
  
  geom_point(aes(y = res6$aver$proportion_crashes_avoided_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$aver$proportion_crashes_avoided_sqerr),color = "orange",  size = 0.5)+
  
  labs(x = "Iteration", y = "Proportion of avoided crashes (by AEB) sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Proportion of avoided crashes (by AEB) sd for different sampling methods")

g11 <- ggplot(res1$aver, aes(x = iter)) +
  geom_point(aes(y = absolute_injury_risk_reduction_sqerr),color = "firebrick", shape = "diamond", size = 2)+
  geom_line(aes(y = absolute_injury_risk_reduction_sqerr),color = "firebrick", size = 0.5)+
  
  geom_point(aes(y = res2$aver$absolute_injury_risk_reduction_sqerr),color = "blue", shape = "diamond", size = 2)+
  geom_line(aes(y = res2$aver$absolute_injury_risk_reduction_sqerr),color = "blue", size = 0.5)+
  
  geom_point(aes(y = res3$aver$absolute_injury_risk_reduction_sqerr),color = "green", shape = "diamond", size = 2)+
  geom_line(aes(y = res3$aver$absolute_injury_risk_reduction_sqerr),color = "green", size = 0.5)+
  
  geom_point(aes(y = res4$aver$absolute_injury_risk_reduction_sqerr),color = "black", shape = "diamond", size = 2)+
  geom_line(aes(y = res4$aver$absolute_injury_risk_reduction_sqerr),color = "black",  size = 0.5)+
  
  geom_point(aes(y = res5$aver$absolute_injury_risk_reduction_sqerr),color = "yellow", shape = "diamond", size = 2)+
  geom_line(aes(y = res5$aver$absolute_injury_risk_reduction_sqerr),color = "yellow", size = 0.5)+
  
  geom_point(aes(y = res6$aver$absolute_injury_risk_reduction_sqerr),color = "orange", shape = "diamond", size = 2)+
  geom_line(aes(y = res6$aver$absolute_injury_risk_reduction_sqerr),color = "orange",  size = 0.5)+
  
  labs(x = "Iteration", y = "Injury risk reduction (by AEB) sd")+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Injury risk reduction (by AEB) sd for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g5, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g8, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g9, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g10, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g11, dpi = 1000, width = 270, height = 270, unit = "mm")

