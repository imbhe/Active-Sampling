library("tidyverse")
library("ggplot2")
load("Data/res_analysis_data.R")
fcount <- 1

averlist = list()
for (i in 1:length(res_total[[1]])) {
  aver <- res_total[[1]][[i]]$aver
  averlist[[i]] <- aver
}
aver_sum1 = do.call(rbind, averlist)

averlist = list()
for (i in 1:length(res_total[[3]])) {
  aver <- res_total[[3]][[i]]$aver
  averlist[[i]] <- aver
}
aver_sum3 = do.call(rbind, averlist)
# fix the label 
g1 <- ggplot(aver_sum1, aes(x = neff0,y=mean_impact_speed0_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different sampling methods")

g2 <- ggplot(aver_sum1, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods")

g3 <- ggplot(aver_sum1, aes(x = neff0,y = proportion_crashes_avoided_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods")

g4 <- ggplot(aver_sum1, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g1, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g2, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g3, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g4, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

g5 <- ggplot(aver_sum3, aes(x = neff0,y=mean_impact_speed0_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed for different sampling methods-10 iteration")

g6 <- ggplot(aver_sum3, aes(x = neff0,y=absolute_impact_speed_reduction_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of Impact speed reduction for different sampling methods-10 iteration")

g7 <- ggplot(aver_sum3, aes(x = neff0,y = proportion_crashes_avoided_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean crash avoidance rate for different sampling methods-10 iteration")

g8 <- ggplot(aver_sum3, aes(x = neff0,y = absolute_injury_risk_reduction_sqerr)) +
  geom_point(aes(colour = group), size = 1) +
  geom_line(aes(colour = group), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("RMSE of mean injury risk reduction for different sampling methods-10 iteration")
ggsave(sprintf("Output/fig%d.png", fcount), g5, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g6, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g7, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g8, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1

groupnames= unique(aver_sum1$group)
sublist = list()
for(i in 1:length(groupnames)){
  group1 = aver_sum1[aver_sum1$group == groupnames[i],]
  group1$inter_choice = rep('one per iteration',length(group1$group))
  group3 = aver_sum3[aver_sum3$group == groupnames[i],]
  group3$inter_choice = rep('multiple per iteration',length(group3$group))
  sub_inloop = rbind(group1,group3)
  sublist[[i]] <- sub_inloop
}


g9 <- ggplot(sublist[[1]], aes(x = neff0,y=mean_impact_speed0_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Impact speed",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of Impact speed for different sampling methods")

g10 <- ggplot(sublist[[2]], aes(x = neff0,y=absolute_impact_speed_reduction_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of impact speed reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of Impact speed reduction for different sampling methods")

g11 <- ggplot(sublist[[3]], aes(x = neff0,y = proportion_crashes_avoided_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of mean crash avoidance rate",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of mean crash avoidance rate for different sampling methods")

g12 <- ggplot(sublist[[4]], aes(x = neff0,y = absolute_injury_risk_reduction_sqerr)) +
  geom_point(aes(colour = inter_choice), size = 1) +
  geom_line(aes(colour = inter_choice), size = 1) +
  labs(x = "Needed simulation number", y = "RMSE of Mean injury risk reduction",
       colour = NULL)+
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(size = 12),
        legend.position="top")+
  ggtitle("Compare RMSE of mean injury risk reduction for different sampling methods")

ggsave(sprintf("Output/fig%d.png", fcount), g9, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g10, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g11, dpi = 1000, width = 270, height = 270, unit = "mm")
fcount <- fcount + 1 
ggsave(sprintf("Output/fig%d.png", fcount), g12, dpi = 1000, width = 270, height = 270, unit = "mm")
