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
required_packages <- c("ggplot2", "ggpubr", "scales", "dplyr")

# Load required packages
load_required_packages(required_packages)

source("Application/Rscript/CI_calculation.R")

cb_palette = c("#666666","#E7298A", "#1B9E77","#E6AB02",  "#A6761D","#6BAED6","#7570B3","#D95F02" )

load("Application/Results/RMSE_check_20240405.R")

simlist = list()
for (i in 1:length(res_total[[1]])) {
  simlist[[i]] <- res_total[[1]][[i]]$res
}

total_sum1 = do.call(rbind, simlist)
# selected_total_sum <- subset(total_sum, opt_method != "naive")

load("Application/Results/leverage_sampling.R")

simlist = list()
for (i in 1:length(res_total[[1]])) {
  simlist[[i]] <- res_total[[1]][[i]]$res
}

total_sum2 = do.call(rbind, simlist)
total_sum = rbind(total_sum1, total_sum2)
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

selected_total_sum$new_group = NA
selected_total_sum[selected_total_sum$target == "impact speed reduction",]$new_group = "Active sampling, target = mean impact speed reduction"
selected_total_sum[selected_total_sum$target == "crash avoidance",]$new_group = "Active sampling, target = crash avoidance rate"
selected_total_sum[selected_total_sum$proposal_dist == "density sampling",]$new_group = "Density importance sampling"
selected_total_sum[selected_total_sum$proposal_dist == "severity sampling",]$new_group = "Severity importance sampling"
selected_total_sum[selected_total_sum$sampling_method == "simple random sampling",]$new_group = "Simple random sampling"
selected_total_sum[selected_total_sum$proposal_dist == "leverage sampling",]$new_group = "Leverage sampling"

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

# T test
column_names = c("iter",
                 "sample_size",
                 "impact_speed_reduction_p_value",
                 "isr_significance",
                 "crash_avoidance_p_value",
                 "ca_significance")
p_df <- data.frame(matrix(ncol = length(column_names), nrow = max(unique(ci_df$iter))))
colnames(p_df) <- column_names
for(iter_criteria in unique(ci_df$iter)){
  p_df$iter[iter_criteria] = iter_criteria
  isr_t = t.test(unlist(ci_df %>% filter (new_group =="Active sampling, target = mean impact speed reduction" &
                                                    iter == iter_criteria) %>%
         pull(mean_impact_speed_reduction_err_vector)),
       unlist(ci_df %>% filter (new_group =="Density importance sampling" &
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
  } else {
    p_df$isr_significance[iter_criteria] = 0.0001
  }
}
significance_star = c()
indices <- which(p_df$isr_significance == "non significance")
if(length(indices) == 0){
  significance_star[1] = 1
}else {
significance_star[1] = p_df$iter[max(indices)+1]}

batch_size = unique(ci_df$batch_size)

# load other two sampling Latin hypercube sampling and Gaussian process active learning
load("Application/Results/result_500repetitions_eRMSE_LHS.RData")
lhs <- res
lhs$misr_eRmse <- sqrt(lhs$mse_misr)
lhs$misr_CI_lower <- sqrt(lhs$mse_misr - qnorm(0.975) * lhs$sdmse_misr/sqrt(lhs$nreps))
lhs$misr_CI_upper <- sqrt(lhs$mse_misr + qnorm(0.975) * lhs$sdmse_misr/sqrt(lhs$nreps))

lhs$car_eRmse <- sqrt(lhs$mse_car)
lhs$car_CI_lower <- sqrt(lhs$mse_car - qnorm(0.975) * lhs$sdmse_car/sqrt(lhs$nreps))
lhs$car_CI_upper<- sqrt(lhs$mse_car + qnorm(0.975) * lhs$sdmse_car/sqrt(lhs$nreps))

lhs$res_group <- "Latin hypercube sampling"

load("Application/Results/result_500repetitions_eRMSE_GPR.RData")
gpr <- res
gpr$misr_eRmse <- sqrt(gpr$mse_misr)
gpr$misr_CI_lower <- sqrt(gpr$mse_misr - qnorm(0.975) * gpr$sdmse_misr/sqrt(gpr$nreps))
gpr$misr_CI_upper <- sqrt(gpr$mse_misr + qnorm(0.975) * gpr$sdmse_misr/sqrt(gpr$nreps))

gpr$car_eRmse <- sqrt(gpr$mse_car)
gpr$car_CI_lower <- sqrt(gpr$mse_car - qnorm(0.975) * gpr$sdmse_car/sqrt(gpr$nreps))
gpr$car_CI_upper<- sqrt(gpr$mse_car + qnorm(0.975) * gpr$sdmse_car/sqrt(gpr$nreps))

gpr$res_group <- "Gaussian process active learning"
gpr <- gpr %>%
  select(-any_of(c("prediction_model_type","target","p25.x","p75.x","p25.y","p75.y")))

res <- rbind(lhs,gpr)

ci_df <- ci_df %>% add_row(
  iter = res$n/unique(ci_df$batch_size),
  batch_size = unique(ci_df$batch_size),
  new_group = res$res_group,
  mean_impact_speed_reduction_RMSE = res$misr_eRmse,
  mean_impact_speed_reduction_lower_ci = res$misr_CI_lower,
  mean_impact_speed_reduction_upper_ci = res$misr_CI_upper,
  
  crash_avoidance_RMSE = res$car_eRmse,
  crash_avoidance_lower_ci = res$car_CI_lower,
  crash_avoidance_upper_ci = res$car_CI_upper,
)
  groups = c("Simple random sampling",
             "Density importance sampling",
             "Severity importance sampling",
             "Latin hypercube sampling",
             "Gaussian process active learning",
             "Leverage sampling",
             "Active sampling, target = mean impact speed reduction",
             "Active sampling, target = crash avoidance rate")
  ci_df$new_group <- factor(ci_df$new_group, levels = 
                                           groups)
  indices = c()
  for(i in unique(ci_df %>% filter (new_group =="Gaussian process active learning") %>%
                  pull(iter)*10)[-46]) {
    indices[i] = ci_df %>% filter (new_group =="Active sampling, target = crash avoidance rate"&
                                     iter == round(i/10)) %>%
      pull(crash_avoidance_upper_ci) <
      ci_df %>% filter (new_group =="Gaussian process active learning"&
                          iter == (i/10)) %>%
      pull(crash_avoidance_lower_ci)
  }
  
  significance_star[2] = which(indices == TRUE)[1]
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
  annotate("point", x = significance_star[1]*batch_size, y = 10^1.2, shape = 8, color = cb_palette[7], size = 1)+
  xlim(0, 2000) +
  scale_y_continuous(trans = 'log10', labels = scales::label_number()) +  # Log scale with decimal labels
  labs(x = "Sample size", 
       y = expression("eRMSE"),  # Updated y-axis label
       colour = NULL, linetype = NULL) +
  ggtitle("Mean impact speed reduction")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 4, ncol = 2)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "34","twodash", "23")) + 
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
  annotate("point", x = significance_star[2], y = 0.3, shape = 8, color = cb_palette[8], size = 1)+
  xlim(0, 2000) +
  scale_y_continuous(trans = 'log10', labels = scales::label_number()) +  # Log scale with decimal labels
  labs(x = "Sample size", y = "",
       colour = NULL, linetype = NULL) +
  ggtitle("Crash avoidance rate")+
  guides(color = guide_legend(override.aes = list(size = 1), nrow = 4, ncol = 2)) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "34","twodash", "23")) + 
  scale_fill_manual(values = cb_palette) +  # Set custom CI area colors
  theme(legend.position = 'bottom')
c1 <- ggarrange(g1, g2 ,
                ncol = 2, nrow = 1,common.legend = TRUE,align = "hv", labels=c('A','B'),
                font.label = list(size = 10, color = "black"),legend="bottom")

ggsave(sprintf(paste("Application/Figures/","Figure 5.png",sep = "")), c1, dpi = 1000, width =160, height =75, unit = "mm")
