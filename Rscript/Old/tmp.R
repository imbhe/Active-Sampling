Y <- new_sample %>% 
  mutate(baseline_crash = impact_speed0 > 0, 
         crash_avoidance = baseline_crash * (1 - (impact_speed1 > 0)) ) %>% 
  dplyr::select(impact_speed_reduction, injury_risk_reduction, crash_avoidance, baseline_crash) %>%
  as.matrix()

w <- with(new_sample, eoff_acc_prob * sampling_weight )
t_yi[i, ] <- colSums(w * Y)

t_y <- colSums(bwt * t_yi[1:i,, drop = FALSE])
m_y <- c(t_y[1:3] / t_y[4]) # NA if no crashes, i.e., all elements in t_y = 0.


labelled_mean_impact_speed0 = sum(labelled$impact_speed0 * labelled$eoff_acc_prob) / sum(labelled$eoff_acc_prob),
labelled_mean_impact_speed1 = sum(labelled$impact_speed1 * labelled$eoff_acc_prob) / sum(labelled$eoff_acc_prob),
labelled_mean_injury_risk0 = sum(labelled$injury_risk0 * labelled$eoff_acc_prob) / sum(labelled$eoff_acc_prob),
labelled_mean_injury_risk1 = sum(labelled$injury_risk1 * labelled$eoff_acc_prob) / sum(labelled$eoff_acc_prob),
labelled_mean_crash_avoidance = sum(crashes$impact_speed1 == 0) * sum(crashes[crashes$impact_speed1 == 0, ]$eoff_acc_prob) /
  sum(crashes$impact_speed0 > 0) / sum(crashes[crashes$impact_speed0 > 0, ]$eoff_acc_prob)) %>% 
  