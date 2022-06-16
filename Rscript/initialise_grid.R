initialise_grid <- function(data, grid) {
  
  # Labelled dataset.
  labelled <- data %>% 
    left_join(grid, by = c("eoff", "acc")) %>%
    filter(sim_count0 == 1) %>% 
    mutate(sim_count1 = as.numeric((impact_speed0 != 0)), 
           sampling_weight = 0, 
           final_weight = eoff_acc_prob * sampling_weight)
  
  # Unlabelled dataset.
  n <- nrow(data)
  zeroes <- rep(0, n)
  ones <- rep(1, n)
  unlabelled <- data %>% 
    left_join(grid, by = c("eoff", "acc")) %>%
    mutate(collision_prob0_pred = NA_real_, 
           collision_prob1_pred = NA_real_, 
           impact_speed0_pred = NA_real_,
           impact_speed1_pred = NA_real_,
           injury_risk0_pred = NA_real_,
           injury_risk1_pred = NA_real_,
           crash0 = NA_integer_,
           crash1 = NA_integer_,
           non_crash0 = NA_integer_,
           non_crash1 = NA_integer_,
           max_impact0 = NA_integer_,
           max_impact1 = NA_integer_,
           sim_count0 = ifelse(is.na(sim_count0), ones, 1 - sim_count0),
           sim_count1 = ifelse(is.na(sim_count1), ones, 1 - sim_count1),
           sim_count1 = ifelse(impact_speed0 <= 0 & reduce_simulations_by_logic, 0, sim_count1)) %>%
    dplyr::select( -impact_speed0, -impact_speed1, -injury_risk0, -injury_risk1)
  
  return(list(labelled = labelled, unlabelled = unlabelled))
}