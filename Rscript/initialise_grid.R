initialise_grid <- function(data, grid) {
  
  # Labelled dataset.
  labelled <- data %>% 
    right_join(grid, by = c("eoff", "acc")) %>%
    mutate(iter = 0,
           batch_size = 0,
           batch_weight = 1,
           pi = 0,
           mu = 0,
           nhits = 0,
           sampling_weight = 0,
           final_weight = 0)
  
  # Unlabelled dataset.
  n <- nrow(data)
  unlabelled <- data %>% 
    mutate(collision_prob0_pred = NA_real_, 
           collision_prob1_pred = NA_real_, 
           impact_speed_reduction_pred = NA_real_,
           sigma_impact_speed_reduction = NA_real_,
           sigma_injury_risk_reduction = NA_real_,
           sigma_collision1 = NA_real_,
           iter = NA) 
  
  return(list(labelled = labelled, unlabelled = unlabelled))
}
