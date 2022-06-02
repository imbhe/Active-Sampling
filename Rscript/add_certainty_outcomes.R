# Add data-points with certain outcomes (no crash or max impact speed crash) 
# to labelled set, remove from unlabelled set.
add_certainty_outcomes <- function(new_sample, labelled, unlabelled) {
  
  for ( i in unique(new_sample$ID) ) { # Iterate over all cases.
    
    # Min and max impact speed for current case.
    labelled_i <- labelled %>% 
      filter(ID == i)
    ymin <- min(labelled_i$y)
    ymax <- max(labelled_i$y)
    
    # New data for current case.
    labelled_ij <- new_sample %>% 
      filter(ID == i)
    
    
    for ( j in 1:nrow(labelled_ij) ) { # Iterative over all labelled data points.
      
      newx <- labelled_ij[j, ]
      
      if ( newx$y == ymin ) { # Add certainty non-crashes.
        ix <- with(unlabelled, which(ID == i & before <= newx$before & after <= newx$after))
        
        add <- unlabelled %>% 
          filter(row_number() %in% ix) %>%
          mutate(y = ymin) %>% 
          dplyr::select(-yhat, -prob_positive)
        
        labelled <- labelled %>%
          add_row(add)
        
        if ( length(ix) > 0) {
          unlabelled <- unlabelled[-ix, ]
        }
        
      } else if ( newx$y == ymax ) { # Add certainty max impact speed crashes.
        
        ix <- with(unlabelled, which(ID == i & before >= newx$before & after >= newx$after))
        
        add <- unlabelled %>% 
          filter(row_number() %in% ix) %>%
          mutate(y = ymax) %>% 
          dplyr::select(-yhat, -prob_positive)
        
        labelled <- labelled %>%
          add_row(add)
        
        if ( length(ix) > 0) {
          unlabelled <- unlabelled[-ix, ]
        }
      }
    }
  }
  
  return(list(labelled = labelled, unlabelled = unlabelled))
  
}