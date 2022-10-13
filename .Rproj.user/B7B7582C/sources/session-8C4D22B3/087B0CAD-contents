find_max_impact_crashes <- function(new_sample, labelled, unlabelled) {
  
  for ( i in unique(new_sample$caseID) ) { # Iterate over all cases.
    
    # Get max impact speed for current case.
    max <- labelled %>% 
      filter(caseID == i) %>% 
      summarise(impact_speed0 = max(impact_speed0),
                impact_speed1 = max(impact_speed1))

   
    # New data for current case.
    labelled_i <- new_sample %>% 
      filter(caseID == i)
    
    
    for ( j in 1:nrow(labelled_i) ) { # Iterative over all labelled data points.
      
      newx <- labelled_i[j, ]
      
      if ( newx$impact_speed0 == max$impact_speed0 ) { # Find certainty non-crashes, baseline scenario.
        ix <- with(unlabelled, which(caseID == i & eoff >= newx$eoff & acc >= newx$acc))
        
        unlabelled %<>% 
          mutate(max_impact0 = ifelse(row_number() %in% ix, 1, max_impact0 ))
        
      }
      
      if ( newx$impact_speed1 == max$impact_speed1 ) { # Find certainty non-crashes, with counter measure.
        ix <- with(unlabelled, which(caseID == i & eoff >= newx$eoff & acc >= newx$acc))
        
        unlabelled %<>% 
          mutate(max_impact1 = ifelse(row_number() %in% ix, 1, max_impact1 ))
        
      }
    }
  }
  
  ix0 <- which(unlabelled$max_impact0 == 1)
  ix1 <- which(unlabelled$max_impact1 == 1)
  
  return(list(max_impact_crashes0 = ix0, max_impact_crashes1 = ix1))
  
}