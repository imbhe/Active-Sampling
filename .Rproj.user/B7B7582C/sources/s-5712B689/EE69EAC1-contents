prepare_df_with_y <- function(loadfile, glance, dec, plotit = TRUE){
  df_with_y =  read_pickle_file(loadfile)
  drops <- c("t")
  df_with_y = df_with_y[ , !(names(df_with_y) %in% drops)]
  df_with_y = df_with_y[!duplicated(df_with_y),]
  transform(df_with_y,before = as.numeric(before), after = as.numeric(after),
            glance_prob = as.numeric(glance_prob),y = as.numeric(y))
  df_with_y$y = as.numeric(df_with_y$y)
  for ( i in unique(df_with_y$ID) ){ # Iterate over all cases.
    labelled = df_with_y[df_with_y$ID == i,]
    for ( j in unique(labelled$before) ) {
      labelled_dec = labelled[labelled$before == j,]
      labelled_dec = labelled_dec[order(labelled_dec$after),]
      dec_weight = dec$dec_weight[which(dec$dec == j)] 
      if (length(labelled_dec[labelled_dec$y == 0,]$after)>0)
      {
        min_index = max(labelled_dec[labelled_dec$y == 0,]$after)
        min_index = which(glance$glance_duration == min_index)
      }
      else
      {
        min_index = 0
      }
      if (length(labelled_dec[labelled_dec$y == labelled_dec[nrow(labelled_dec)-1,]$y,]$after)>0 &
          labelled_dec[nrow(labelled_dec),]$y != 0)
      {
        max_index = min(labelled_dec[labelled_dec$y == labelled_dec[nrow(labelled_dec),]$y,]$after)
        max_index = which(glance$glance_duration == max_index)
      }
      else{
        max_index = 0
      }
      max_v = tail(labelled_dec$y, n =1)
      if (min_index > 2){
        for (k in 2:(min_index-1)){
          if(length(which(labelled_dec$after == glance$glance_duration[k]))==0){
            df_with_y[nrow(df_with_y) + 1,] = c(j,
                                                glance$glance_duration[k],
                                                glance$glance_weight[k]*dec_weight,
                                                i,
                                                0)
          }
          
        }
      }
      if (max_index < length(glance$glance_duration) & max_index > 0) {
        for (k in (max_index +1):length(glance$glance_duration)){
          if(length(which(labelled_dec$after == glance$glance_duration[k])) == 0){
            df_with_y[nrow(df_with_y) + 1,] = c(j,
                                                glance$glance_duration[k],
                                                glance$glance_weight[k]*dec_weight,
                                                i,
                                                max_v)
          }
        }
      }
      if(min(df_with_y[df_with_y$ID == i & df_with_y$before == j & df_with_y$after == tail(glance$glance_duration,1),]$y) == 0){
        df_with_y[df_with_y$ID == i & df_with_y$before == j,]$y = 0
      }
      if(plotit){
        plot(as.numeric(df_with_y[df_with_y$ID == i,]$after),df_with_y[df_with_y$ID == i,]$before,col = 'red',
             main = c('CaseID:',i),
             xlab = 'Glance duration',
             ylab = 'Deceleration',
             xlim = c(0,6.5))
      }
    }
  }
  df_with_y = df_with_y[!duplicated(df_with_y),]
  df_with_y = df_with_y[order(df_with_y$ID,df_with_y$before,df_with_y$after),]
  for ( i in unique(df_with_y$ID) ) { 
    labelled = df_with_y[df_with_y$ID == i,]
    for ( j in unique(labelled$before) ) { 
      labelled_dec = labelled[labelled$before == j,]
      labelled_dec = labelled_dec[order(labelled_dec$after),]
      glance_sum = sum(glance$glance_weight[-1])
      df_with_y[(df_with_y$ID == i & df_with_y$before == j),]$glance_prob = as.numeric(df_with_y[df_with_y$ID == i & df_with_y$before == j,]$glance_prob)/(1-glance$glance_weight[1])
      sum(as.numeric(df_with_y[(df_with_y$ID == i & df_with_y$before == j),]$glance_prob))
    }
    sum(as.numeric(df_with_y[df_with_y$ID == i,]$glance_prob))
  }
  transform(df_with_y,before = as.numeric(before), after = as.numeric(after),
            glance_prob = as.numeric(glance_prob),y = as.numeric(y))
  df_with_y$before = as.numeric(df_with_y$before)
  df_with_y$after = as.numeric(df_with_y$after)
  df_with_y$y = as.numeric(df_with_y$y)
  df_with_y$glance_prob = as.numeric(df_with_y$glance_prob)
  return(df_with_y)
}