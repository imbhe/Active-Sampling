# Get outcome (impact speed) for case ID with given glance time before and after tauinv.
get_y <- function(ID, before, after,optimizefactor) {
  if(optimizefactor == 'Impact_speed'){
    y <- as.numeric(df_with_y$y[df_with_y$ID == ID & df_with_y$before == before & df_with_y$after == after])
  }
  else if(optimizefactor == 'Injury_reduction'){
    y <- as.numeric(df_with_y$R[df_with_y$ID == ID & df_with_y$before == before & df_with_y$after == after])
  }
  else if(optimizefactor == 'Speed_reduction'){
    y <- as.numeric(df_with_y$dy[df_with_y$ID == ID & df_with_y$before == before & df_with_y$after == after])
  }
}
get_y <- Vectorize(get_y, c("ID", "before", "after"))