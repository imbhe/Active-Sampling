# Calculate sampling scheme.
calculate_sampling_scheme <- function(data, crit, fit) {
  
  if ( crit == "uniform" ) {
    
    pi <- rep(1, nrow(data))
    
  } else if ( crit == "prob" ) {
    
    pi <- with(data, glance_prob * sqrt(prob_positive))
    
  } else if ( crit == "mean" ) {
    
    pi <- with(data, glance_prob * sqrt(prob_positive) * yhat)
    
  } else if ( crit == "moments" ) {
    
    size <- with(data, yhat + yhat^2 + yhat^3 + yhat^4)
    pi <- with(data, glance_prob * sqrt(prob_positive) * size)
    
  } else if ( crit == "std_moments" ) {
    
    size <- with(data, yhat / sd(yhat) + yhat^2 / sd(yhat^2) + yhat^3 / sd(yhat^3) + yhat^4 / sd(yhat^4))
    pi <- with(data, glance_prob * sqrt(prob_positive) * size)    
  } else if ( crit == "nll" ) {
    
    print("Not implemented yet!")
    
  } else if ( crit == "KL" ) {
    
    print("Not implemented yet!")
    
  }
  
  return(pi / sum(pi))
  
}