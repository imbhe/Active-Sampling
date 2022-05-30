#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Program info 
#
# File name: calc_overshot_dist.R
#
# Description: calculated overshot distribution from glance times.
#
# Input: 
#   - input data frame with two columns:.
#      * glance duration.
#      * count of glances with specified duration.
#   - timevar: name of time variable.
#   - countvar: name of count variable.
#   - h: step size (time resolution). 
#
# Output: dataframe with probability mass function of overshot distribution, having columns
#   - t, overshot time
#   - prob, probability of overshot time equal to t.
# 
# Author: Henrik Imberg.
#
# Date: March 3rd 2020.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_overshot_dist <- function(data, timevar, countvar, h = NULL) {
  
  if ( is.null(h) ){ # If no step size provided: use same as (smallest) step size in input data.
    h <- min(diff(as.numeric(unlist(data[[timevar]]))))
  }

  # Total glancing time.
  t_tot <- sum(data[[timevar]] * data[[countvar]])
  
  # Survival function of overshot distribution (given overshot > 0).
  tseq <- seq(0, max(data[[timevar]]), h)
  surv <- vapply(tseq, function(x) {
    sum(data[[countvar]] * pmax(data[[timevar]] - x, 0)) / t_tot
  }, numeric(1))
  
  # Probability mass function of overshot distribution.
  pmf <- rev(diff(rev(surv)))
  pmf <- pmf / sum(pmf) # Normalise: sum up to one.
  tseq <- tseq[-1]
  
  # Create dataframe for overshot distribution.
  df <- data.frame(t = tseq, prob = pmf)

  # Return.
  return(df)
  
}

# Simplicitation, added 3 March 2022.
pmf2OverShot <- function(pmf) {

  n <- length(pmf)
  t <- sum(pmf * 1:n)
  
  # Survival function of overshot distribution (given overshot > 0).
  surv <- vapply(0:n, function(x) {
    sum(pmf * pmax(1:n - x, 0)) / t
  }, numeric(1))
  
  # Probability mass function of overshot distribution.
  OverShot <- rev(diff(rev(surv)))
  OverShot <- OverShot / sum(OverShot) # Normalise: sum up to one.

  # Return.
  return(OverShot)
  
}

# Simplification and correction, added 4 March 2022.
pmf2OverShotCorrected <- function(pmf) {
  
  prob <- rev(cumsum(rev(pmf / 1:length(pmf))))
  prob <- prob / sum(prob)

}