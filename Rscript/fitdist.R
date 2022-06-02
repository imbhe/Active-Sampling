# Estimate impact speed distribution.
fitdist <- function(data) {
  
  crashes <- data %>% filter(y > 0)
  
  # Log-normal distribution.
  lnfit <- with(crashes, optim(c(0, 1), function(par) wnll(par, dens = dlnorm, y = y, w = w, p = glance_prob)))
  
  # Gamma distribution.
  mu <- with(crashes, sum(glance_prob * y) / sum(glance_prob))
  sigma2 <- with(crashes, sum(glance_prob * (y - mu)^2) / sum(glance_prob))
  theta0 <- c(shape = mu^2 / sigma2, rate = mu / sigma2) # Initial value.
  gamfit <- with(crashes, optim(theta0, function(par) wnll(par, dens = dgamma, y = y, w = w, p = glance_prob)))
  
  # Choose best fitting model.
  if ( gamfit$value < lnfit$value ) {
    fit <- gamfit
    fit$dist <- "gamma"
  } else {
    fit <- lnfit
    fit$dist <- "lognormal"
  }
  return(fit)
}
