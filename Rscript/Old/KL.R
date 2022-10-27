# Kullback-Leibler divergence between two Gaussian distributions with mean and standard deviations
# (mu1, sigma1) and (mu2, sigma2).
KL <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 1/2 
}
