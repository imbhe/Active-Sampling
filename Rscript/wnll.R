# Weighted negative log-likelihood of two-parameter distribution with density dens.
wnll <- function(par, y, w, p, dens) {
  - sum(w * p * dens(y, par[1], par[2], log = TRUE))
}