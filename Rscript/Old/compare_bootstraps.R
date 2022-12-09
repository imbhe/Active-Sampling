
library(boot)

rm(list = ls())

N <- 1e2
prob <- c(1, 20)
X <- t(rmultinom(1e3, N, prob / sum(prob)))
print(sqrt(var(X[, 2] / N)))

# Ordinary.
# Should not be used.
df <- data.frame(x = X[1, ], y = c(0, 1))
boot(df, function(df, ix) sum(df$x[ix] * df$y[ix]) / sum(df$x[ix]), 1e4)
boot(df, function(df, w) sum(df$x * df$y * w) / sum(df$x * w), 1e4, stype = "w")
boot(df, function(df, w) sum(df$x * df$y * w) / sum(df$x * w), 1e4, weights = df$x, stype = "w")
boot(df, function(df, f) sum(df$x * df$y * f) / sum(df$x * f), 1e4, weights = df$x, stype = "f")

# Importance weighted, with sample size = total number of selections.
# Correct.
my_boot <- function(df, B) {
  n <- sum(df$x)
  prob <- df$x / n
  mu <- n * prob
  n_hits <- rmultinom(B, n, prob)
  
  sd((df$x * df$y) %*% (n_hits / mu) / (df$x %*% (n_hits / mu)))
}
print(my_boot(df, 1e4))

# Creating extended dataset, one row per selection.
# Correct.
ix <- rep(1:nrow(df), df$x) # To repeat rows.
reps <- which(c(1, diff(ix)) == 0) # Find duplicate rows, set corresponding simulation counts to 0.
df2 <- df[ix, ] 
df2$x <- 1
print(boot(df2, function(df, f) sum(df$x * df$y * f) / sum(df$x * f), 1e4, stype = "f"))
