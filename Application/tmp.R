X <- X[1:ninit,]
y <- y[1:ninit]
d <- darg(list(mle=TRUE, max=0.25), X)
gpi <- newGP(X, y, d=0.1, g=0.1*var(y), dK=TRUE)
mle <- jmleGP(gpi, c(d$min, d$max), c(g$min, g$max), d$ab, g$ab)
rmse.fish <- sqrt(mean((yytrue - predGP(gpi, XX, lite=TRUE)$mean)^2))
prog.fish <- c()
for(i in nrow(X):99) {
  solns <- xnp1.search(X, gpi, obj=obj.fish)
  m <- which.max(solns$val)
  prog.fish <- c(prog.fish, solns$val[m])
  xnew <- as.matrix(solns[m, 3:4])
  X <- rbind(X, xnew)
  y <- c(y, f(xnew))
  updateGP(gpi, xnew, y[length(y)])
  mle <- rbind(mle, jmleGP(gpi, c(d$min, d$max), c(g$min, g$max), 
                           d$ab, g$ab))
  p <-predGP(gpi, XX, lite=TRUE)
  rmse.fish <- c(rmse.fish, sqrt(mean((yytrue - p$mean)^2)))
}