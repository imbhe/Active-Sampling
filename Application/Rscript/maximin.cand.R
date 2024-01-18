################################################################################
#
# maximin.cand.R
# Adopted from maximin::maximin.cand but changed some if clauses to avoid errors
# when class(Xorig), class(Xcand) or class(X) is "matrix" "array".
#
################################################################################

maximin.cand <- function (n, Xcand, Tmax = nrow(Xcand), Xorig = NULL, init = NULL, verb = FALSE, tempfile = NULL) {
  
  if ( !("matrix" %in% class(Xcand)) )  {
    Xcand <- as.matrix(Xcand)
  }
  if (!is.null(Xorig) && ! ("matrix" %in% class(Xorig)))  {
    Xorig <- as.matrix(Xorig)
  }
  if (n == 1) {
    stop("n must be bigger than 1.")
  }
  if (!is.null(Xorig)) 
    if (ncol(Xcand) != ncol(Xorig)) 
      stop("column dimension mismatch between Xcand and Xorig :-(")
  if (Tmax <= n) {
    warning("Tmax had better be bigger than n.")
  }
  distance <- plgp::distance
  ncand <- nrow(Xcand)
  if (!is.null(init)) {
    xi <- init
  }
  else {
    xi <- sample(1:ncand, n)
  }
  X <- Xcand[xi, ]
  xo <- setdiff(1:ncand, xi)
  D <- distance(X)
  md <- min(as.numeric(D[upper.tri(D)]))
  md.ind <- which(D == md, arr.ind = TRUE)[, 1]
  Xun <- Xcand[-xi, ]
  Du <- distance(Xun, X)
  uw.ind <- which(rowSums(Du > md) == ncol(Du))
  if (!is.null(Xorig)) {
    D2 <- distance(X, Xorig)
    md2 <- min(D2)
    if (md2 < md) {
      md <- md2
      md.ind <- which(D2 == md2, arr.ind = TRUE)[, 1]
    }
    else if (md2 == md) {
      md.ind <- unique(c(md.ind, which(D2 == md2, arr.ind = TRUE)[, 
                                                                  1]))
    }
    Du2.newcols <- distance(Xun, Xorig)
    uw.ind <- which(rowSums(Du > md) == ncol(Du) & rowSums(Du2.newcols > 
                                                             md) == ncol(Du2.newcols))
  }
  mind <- rep(NA, Tmax + 1)
  mind[1] <- md
  for (t in 1:Tmax) {
    if (length(uw.ind) == 0) {
      warning("terminated early since the maximum progress has been achieved :-)")
      return(list(inds = xi, mis = mind))
    }
    row.in.ind <- ceiling(runif(1) * length(md.ind))
    row.in <- md.ind[row.in.ind]
    xold <- matrix(X[row.in, ], nrow = 1)
    row.out.ind <- ceiling(runif(1) * length(uw.ind))
    row.out <- uw.ind[row.out.ind]
    X[row.in, ] <- Xcand[xo[row.out], ]
    if ( ! ("matrix" %in% class(X[-row.in, ])) ) {
      X.rrow <- matrix(X[-row.in, ], nrow = 1)
    }
    Xr <- matrix(X[row.in, ], nrow = 1)
    if ( "matrix" %in% class(X[-row.in, ])) {
      dr <- distance(Xr, X[-row.in, ])
    }
    else {
      dr <- distance(Xr, X.rrow)
    }
    D[row.in, -row.in] <- D[-row.in, row.in] <- as.numeric(dr)
    dprime <- as.numeric(D[upper.tri(D)])
    mdprime <- min(dprime)
    mdprime.ind <- which(D == mdprime, arr.ind = TRUE)[, 
                                                       1]
    Xun[row.out, ] <- Xcand[xi[row.in], ]
    Du[, row.in] <- as.numeric(distance(Xr, Xun))
    Du[row.out, ] <- as.numeric(distance(xold, X))
    if (!is.null(Xorig)) {
      D2[row.in, ] <- as.numeric(distance(Xr, Xorig))
      md2prime <- min(D2)
      if (md2prime < mdprime) {
        mdprime <- md2prime
        mdprime.ind <- which(D2 == md2prime, arr.ind = TRUE)[, 
                                                             1]
      }
      else if (md2prime == mdprime) {
        mdprime.ind <- unique(c(mdprime.ind, which(D2 == 
                                                     md2prime, arr.ind = TRUE)[, 1]))
      }
      Du2.newcols[row.out, ] <- as.numeric(distance(xold, 
                                                    Xorig))
      uwprime.ind <- which(rowSums(Du > mdprime) == ncol(Du) & 
                             rowSums(Du2.newcols > mdprime) == ncol(Du2.newcols))
    }
    else {
      uwprime.ind <- which(rowSums(Du > mdprime) == ncol(Du))
    }
    if ((mdprime < md)) 
      stop("md should increase with each iteration :-|")
    xiold <- xi[row.in]
    xi[row.in] <- xo[row.out]
    xo[row.out] <- xiold
    md <- mdprime
    md.ind <- mdprime.ind
    uw.ind <- uwprime.ind
    mind[t + 1] <- md
    if (!is.null(tempfile)) 
      save(xi, mind, t, file = tempfile)
    if (verb == TRUE) 
      if (t%%10 == 0) 
        cat("t=", t, "/Tmax=", Tmax, " is done.\n", 
            sep = "")
  }
  return(list(inds = xi, mis = mind))
}