##################################################
## Project: Active Sampling
## Description: Helper functions for active sampling experiments on simulated data, Section 5 in the paper. 
## Date: 13 May 2024
## Author: Henrik Imberg
##################################################


# To simulate data.
sim_data <- function(N = 1e2, bandwidth = 0.1, r2 = 0.9, normalization = c("zero mean", "strictly positive")) {
  
  normalization <- match.arg(normalization)

  # Transformations. 
  r <- sqrt(r2)
  z <- seq(1 / N, 1, length.out = N)
  K <- outer(z, z, function(x1, x2) exp(-(x1 -x2)^2 / bandwidth^2)) 
  yhat <- mvrnorm(mu = rep(0, times = N), Sigma = K)
  yhat <- (yhat - mean(yhat)) / sd(yhat) * r
  y <- yhat + rnorm(N, sd = sqrt(1 - r^2))
  
  # Make sure z and y are positively correlated. 
  yhat <- yhat * sign(cor(z, y))
  y <- y * sign(cor(z, y))
  
  # Normalize y. 
  if ( normalization == "zero mean" ) { # Zero mean and unit variance. 
    y <- (y - mean(y)) / sd(y)
    yhat <- (yhat - mean(y)) / sd(y)
  } else if ( normalization == "strictly positive" ) { # Unit variance with support on the positive real line. 
    yhat <- (yhat - min(y) + 0.1) / sd(y)
    y <- (y - min(y) + 0.1) / sd(y) 
  } 

  # Return.
  return(data.frame(z, yhat, y))
}

# Train prediction model. 
train <- function(data, method = c("const", "pps", "lm", "gam", "gbt", "gpr", "rf")) {
  
  method <- match.arg(method)

  if ( method == "const") {   
    mod <- lm(y ~ 1, data = data)
  } else if ( method == "pps") {   
    mod <- lm(y ~ -1 + z, data = data)
  } else if ( method == "lm") {   
    mod <- lm(y ~ z, data = data)
  } else if ( method == "gam" ) { 
    mod <- gam(y ~ s(z), data = data)
  } else if ( method == "gbt" ) {
    mod <- blackboost(y ~ z, data = data)
  } else if ( method == "gpr" ) {
    mod <- quietly(gausspr)(y ~ z, data = dta)$result # Quiet wrapper. 
  } else if ( method == "rf" ) {
    mod <- ranger(y ~ z, data = data)
  }

  return(mod)
}

# Safe wrapper for train().
safe_train <- function(...) { 
  safely(train, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}

# Quiet correlation function. 
quiet_cor <- function(...) {
  r <- quietly(cor)(...)$result
  if ( is.na(r) ) {
    r <- 0
  }
  return(r)
} 

# Predict. 
my_predict <- function(object, newdata) {

  if ( "ranger" %in% class(object) ) {
    yhat <- predict(object, newdata)$pred
  } else {
    yhat <- predict(object, newdata)
  } 
  
  return(yhat)
}

# Extract residual SD (RMSE) from model fit. 
get_rmse <- function(object) {
  
  if ( class(object)[1] %in% c("lm", "gam") ) {
    sigma <- sigma(object)
  } else if ( class(object)[1] == "blackboost" ) {
    sigma <- sd(object$resid())
  } else if ( class(object)[1] == "gausspr" ) {
    sigma <- object@error
  } else if ( class(object)[1] == "ranger" ) {
    sigma <- object$prediction.error
  } else {
    cat(sprintf("Model of class %s not implemented.", paste(class(object))))
  }
  
  return(sigma)
}

# Extract R-squared from model fit. 
get_rsquared <- function(object, data) {
  yhat <- my_predict(object, data)
  r2 <- quiet_cor(yhat, data$y)^2
  return(r2)
}

# Active sampling algorithm.
active_sampling <- function(data, # Input dataset. 
                            niter = 4, # Number of iterations. 
                            ninit = 25, # Initial sample size. 
                            bsize = 25, # Batch size.
                            model = c("const", "pps", "lm", "gam", "gbt", "gpr", "rf"), # Surrogate model. 
                            naive = FALSE, # Naive implementation of active sampling? Ignores prediction uncertainty if this is TRUE.
                            estimator = c("default", "Hajek"), # Estimator for the mean. Default (linear) or Hajek (non-linear) estimator. 
                            verbose = FALSE) {
  
  # Set parameters. 
  N <- nrow(data)
  labelled <- NULL
  nseq <- c(ninit, rep(bsize, niter - 1))
  cum_nseq <- cumsum(nseq)
  ground_truth <- mean(data$y)
  mean_z <- mean(data$z)
  sqerr <- rep(NA, niter)
  if ( model == "const" ) { # If simple random sampling.
    sqerr_cv <- sqerr_ratio <- rep(NA, niter) # Squared errors of regression (control variate) and ratio estimators.
  }
  
  # Iterate. 
  for ( i in 1:niter ) {
    
    if (verbose) cat(sprintf("Iteration %d.\n", i))
    
    # Update predictions. 
    if ( i > 1 ) {
      
      # Train. 
      if ( verbose ) cat(sprintf("Train %s.\n", toupper(model)))
      fit <- safe_train(labelled, model)
      if ( is.null(fit) ) { # If unable to fit: use linear model. 
        fit <- safe_train(labelled, "lm")
        if (verbose) cat("Training step failed. Train linear model instead.\n")
      }    
      if (verbose)  cat(sprintf("Prediction RMSE = %.2f, R-squared = %.2f.\n", 
                                get_rmse(fit), 
                                get_rsquared(fit, labelled)))
      
      # Predict. 
      sigma <-  get_rmse(fit)
      if ( is.na(sigma) | is.infinite(sigma) ) {
        if (verbose) cat("Unreliable fit, prediction RMSE = NA or Inf. Constant predictions used instead.\n")
        pred <- rep(est, N) + 1
        sigma <- 0
      } else {
        pred <- my_predict(fit, data)
        sigma <- get_rmse(fit)  
      }
      
      # Naive method: ignore prediction uncertainty. 
      if ( naive ) {
        sigma <- 0
      }
    }
    
    # Sample size current iteration. 
    if ( i == 1 ) {
      n <- ninit
    } else {
      n <- bsize
    }
    if (verbose) cat(sprintf("n current = %d, n total = %d.\n", n, cum_nseq[i]))
    
    # To calculate optimal sampling scheme.
    if ( is.null(labelled) ) { # If no observations have been selected: use simple random sampling.
      size <- rep(1, N) 
    } else { 
      if ( estimator == "default" ) {
        size <- sqrt(pred^2 + sigma^2) # Probability proportional to size. 
      } else if ( estimator == "Hajek" ) {
        size <- sqrt((pred - est)^2 + sigma^2) # Probability proportional to size. 
      }
    }
    
    # To avoid zero or undefined sampling probabilities.
    if  ( any(is.na(size)) | any(size == 0) ) {
      size <- rep(1, N)
    }

    # Sample new instances. 
    prob <- size / sum(size)
    nhits <- rmultinom(n = 1, size = n, prob = size)
    ix <- which(nhits > 0)
    new_sample <- data[ix, ]
    
    # Calculate weights. 
    bwt <- nseq[1:i] / sum(nseq[1:i])
    new_sample$w <- bwt[i] * nhits[ix] / (n * prob[ix])
    
    # Re-weight old batch weights by n_1 + ... n_{k-1} / (n_1 + ... + n_k).
    if ( i > 1 ) {
      rewt <- cum_nseq[i - 1] / cum_nseq[i]
      labelled$w <- labelled$w * rewt
    }

    # Add to labelled dataset.
    labelled <- rbind(labelled, new_sample)
    if ( verbose ) {
      if ( nrow(labelled < 10 ) ) {
        print(head(labelled))
      } else {
        print(rbind(head(labelled), tail(labelled)))
      }
    }
    
    # Estimate.
    if ( estimator == "default" ) {
      est <- with(labelled, sum(w * y) / N) 
      est_z <- with(labelled, sum(w * z) / N) 
    } else if ( estimator == "Hajek" ) {
      est <- with(labelled, sum(w * y) / sum(w)) 
      est_z <- with(labelled, sum(w * z) / sum(w)) 
    }
    sqerr[i] <- (est - ground_truth)^2

    # Additional estimators if simple random sample.
    if ( model == "const" ) {
      est_cv <- est + (mean_z - est_z) * coef(lm(y~z, data = labelled))[2] # Control-variate estimator.
      est_ratio <- est / est_z * mean_z # Ratio estimator.
      sqerr_cv[i] <- (est_cv - ground_truth)^2 # Errors.
      sqerr_ratio[i] <- (est_ratio - ground_truth)^2 
      }
    
    if (verbose) cat(sprintf("Current estimate = %.4f, squared error = %.4f.\n\n", est, sqerr[i]))
    
  }
  
  # Results. 
  res <- data.frame(n = cum_nseq, 
                    sqerr = sqerr)
  
  if ( model == "const" ) {
    res <- data.frame(n = cum_nseq, 
                      sqerr = sqerr, 
                      sqerr_cv = sqerr_cv,
                      sqerr_ratio = sqerr_ratio)
  }
  
  # Return. 
  return(res)
}