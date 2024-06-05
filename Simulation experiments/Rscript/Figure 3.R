##################################################
## Project: Active Sampling
## Description: Simulation experiments for Figure 3. 
## Date: 13 May 2024
## Author: Henrik Imberg
##################################################


# Init --------------------------------------------------------------------

# Clean-up.
cat("\14")
rm(list = ls())
gc()

print("Running simulation for Figure 3. Estimated computation time 20–60 minutes.")


# Load packages -----------------------------------------------------------

source("Application/Rscript/load_required_packages.R")
load_required_packages(c("BSDA", "kernlab", "magrittr", "MASS", "mboost", "mgcv", 
                         "progress", "ranger", "stringr", "TeachingDemos", "tidyverse"))


# Load functions ----------------------------------------------------------

source("Simulation experiments/Rscript/functions.R")


# Plot settings. ----------------------------------------------------------

ptsize <- 10
theme_set(theme_bw()) 
theme_update(axis.text = element_text(size = ptsize, colour = "black", family = "serif"),
             axis.line = element_line(colour = "black", linewidth = 0.25), 
             axis.ticks = element_line(colour = "black", linewidth = 0.25), 
             axis.title.y = element_text(margin = margin(t = 0, r = 0.2, b = 0, l = 0.2, unit = 'cm')),
             legend.key.width = unit(1.25, "cm"),
             legend.key.height = unit(0.5, "cm"),
             legend.text = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.title = element_text(size = ptsize, colour = "black", family = "serif"),
             legend.position = "bottom",
             strip.background = element_blank(),
             strip.text = element_text(size = ptsize, colour = "black", family = "serif"),
             panel.border = element_blank(),
             panel.spacing.x = unit(0.3, "cm"),
             panel.spacing.y = unit(0.3, "cm"),
             panel.grid = element_blank(),  
             plot.margin = margin(t = 0, r = 0.2, b = 0, l = 0, unit = 'cm'),
             text = element_text(size = ptsize, colour = "black", family = "serif"))

update_geom_defaults("line", list(linewidth = 1))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("text", list(size = ptsize / .pt, family = "serif"))

rm(ptsize)


# Setup simulations -------------------------------------------------------

# Set parameters.
nreps <- 500 # Number of simulation experiments. 
nmax <- 250 # Maximal subsample size.
allres <- tibble() # To store results. 


# SRS and PPS.
params1 <- crossing(model = c("const", "pps"),
                    naive = FALSE,
                    bsize = 10, 
                    estimator = "default") 


# LM and GAM.
params2 <- tibble(model = c("lm", "gam")) %>% # Auxiliary models. 
  crossing(naive = FALSE, # Default, non-naive, implementation of active sampling, accounting for prediction uncertainty.
           bsize = 10, # Batch size.
           estimator = "default") # Default estimator for the finite population mean. 


# Combine. 
params <- params1 %>% 
  add_row(params2) %>%
  mutate(niter = nmax / bsize) %>% # Number of iterations. 
  crossing(datafile = list.files("Simulation experiments/Data", ".RData")) %>% # Input data sets. 
  filter( str_detect(datafile, "strictly_positive") ) %>% # Only consider strictly positive scenario.
  mutate(bandwidth = NA,
         r2 = NA, 
         normalization = NA)

rm(nmax, params1, params2)


# Set up progress bar.
pb <- progress_bar$new(format = "[:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(params),
                       clear = FALSE)      


# Run experiments ---------------------------------------------------------

for ( i in 1:nrow(params) ) {
  
  # Update progress bar.
  pb$tick()
  
  
  # Extract bandwidth and R2 of true signal from file name. 
  file <- unlist(params$datafile[i])
  splt <- unlist(strsplit(gsub("[^[:alnum:] ]", "&", file), "&"))
  params$bandwidth[i] <- as.numeric(splt[3]) + as.numeric(splt[4]) / 100 
  params$r2[i] <- as.numeric(splt[6]) + as.numeric(splt[7]) / 100 
  params$normalization[i] <- sprintf("%s_%s", splt[8], splt[9])
  rm(splt, file)

  
  # Load data.
  file <- sprintf("Simulation experiments/Data/SimData_Bandwidth_%.2f_R2_%.2f_%s.RData", 
                  params$bandwidth[i], 
                  params$r2[i], 
                  params$normalization[i])
  load(file)
  
  
  # Set seed for reproducibility. 
  char2seed(sprintf("%.2f_%.2f_%s", params$bandwidth[i], params$r2[i], params$normalization[i]), set = TRUE)
  
  
  # To store results. 
  sqerr <- matrix(NA, nrow = nreps, ncol = params$niter[i]) 
  if ( params$model[i] ==  "const" ) {
    sqerr_cv <- sqerr_ratio <- matrix(NA, nrow = nreps, ncol = params$niter[i]) 
  }
  
  
  # Run repeated sub-sampling experiments. 
  for ( j in 1:nreps ) {
    
    # Active sampling.
    res <- active_sampling(data = dta, 
                           ninit = params$bsize[i], 
                           bsize = params$bsize[i], 
                           niter = params$niter[i], 
                           model = params$model[i],
                           naive = params$naive[i],
                           estimator = params$estimator[i]) 
    
    # Store results 
    sqerr[j, ] <- res$sqerr
    if ( params$model[i] ==  "const" ) {
      sqerr_cv[j, ] <- res$sqerr_cv
      sqerr_ratio[j, ] <- res$sqerr_ratio
    }
    
    rm(res)

  }
  
  # Combine results.
  res <- tibble(bandwidth = params$bandwidth[i], # Gaussian bandwidth, true signal.
                r2 = params$r2[i], # R2, true signal.
                normalization = params$normalization[i], # Normalization, true signal (zero mean or support of positive real line)
                model = params$model[i], # Auxiliary model.
                bsize = params$bsize[i], # Batch size.
                naive = params$naive[i], # Implementation of active sampling (naive or not).
                estimator = params$estimator[i], # Implementation of active sampling (naive or not).
                n = cumsum(seq(params$bsize[i], params$bsize[i], length.out = params$niter[i])), # Cumulative sample size.
                mse = apply(sqerr, 2, mean), # Mean squared error.
                sd_mse = apply(sqerr, 2, sd), # Standard deviation of the MSE.
                nreps = nreps) # Number of simulations/repetitions.
  
  
  # Additional estimators for SRS. 
  if ( params$model[i] == "const" ) {
    
    # Control variate estimator. 
    res_cv <- res %>% 
      mutate(model = "cv", 
             mse = apply(sqerr_cv, 2, mean), 
             sd_mse = apply(sqerr_cv, 2, sd)) 
    
    # Ratio estimator.
    res_ratio <- res %>% 
      mutate(model = "ratio", 
             mse = apply(sqerr_ratio, 2, mean), 
             sd_mse = apply(sqerr_ratio, 2, sd)  )
    
    # Add in long format.
    res %<>% 
      add_row(res_cv) %>% 
      add_row(res_ratio) %>% 
      dplyr::select(names(res)[1:11])
    
    rm(res_cv, res_ratio)
  }
  
  
  # Add 95% CI for RMSE.
  res %<>%
    mutate(mse_low = pmax(0, mse - 1.96 * sd_mse / sqrt(nreps)),
           mse_high = mse + 1.96 * sd_mse / sqrt(nreps),
           rmse = sqrt(mse),
           rmse_low = sqrt(mse_low),
           rmse_high = sqrt(mse_high))
  
  
  # Append results 
  allres <- allres %>% bind_rows(res)
  
  
  # Clean-up.
  rm(dta, file, res, sqerr)
  if ( params$model[i] == "const" ) {
    rm(sqerr_cv, sqerr_ratio)
  }
  gc()
  
}

# Clean-up.
rm(i, j, nreps, pb)


# Post-processing ---------------------------------------------------------

allres %<>% 
  mutate(star = "")

# Test performance gain vs simple random sampling.
for ( i in 1:nrow(params) ) {
  if ( !(params$model[i] %in% c("const", "pps", "cv", "ratio")) ) {
 
    # Active sampling performance. 
    as <- allres %>%
      filter( model == params$model[i]
              & bsize == params$bsize[i]
              & naive == params$naive[i]
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])
    
    # Simple random sampling performance. 
    ref <- allres %>%
      filter( model == "const"
              & bandwidth == params$bandwidth[i]
              & r2 == params$r2[i]
              & normalization == params$normalization[i]
              & estimator == params$estimator[i])

    # To store results.     
    nseq <- sort(unique(as$n))
    nn <- length(nseq)
    p <- rep(NA, nn)
    
    # Test if significant improvement with active sampling vs SRS.
    for ( j in 1:nn ) {
      
      x <- as[which(as$n == nseq[j]), ]
      y <- ref[which(ref$n == nseq[j]), ]
      
      test <- tsum.test(mean.x = x$mse, s.x = x$sd_mse, n.x = x$nreps, 
                        mean.y = y$mse, s.y = y$sd_mse, n.y = y$nreps)
      
      p[j] <- test$p.value
      
    }
    
    # "Star" (asterisk) for significant improvement. 
    star <- rep("", nn)
    ix <- which(vapply(1:nn, function(ix) all(p[ix:nn] < 0.05), logical(1)))[1]
    star[ix] <- "*"
    
    # Sample size reduction of active sampling vs SRS for the same level of performance. 
    sample_size_ratio <- rep(NA, nn)
    n_ref <- ref$n[length(ref$rmse)]
    rmse_ref <- ref$rmse[length(ref$rmse)]
    as_rmse <- approx(as$n, as$rmse, xout = min(nseq):max(nseq))
    n_as <- as_rmse$x[which(as_rmse$y < rmse_ref)[1]]
    sample_size_ratio[nn] <- n_as / n_ref
    
    # Add to results. 
    allres[which(allres$model == params$model[i]
                 & allres$bsize == params$bsize[i]
                 & allres$naive == params$naive[i]
                 & allres$bandwidth == params$bandwidth[i]
                 & allres$r2 == params$r2[i]
                 & allres$normalization == params$normalization[i]
                 & allres$estimator == params$estimator[i]), c("star", "sample_size_ratio")] <- data.frame(star, sample_size_ratio)
    
    # Clean-up.
    rm(list = setdiff(ls(), c("allres", "params")))
  }
}
rm(i, params)


# Make Figure 3 -----------------------------------------------------------

# For nice display of R2 in figures. 
allres$r2lab = allres$r2
allres$r2lab <- as.factor(allres$r2lab)
levels(allres$r2lab) <- c(expression(paste("R"^2, " = 0.10")), 
                          expression(paste("R"^2, " = 0.50")), 
                          expression(paste("R"^2, " = 0.75")),
                          expression(paste("R"^2, " = 0.90")))

# For nice display of sigma in figures. 
allres$sigma = allres$bandwidth
allres$sigma <- as.factor(allres$sigma)
levels(allres$sigma) <- c(expression(paste(sigma, " = 0.1")), 
                          expression(paste(sigma, " = 1")), 
                          expression(paste(sigma, " = 10")))

# Breaks and labels. 
breaks <- c("const", "cv", "ratio", "pps", "lm", "gam", "gbt", "gpr", "rf")
labels <- c("Simple random sampling", "Control variates", "Ratio estimator", "Importance sampling", "Active sampling+LM", "Active sampling+GAM", "Active sampling+GBT", "Active sampling+GPR", "Active sampling+RF")

# Prepare plot data set. 
plt <- allres %>% 
  mutate(model_num = as.numeric(factor(model, levels = unique(model))),
         xstar = n, 
         ystar = max(rmse) - 0.3 * 1.2^(-2.5 * model_num)) 

# Plot.
p <- plt %>% 
  ggplot(aes(x = n, color = model, fill = model, linetype = model)) +
  geom_ribbon(aes(ymin = rmse_low, ymax = rmse_high), color = NA, show.legend = FALSE, alpha = 0.5) + 
  geom_line(aes(y = rmse)) +
  geom_text(aes(x = xstar, y = ystar, label = star), show.legend = FALSE, size = 16 / .pt, vjust = 1) + 
  scale_colour_brewer(palette = "Dark2", breaks = breaks, label = labels) +
  scale_fill_brewer(palette = "Dark2", breaks = breaks, label = labels) +
  scale_linetype_discrete(breaks = breaks, label = labels) +
  scale_y_continuous(trans = "log10") +
  facet_grid(r2lab~sigma, labeller = label_parsed) +
  labs(x = "Sample size",
       y = "eRMSE",
       colour = NULL,
       fill = NULL,
       linetype = NULL) 

print(p)

# Save.
ggsave("Simulation experiments/Figures/Figure 3.png", width = 160, height = 130, unit = "mm", dpi = 1000)

# Clean-up.
rm(p, plt, breaks, labels)

print("Done!")
