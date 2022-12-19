# Active Sampling R Code

Code used for the experiments in the paper Active Sampling: A machine-learning-assisted framework for finite population inference with optimal subsamples by Henrik Imberg, Xiaomi Yang, Carol Flannagan and Jonas Bärgman.

Make sure that the boot, caret, magrittr, ranger, stringr and tidyverse packages are installed before running the code. 

* active_sampling.R: Implements the active sampling method for the crash-causation-based scenario generation application considered in the paper. Additional details of input and output parameters of this function are provided in the file header.

* test_active_sampling.R is a small test script with a small input dataset Data/glance_dec_test.R.

* calculate_sampling_scheme.R, estimate_targets.R, estimate_totals.R, initialise_grid.R, safe_caret_train.R, and update_predictions.R are helper functions used internally in active_sampling.R.

* run_simulations.R: Takes input from Data folder, including input data and parameter setup for simulations, to set up and run our experiments.

* call_active_sampling.R: Makes repeated calls to active_sampling.R to evaluate the average performance of the sampling methods. 

Output and figures are stored in the Output folder.

Input data, including input parameters to the experiments, are stored in the Data folder.
