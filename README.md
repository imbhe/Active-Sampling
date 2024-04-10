# Active Sampling R Code

Code used for the experiments in the paper Active sampling: A machine-learning-assisted framework for finite population inference with optimal subsamples by Henrik Imberg, Xiaomi Yang, Carol Flannagan and Jonas Bärgman.

Make sure that the boot, caret, magrittr, ranger, stringr and tidyverse packages are installed before running the code. 

* active_sampling.R: Implements the active sampling method for the crash-causation-based scenario generation application considered in the paper. Additional details of input and output parameters of this function are provided in the file header.

* test_active_sampling.R is a small test script with a small input dataset Data/glance_dec_data_test.R.

* calculate_sampling_scheme.R, estimate_targets.R, estimate_totals.R, initialise_grid.R, safe_caret_train.R, and update_predictions.R are helper functions used internally in active_sampling.R.

* run_simulations.R: Takes input from Data folder, including input data and parameter setup for simulations, to set up and run our experiments.

* call_active_sampling.R: Makes repeated calls to active_sampling.R to evaluate the average performance of the sampling methods. 

Output and figures are stored in the Output folder.

Input data, including input parameters to the experiments, are stored in the Data folder.

Replication of Figure 4

The R script “~ActiveSampling/Application/Rscript/Replication_of_Figure4.R” load the data input from “~ActiveSampling/Application/ Data/Data.R” and sampling algorithm type input from “~Application/Input/sampling_method_input_coverage_example.xlsx” and sampling parameter input from “~Application/Input/parameter_input coverage_example.xlsx”. The default setting is to run 500 groups of simulations and 500 groups of bootstraps for both the impact speed reduction target Active sampling and the crash avoidance target Active sampling method. By running this R script “Replication_of_Figure4.R” load”, it runs the default setting of simulations and also the post analysis to plot Figure 4 under folder “~ActiveSampling/Application/Reproduced_figures/” with name “Replication_95_coverage_plot.png”. The simulations can take up to two days.

We have run many kinds of sampling methods with different target and prediction models. It can take time to re-run all the simulations. We have saved all the results based on which the figures in the manuscript were plotted. Below is the explaination on how to get the figures based on the saved results.

Figure 5
Run R script “~/Documents/GitHub/Check reproducibility/ActiveSampling/Application/Rscript/Figure5.R” and a new Figure will be saved by the path “Application/Figures/Figure5.png”.

Figure S7
Run R script “~/Documents/GitHub/Check reproducibility/ActiveSampling/Application/Rscript/FigureS7.R” and a new Figure will be saved by the path “Application/Figures/FigureS7.png”.

