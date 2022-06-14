# Question: where is the seed used?
source("Rscript/active_learning.R")
library("tidyverse")
library("ggplot2")
source("Rscript/sim_output.R")
load("Data/glance_dec_data_test.R")
sampling_method = c("uniform", 
                    "propto eoff_acc_prob", 
                    "propto eoff_acc_prob * eoff * maximpact0", 
                    "propto eoff_acc_prob * abs(acc) * maximpact0", 
                    "propto eoff_acc_prob * eoff * abs(acc) * maximpact0", 
                    "optimised") 
target = c("none", 
           "baseline impact speed distribution", 
           "impact speed reduction", 
           "crash avoidance",
           "injury risk reduction", 
           "injury risk reduction, stratified")

total_iter = 250
total_nboot = 2
Sim_n =200

inputparameter1 = c(sampling_method[6],target[2],TRUE,1,total_iter, total_nboot,Sim_n)
res1 <- sim_output(df,inputparameter1)
print("res1 is finished")
inputparameter2 = c(sampling_method[6],target[3],TRUE,1,total_iter, total_nboot,Sim_n)
res2 <- sim_output(df,inputparameter2)
print("res2 is finished")
inputparameter3 = c(sampling_method[6],target[4],TRUE,1,total_iter, total_nboot,Sim_n)
res3 <- sim_output(df,inputparameter3)
print("res3 is finished")
inputparameter4 = c(sampling_method[6],target[5],TRUE,1,total_iter, total_nboot,Sim_n)
res4 <- sim_output(df,inputparameter4)
print("res4 is finished")
inputparameter5 = c(sampling_method[6],target[6],TRUE,1,total_iter, total_nboot,Sim_n)
res5 <- sim_output(df,inputparameter5)
print("res5 is finished")
inputparameter6 = c(sampling_method[1],target[1],TRUE,1,total_iter, total_nboot,Sim_n)
res6 <- sim_output(df,inputparameter6)
print("res6 is finished")
  
res_out = list(res1,res2,res3,res4,res5,res6)
save(res_out, file = "Data/res_analysis.R")
