library(kernlab)
library(imager)
library(xtable)
library(caret)
options(scipen = 999)

source("process_data.R")
# Read Data, only if it doesnt exist
if (!file.exists('data/train.RData')) read_data(train = T)
if (!file.exists('data/test.RData')) read_data(train = F)
# clear workspace
rm(list = ls())
load("data/train.RData")
load("data/test.RData")

# Fits an SVM
source("svm.R")
Cs = c(0.01, 0.1, 0.5, 1, 2, 10, 20, 50, 100)
kernels = c("vanilladot", "rbfdot", "polydot", "logdot", 
            "laplacedot", "mix_rbfdot_polydot", "mix_rbfdot_logdot",
            "mix_logdot_polydot")
hyperparams <- list(
  'mix' = c(0.2, 0.4, 0.5, 0.6, 0.8),
  'sigma' = c(1e-4, 1e-3, 1e-2, 1e-1, 1, 10),
  'degree' = c(2, 3, 4, 5)
)
fit_models(kernels, Cs)
