library(kernlab)
library(imager)
library(xtable)
library(caret)
library(class)
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
lambda <- 0.5; sigma_rbf <- 1; degree <- 2; beta <- 0.25; beta_gih <- 0.25
Cs = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
kernels = c("vanilladot", "rbfdot", "polydot", "gihdot", "logdot",
            "laplacedot", "mix_rbfdot_polydot", "mix_rbfdot_logdot",
            "mix_logdot_polydot", "mix_rbfdot_gihdot",
            "mix_logdot_gihdot","mix_polydot_gihdot")
param_vec <- c()
hyperparams <- list(
  'mix' = c(0.2, 0.4, 0.5, 0.6, 0.8),
  'sigma' = c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10),
  'degree' = c(2, 3, 4, 5, 6, 7),
  'beta' = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.7, 0.9)
)
source("svm.R")
fit_models(kernels, Cs)

# Fits a KNN
source("knn.R")
fit_models(c(3, 5, 7, 9, 13, 15, 17, 21, 35, 51))






