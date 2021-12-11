rm(list = ls())
library(e1071)
setwd("D:/UPC/3R/AA2/AA2-projecte/")
load("train.RData") 
load("test.RData")

N <- dim(x_train)[1]
k <- 3
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    train <- x_train[folds!=i,] # for building the model (training)
    y_tra <- y_train[folds!=i] # for building the model (training)
    valid <- x_train[folds==i,] # for prediction (validation)
    y_val <- y_train[folds==i] # for prediction (validation)
    
    switch(which.kernel,
           linear={model <- svm(train, y_tra, type="C-classification", probability = T, cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(train, y_tra, type="C-classification", probability = T, cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(train, y_tra, type="C-classification", probability = T, cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF   ={model <- svm(train, y_tra, type="C-classification", probability = T, cost=mycost, kernel="radial", scale = F)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
     pred <- predict(model,valid)

     #compute validation error for part 'i'
     valid.error[i] <- sum(pred != y_val)/length(y_val)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}

Cs = c(0.01, 0.1, 0.5, 1, 10)
kernels = c("linear", "RBF")

sink('results.txt')
cat("\t")
cat(Cs)
cat("\n")
for (ker in kernels) {
  cat(k)
  cat(" ")
  for(c in Cs) {
    error_svm <- train.svm.kCV(ker, c)
    cat(error_svm)
    cat(" ")
  }
  cat("\n")
}
sink()

