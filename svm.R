library(e1071)
setwd("D:/UPC/3R/AA2/AA2-projecte/")
load("train.RData") #dataset
load("test.RData")

N <- dim(train)[1]
k <- 10
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)
Cs = c(0.01, 0.1, 0.5, 1, 10)

train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    t_aux <- train[folds!=i,] # for building the model (training)
    valid <- train[folds==i,] # for prediction (validation)
    names(t_aux)[1]
    
    switch(which.kernel,
           linear={model <- svm(X1 ~ ., data = t_aux, type="C-classification", probability = T, cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(X1 ~ ., data = t_aux, type="C-classification", probability = T, cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(X1 ~ ., data = t_aux, type="C-classification", probability = T, cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF   ={model <- svm(X1 ~ ., data = t_aux, type="C-classification", probability = T, cost=mycost, kernel="radial", scale = F)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
     x_valid <- valid[,2:length(valid)]
     pred <- predict(model,x_valid)
     t_true <- valid[,1]
    
     #compute validation error for part 'i'
     valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}


is_first = T
sink('results.txt')
cat(kernels)
cat("\n")
for (k in kernels) {
  for(c in Cs) {
    if (is_first == T) {
      cat(c) 
      cat(" ")
      } 
    #error_svm <- train.svm.kCV("RBF", c)
    error_svm <- 50
    cat(error_svm)
  }
  cat("\n")
  is_first = FALSE
}
sink()

