library(e1071)


setwd("D:/UPC/3R/AA2/AA2-projecte/")
path = "data/training_set"

classes <- list.files(path)
dataset <- data.frame()

vec <- c()

# had to specify columns to get rid of the total column
# for (i in 1:length(classes)) {
#   path_class = paste(path, classes, sep = '/')
#   file_list <- list.files(path_class[i])
#   for (j in 1:length(file_list)){
#     print(j)
#     temp_data <- readJPEG(paste(path_class[i], file_list[j], sep = '/'), native = FALSE)
#     vec <- c(vec, c(temp_data, classes[i]))
#   }
# }

# load("dades.RData")

N = 1000
## a typical choice is k=10
k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

C <- 1

## This function is not intended to be useful for general training purposes but it is useful for illustration
## In particular, it does not optimize the value of C (it requires it as parameter)

train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    switch(which.kernel,
           linear={model <- svm(vec[1], vec[2], type="C-classification", cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(vec[1], vec[2], type="C-classification", cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(vec[1], vec[2], type="C-classification", cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF={model <- svm(vec[1], vec[2], type="C-classification", cost=mycost, kernel="radial", scale = FALSE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    # x_valid <- valid[,1:2]
    # pred <- predict(model,x_valid)
    # t_true <- valid[,3]
    
    # compute validation error for part 'i'
    # valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}

(VA.error.linear <- train.svm.kCV ("RBF", C))
