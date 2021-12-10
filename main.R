library(e1071)
library(kernlab)
library(jpeg)
library(imager)
library(data.table)

# setwd("D:/UPC/3R/AA2/AA2-projecte/")
path = "data/training_set"

classes <- list.files(path)
dataset <- data.frame()

max.w = 0
max.h = 0

# search for max sizes to pad
for (i in 1:length(classes)) {
  path_class = paste(path, classes, sep = '/')
  file_list <- list.files(path_class[i])
  for (j in 1:length(file_list)) {
    print(j)
    temp_data <- load.image(paste(path_class[i], file_list[j], sep = '/'))
    max.w = max(max.w, width(temp_data))
    max.h = max(max.h, height(temp_data))
  }
}

dataset <- data.frame(matrix(vector(), 90*10, max.h*max.w*3 + 1))

# read images, pad and add to dataset
count <- 1
for (i in 1:length(classes)) {
  path_class = paste(path, classes, sep = '/')
  file_list <- list.files(path_class[i])
  for (j in 1:length(file_list)) {
    print(j)
    temp_data <- load.image(paste(path_class[i], file_list[j], sep = '/'))
    # pad
    temp_data <- pad(temp_data, max.w - width(temp_data), "x")
    temp_data <- pad(temp_data, max.h -height(temp_data), "y")
    # flatten
    flat_image <- (as.data.frame(temp_data))$value
    # append to dataset
    flat_image <- c(classes[i], flat_image)
    dataset[count, ] <- flat_image
    count <- count + 1
  }
}

# save(dataset, file = "dades.RData")
load("dades.RData")


# Kernel SVM
dataset_small = dataset[c()]
svm(X1 ~ ., data = dataset_small, type="C-classification", probabilty = TRUE, cost = 0.5, kernel="radial", scale = FALSE)

C <- c(0.01, 0.1, 1, 2, 5)
# https://rpubs.com/markloessi/506713
# in creating the folds we specify the target feature (dependent variable) and # of folds
folds = createFolds(training_set$Purchased, k = 10)
# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = training_set[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = training_set[x, ] # here we describe the test fold individually
  # now apply (train) the classifer on the training_fold
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial',
                   cost = C)
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

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
