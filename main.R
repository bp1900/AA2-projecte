library(e1071)
library(kernlab)
library(jpeg)
library(imager)
library(data.table)

setwd("D:/UPC/3R/AA2/AA2-projecte/")
path = "data/training_set"

classes <- list.files(path)
x_train <- matrix(nrow = 90*10, ncol = 771)
y_train <- c()

# noves features -> histogrames de color + mitjana + sd
count = 1
for (i in 1:length(classes)) {
  path_class = paste(path, classes, sep = '/')
  file_list <- list.files(path_class[i])
  for (j in 1:length(file_list)) {
    print(j)
    temp_data <- load.image(paste(path_class[i], file_list[j], sep = '/'))
    r = R(temp_data)
    g = G(temp_data)
    b = B(temp_data)
    rhist = (hist(r, seq(0, 1, length = 256), plot = FALSE))$density
    ghist = (hist(g, seq(0, 1, length = 256), plot = FALSE))$density
    bhist = (hist(b, seq(0, 1, length = 256), plot = FALSE))$density
    rmean = mean(r)
    gmean = mean(g)
    bmean = mean(b)
    rstd = sd(r)
    gstd = sd(g)
    bstd = sd(b)
    
    y_train <- c(y_train, classes[i])
    x_train[count, ] <- c(rmean, gmean, bmean, rstd, gstd, bstd, rhist, ghist, bhist)
    count = count + 1
  }
}
y_train <- as.factor(y_train)
save(y_train, x_train, file = "train.RData")


path = "data/test_set"
classes <- list.files(path)

x_test <- matrix(nrow = 10*10, ncol = 771)
y_test <- c()

count = 1
for (i in 1:length(classes)) {
  path_class = paste(path, classes, sep = '/')
  file_list <- list.files(path_class[i])
  for (j in 1:length(file_list)) {
    print(j)
    temp_data <- load.image(paste(path_class[i], file_list[j], sep = '/'))
    r = R(temp_data)
    g = G(temp_data)
    b = B(temp_data)
    rhist = (hist(r, seq(0, 1, length = 256), plot = FALSE))$density
    ghist = (hist(g, seq(0, 1, length = 256), plot = FALSE))$density
    bhist = (hist(b, seq(0, 1, length = 256), plot = FALSE))$density
    rmean = mean(r)
    gmean = mean(g)
    bmean = mean(b)
    rstd = sd(r)
    gstd = sd(g)
    bstd = sd(b)
    
    y_test <- c(y_test, classes[i])
    x_test[count, ] <- c(rmean, gmean, bmean, rstd, gstd, bstd, rhist, ghist, bhist)
    count = count + 1
  }
}

y_test <- as.factor(y_test)

save(y_test, x_test, file = "test.RData")
