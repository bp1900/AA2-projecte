library(e1071)
library(kernlab)
library(jpeg)
library(imager)
library(data.table)

setwd("D:/UPC/3R/AA2/AA2-projecte/")
path = "data/training_set"

classes <- list.files(path)
train <- data.frame(matrix(nrow = 0, ncol = 772))
colnames(train) <- paste0("X", 1:772)

# noves features -> histogrames de color + mitjana + sd
# search for max sizes to pad
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
    
    vec <- c(classes[i], rmean, gmean, bmean, rstd, gstd, bstd, rhist, ghist, bhist)
    train <- rbind(train, vec)
  }
}
colnames(train) <- paste0("X", 1:772)
train$X1 <- as.factor(train$X1)
save(train, file = "train.RData")


path = "data/test_set"
classes <- list.files(path)

test <- data.frame(matrix(nrow = 0, ncol = 772))

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
    
    vec <- c(classes[i], rmean, gmean, bmean, rstd, gstd, bstd, rhist, ghist, bhist)
    test <- rbind(test, vec)
  }
}
colnames(test) <- paste0("X", 1:772)
test$X1 <- as.factor(test$X1)
save(test, file = "test.RData")
