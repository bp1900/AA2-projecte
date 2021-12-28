read_data <- function(train = T) {
  if (train) {path = "data/training_set"; n = 90}
  else {path = "data/test_set"; n = 10}
  
  classes <- list.files(path)
  x <- matrix(nrow = n*10, ncol = 771)
  y <- c()
  
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
      
      y <- c(y, classes[i])
      x[count, ] <- c(rmean, gmean, bmean, rstd, gstd, bstd, rhist, ghist, bhist)
      count = count + 1
    }
  }
  y <- as.factor(y)
  
  if (train) {
    x_train <- x
    y_train <- y
    save(y_train, x_train, file = "data/train.RData")
  } else {
    x_test <- x
    y_test <- y
    save(y_test, x_test, file = "data/test.RData")
  }
}