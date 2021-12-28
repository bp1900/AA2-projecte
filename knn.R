library(FNN)

# Function that fits a KSVM
SVM_fit <- function(kern, cost, K_CV) {
  model <- ksvm(x_train, y_train, type = 'C-svc', prob.model = T, scaled = F, cross = 5, 
                kernel = kern, C = cost)
  cv_err <- model@cross
  pred <- predict(model, x_test)
  sink('results/aux.txt')
  print(confusionMatrix(y_test, pred))
  sink()
  val_err <- sum(pred != y_test)/length(y_test)
  return(c(cv_err, val_err))
}

fit_models <- function(K_CV) {
  model <- knn(train = x_train, test = x_test, cl = y_train, k = 11)
  pred <- predict(model, x_test)
  # Save errors
  sink('results/cross_validation.tex')
  print(xtable(CV_err, digits = 4, align = rep('c', ncol(CV_err) + 1)))
  sink()
  sink('results/validation_error.tex')
  print(xtable(VAL_err, digits = 4, align = rep('c', ncol(VAL_err) + 1)))
  sink()
  # Save Confusion Matrix
  sink('results/logreg_best.txt')
  print(confusionMatrix(y_test, pred))
  sink()
}


