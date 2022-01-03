fit_models <- function(ks) {
  x_train_df <- as.data.frame(x_train)
  # best hyperparamter search
  model <- train(x_train_df, y_train,
               method     = "knn",
               tuneGrid   = expand.grid(k = ks),
               trControl  = trainControl(method  = "cv", number = 5),
               metric     = "Accuracy")
  # cv error with best k
  fit <- train(x_train_df, y_train,
               method     = "knn",
               tuneGrid   = model$bestTune,
               trControl  = trainControl(method  = "cv", number = 5),
               metric     = "Accuracy")
  cv_err <- 1 - as.numeric(fit$results['Accuracy'])
  pred <- knn(train = x_train, test = x_test, cl = y_train, k = model$bestTune)
  val_err <- sum(pred != y_test)/length(y_test)
  
  print(paste0("Best k: ", model$bestTune, " CV_err: ", cv_err, " VAL_err: ", val_err))
}