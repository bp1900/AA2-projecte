# Define Custom Kernels
logdot <- function(x, y) {
  return(-log(1 + sqrt(-(2 * crossprod(x, y) - crossprod(x) - crossprod(y)))**degree, base = 10))
}

mix_rbfdot_polydot <- function(x, y) {
  xprod <- crossprod(x, y)
  rbf <- exp(sigma_rbf * (2 *xprod  - crossprod(x) - crossprod(y)))
  poly <- xprod^degree
  return(lambda*rbf + (1 - lambda)*poly)
}
mix_logdot_polydot <-  function(x, y) { 
  xprod <- crossprod(x, y)
  logker <- -log(1 + sqrt(-(2 * xprod - crossprod(x) - crossprod(y)))**degree, base = 10)
  poly <- xprod^degree
  return(lambda*logker + (1 - lambda)*poly)
}
mix_rbfdot_logdot <-  function(x, y) {
  xprod <- crossprod(x, y)
  log <- -log(1 + sqrt(-(2 * xprod - crossprod(x) - crossprod(y)))**degree, base = 10)
  rbf <- exp(sigma_rbf * (2 *xprod  - crossprod(x) - crossprod(y)))
  return(lambda*rbf + (1 - lambda)*log)
}
class(mix_rbfdot_polydot) <- "kernel"
class(mix_logdot_polydot) <- "kernel"
class(mix_rbfdot_logdot) <- "kernel"
class(logdot) <- "kernel"

# Function that fits a KSVM
SVM_fit <- function(kern, cost, params) {
  if (kern == 'logdot') {kern = logdot; degree <<- params$degree}
  else if (kern == 'mix_rbfdot_logdot') {kern = mix_rbfdot_logdot; lambda <<- params$lambda; sigma_rbf <<- params$sigma; degree <<- params$degree}
  else if (kern == 'mix_logdot_polydot') {kern = mix_logdot_polydot; lambda <<- params$lambda; degree <<- params$degree}
  else if (kern == 'mix_rbfdot_polydot') {kern = mix_rbfdot_polydot; lambda <<- params$lambda; sigma_rbf <<- params$sigma; degree <<- params$degree}
  model <- ksvm(x_train, y_train, type = 'C-svc', prob.model = T, scaled = F, cross = 5, 
                kernel = kern, C = cost, kpar = params)
  cv_err <- model@cross
  pred <- predict(model, x_test)
  sink('results/aux.txt')
  print(confusionMatrix(y_test, pred))
  sink()
  val_err <- sum(pred != y_test)/length(y_test)
  return(c(cv_err, val_err))
}

fit_models <- function(kernels, Cs) {
  dir.create('results', showWarnings = F)
  CV_err <- data.frame(matrix(0, nrow = length(kernels), ncol = length(Cs)), row.names = kernels )
  names(CV_err) <- Cs
  VAL_err <- CV_err
  PARAMS <- CV_err
  # iterate trying all kernels
  for (k in kernels) {
    print(k)
    min_err = 1    # position of minimal error in df for kernel k
    c = Cs[1]
    # We create the parameter list for `kpar`
    if (k == 'vanilladot') param = list()
    else if (substr(k, 1, 3) == 'mix') {
      ker1 <- strsplit(k, '_')[[1]][2]
      ker2 <- strsplit(k, '_')[[1]][3]
      if (ker1 == 'rbfdot') {
        param = list(lambda = hyperparams[['mix']][1], 
                     sigma = PARAMS[ker1, as.character(c)], degree = PARAMS[ker2, as.character(c)])
        print(PARAMS[ker2, as.character(c)])
      } else {
        deg_max = max(PARAMS[ker1, as.character(c)], PARAMS[ker2, as.character(c)])
        param = list(lambda = hyperparams[['mix']][1], degree = deg_max)
      }
      param_vec = hyperparams[['mix']]
      PARAMS[k, as.character(c)] = param[[1]]
    } else if (k == 'rbfdot' || k == 'laplacedot') {
      param = list(sigma = hyperparams[['sigma']][1])
      param_vec = hyperparams[['sigma']]
      PARAMS[k, as.character(c)] = param[[1]]
    } else {
      param = list(degree = hyperparams[['degree']][1])
      param_vec = hyperparams[['degree']]
      PARAMS[k, as.character(c)] = param[[1]]
    }
    
    res <- SVM_fit(k, c, param)
    CV_err[k, as.character(c)]  <- res[1]
    VAL_err[k, as.character(c)] <- res[2]
    file.rename(from = 'results/aux.txt', to = paste0('results/', k, '_best.txt'))
    for (i in 2:length(Cs)) {
      print(i)
      if (k == 'vanilladot') {
        c = Cs[i]
        res <- SVM_fit(k, c, param)
        CV_err[k, as.character(c)]  <- res[1]
        VAL_err[k, as.character(c)] <- res[2]
        if (CV_err[k, i] < CV_err[k, min_err]) {  # Save Confusion Matrix of each best kernel
          min_err = i
          file.rename(from = 'results/aux.txt', to = paste0('results/', k, '_best.txt'))
        }
      } else {
        for (j in param_vec) {
          print(j)
          c = Cs[i]
          if (k == 'rbfdot' || k == 'laplacedot') param = list(sigma = j)
          else if (substr(k, 1, 3) == 'mix') {
            ker1 <- strsplit(k, '_')[[1]][2]
            ker2 <- strsplit(k, '_')[[1]][3]
            if (ker1 == 'rbfdot') {
              param = list(lambda = hyperparams[['mix']][1], 
                           sigma = PARAMS[ker1, as.character(c)], degree = PARAMS[ker2, as.character(c)])
            } else {
              deg_max = max(PARAMS[ker1, as.character(c)], PARAMS[ker2, as.character(c)])
              param = list(lambda = hyperparams[['mix']][1], degree = deg_max)
            }
            param_vec = hyperparams[['mix']]
            PARAMS[k, as.character(c)] = param[[1]]
          } else param = list(degree = j)
          res <- SVM_fit(k, c, param)
          CV_err[k, as.character(c)]  <- res[1]
          VAL_err[k, as.character(c)] <- res[2]
          if (CV_err[k, i] < CV_err[k, min_err]) {  # Save Confusion Matrix of each best kernel
            min_err = i
            PARAMS[k, as.character(c)] = j
            file.rename(from = 'results/aux.txt', to = paste0('results/', k, '_best.txt'))
          }      
        }
      }
    }
  }
  # Save errors
  sink('results/cross_validation.tex')
  print(xtable(CV_err, digits = 4, align = rep('c', ncol(CV_err) + 1)))
  sink()
  sink('results/validation_error.tex')
  print(xtable(VAL_err, digits = 4, align = rep('c', ncol(VAL_err) + 1)))
  sink()
  sink('results/kernel_hyperparam.tex')
  print(xtable(PARAMS, digits = 6, align = rep('c', ncol(VAL_err) + 1)))
  sink()
}


