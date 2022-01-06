# Define Custom Kernels
logdot <- function(x, y) {
  return(-log(1 + sqrt(-(2 * crossprod(x, y) - crossprod(x) - crossprod(y)))**beta, base = 10))
}
gihdot <- function(x, y) {
  return(sum(pmin(x, y)**beta_gih))
}
mix_rbfdot_polydot <- function(x, y) {
  xprod <- crossprod(x, y)
  rbf <- exp(sigma_rbf * (2 *xprod  - crossprod(x) - crossprod(y)))
  poly <- xprod^degree
  return(lambda*rbf + (1 - lambda)*poly)
}
mix_logdot_polydot <-  function(x, y) { 
  xprod <- crossprod(x, y)
  logker <- -log(1 + sqrt(-(2 * xprod - crossprod(x) - crossprod(y)))**beta, base = 10)
  poly <- xprod^degree
  return(lambda*logker + (1 - lambda)*poly)
}
mix_rbfdot_logdot <-  function(x, y) {
  xprod <- crossprod(x, y)
  log <- -log(1 + sqrt(-(2 * xprod - crossprod(x) - crossprod(y)))**beta, base = 10)
  rbf <- exp(sigma_rbf * (2 *xprod  - crossprod(x) - crossprod(y)))
  return(lambda*rbf + (1 - lambda)*log)
}
mix_rbfdot_gihdot <- function(x, y) {
  rbf <- exp(sigma_rbf * (2*crossprod(x, y)  - crossprod(x) - crossprod(y)))
  gih <- gihdot(x, y)
  return(lambda*rbf + (1 - lambda)*gih)
}
mix_logdot_gihdot <- function(x, y) {
  log <- logdot(x, y)
  gih <- gihdot(x, y)
  return(lambda*log + (1 - lambda)*gih)
}
mix_polydot_gihdot <- function(x, y) {
  prod <- crossprod(x, y)^degree
  gih <- gihdot(x, y)
  return(lambda*prod + (1 - lambda)*gih)
}
class(mix_rbfdot_polydot) <- "kernel"
class(mix_logdot_polydot) <- "kernel"
class(mix_rbfdot_logdot) <- "kernel"
class(mix_rbfdot_gihdot) <- "kernel"
class(mix_logdot_gihdot) <- "kernel"
class(mix_polydot_gihdot) <- "kernel"
class(logdot) <- "kernel"
class(gihdot) <- "kernel"

# Function that fits a KSVM
SVM_fit <- function(kern, cost, params) {
  print(paste0(kern, ', degree: ', degree, ', sigma: ', sigma_rbf, ' beta: ', beta, ', lambda: ', lambda))
  if (substr(kern, 1, 3) == 'mix' || kern == 'logdot' || kern == 'gihdot') kern <- get(kern)
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

set_param_vec <- function(k) {
  if (k == 'vanilladot') param_vec <<- c()
  else if (substr(k, 1, 3) == 'mix') param_vec <<- hyperparams[['mix']]
  else if (k == 'rbfdot' || k == 'laplacedot') param_vec <<- hyperparams[['sigma']]
  else if (k == 'polydot') param_vec <<- hyperparams[['degree']]
  else param_vec <<- hyperparams[['beta']]
}

update_param <- function(pos, k, c, PARAMS) {
  if (k != 'vanilladot') j <- param_vec[pos]
  if (k == 'vanilladot') param <- list()
  else if (substr(k, 1, 3) == 'mix') {
    lambda <<- j
    param <- list(lambda = lambda)
    ker1 <- strsplit(k, '_')[[1]][2]
    ker2 <- strsplit(k, '_')[[1]][3]
    if (ker1 == 'rbfdot') {
      sigma_rbf <<- PARAMS['rbfdot', as.character(c)]
      param <- c(param, sigma = sigma_rbf)
    } 
    if (ker1 == 'polydot' || ker2 == 'polydot') {
      degree <<- PARAMS['polydot', as.character(c)]
      param <- c(param, degree = degree)
    }
    if (ker1 == 'logdot' || ker2 == 'logdot') {
      beta <<- PARAMS['logdot', as.character(c)]
      param <- c(param, beta = beta)
    }
    if (ker1 == 'gihdot' || ker2 == 'gihdot') {
      beta_gih <<- PARAMS['gihdot', as.character(c)]
      param <- c(param, beta = beta_gih)
    }
  } else if (k == 'rbfdot' || k == 'laplacedot') {
    sigma_rbf <<- j; 
    param <- list(sigma = sigma_rbf)
  } else if (k == 'polydot') {
    degree <<- j
    param <- list(degree = degree)
  } else {
    beta <<- j
    beta_gih <<- j
    param <- list(beta = beta)  
  }
  return(param)
}

fit_models <- function(kernels, Cs) {
  dir.create('results', showWarnings = F)
  # create dataframes to store the best values of each kernel
  df <- data.frame(matrix(0, nrow = length(kernels), ncol = length(Cs)), row.names = kernels ); names(df) <- Cs
  
  TR_err <- df # train error after hyperparamter tuning using CV
  VAL_err <- df # test error
  PARAMS <- df # best hyperparameters
  CV_err <- data.frame(matrix(999, nrow = length(kernels), ncol = length(Cs)), row.names = kernels ); names(CV_err) <- Cs

  for (k in kernels) {
    set_param_vec(k)
    for (c in Cs) {
      if (k == 'vanilladot') {
        res <- SVM_fit(k, c, list())
        CV_err[k, as.character(c)]  <- res[1]
        TR_err[k, as.character(c)]  <- res[1] # no hyperparamter search on vanilla, directly the train error
        VAL_err[k, as.character(c)] <- res[2]
        
        if (CV_err[k, as.character(c)] < min(CV_err[k, ])) {  # Save conf. matrix of each kernel with best CV error
          file.rename(from = 'results/aux.txt', to = paste0('results/', k, '_best.txt'))
        }
      } else {
        for (j in 1:length(param_vec)) {
          param <- update_param(j, k, c, PARAMS)
          res <- SVM_fit(k, c, param)
          print(res)
          if (res[1] < CV_err[k, as.character(c)]) {  # Save conf. matrix of each kernel with best CV error
            CV_err[k, as.character(c)]  <- res[1]
            PARAMS[k, as.character(c)] = param_vec[j]
            file.rename(from = 'results/aux.txt', to = paste0('results/', k, '_best.txt'))
          }
        }
        # train error with the best hyperparameters found
        param <- update_param(which(PARAMS[k, as.character(c)] == param_vec), k, c, PARAMS)
        res <- SVM_fit(k, c, param)
        TR_err[k, as.character(c)]  <- res[1]
        VAL_err[k, as.character(c)] <- res[2]
      }
    }
  }
  # Save errors and kernel hyperparams
  sink('results/hyperparameter_search_error_CV.tex')
  print(xtable(CV_err, digits = 4, align = rep('c', ncol(CV_err) + 1)))
  sink()
  sink('results/test_error.tex')
  print(xtable(VAL_err, digits = 4, align = rep('c', ncol(VAL_err) + 1)))
  sink()
  sink('results/validation_error.tex')
  print(xtable(TR_err, digits = 4, align = rep('c', ncol(TR_err) + 1)))
  sink()
  sink('results/kernel_hyperparam.tex')
  print(xtable(PARAMS, digits = 6, align = rep('c', ncol(VAL_err) + 1)))
  sink()
}