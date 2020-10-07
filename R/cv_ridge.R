#Cross validation of ridge regression

#'  Use cross validation to find the optimal ridge penalty parameter
#'
#' @param form A formula specifying the model.
#' @param d A dataframe.
#' @param lambdas The candidate tuning parameters for the ridge penalty term
#' @param tol Tolerance for the condition number to truncate colinear variables
#' @param seed A seed to control the randomness
#' @return A list with fitted coefficients, regression data X,Y and tolerance
#' @examples
#' cv_ridge(Sepal.Length~.,iris,lambdas=seq(0,1,0.01),tol=1e-8,seed=1234)
#' @import rsample
#' @import foreach
#' @import stats
#' @export
cv_ridge <- function(form,d,lambdas,tol=1e-8,seed){

  #Start 10-fold training-testing split
  set.seed(seed)
  folds <- vfold_cv(d)
  #Get the mean out-of-sample prediction error for each lambda over 10 folds
  os_mse <- foreach(lambda=lambdas) %do%{

    foreach(fold=folds$splits,.combine = c) %do% {
      fit <- my_ridge(form,analysis(fold),lambda,tol = tol)
      response <- as.character(form[2])
      mean((as.vector(assessment(fold)[,response])-predict(fit,assessment(fold)))^2)
    }
  }
 lambda_mse <- sapply(os_mse,mean)

  #Plot the lambda vs. MSE
  plot(lambdas,lambda_mse,main = "Out-of-sample error vs. Lambda")

  #Return the optimal lambda that minimize the  mean prediction error
  opt_lambda <- lambdas[which.min(lambda_mse)]
  min_mse <- lambda_mse[which.min(lambda_mse)]

  return(list(cv_mse = lambda_mse, opt_lambda=opt_lambda,min_mse=min_mse))

}
