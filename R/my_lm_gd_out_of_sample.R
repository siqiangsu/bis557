#'  OLS using gradient descent and compute out-of-sample error
#'
#' @param form A formula specifying the model.
#' @param d A dataframe.
#' @param eta Learning rate. Default is 0.05.
#' @param seed A number controlling the random sampling.
#' @param iters Total iterations.
#' @return A list with fitted coefficients and out-of-sample mean squared error
#' @examples
#' my_lm_gd_out_of_sample(Sepal.Length~.,iris,eta=0.05,iters=100000,seed=1234)
#' @export
my_lm_gd_out_of_sample <- function(form,d,eta=0.05,iters=100000,seed=1234){
  set.seed(seed)
  #Partition our data into training data and testing data 80%-20% split
  d_permute <- d[sample(nrow(d)),]
  d_train <- d_permute[1:floor(0.8*nrow(d)),]
  d_test <- d_permute[-(1:floor(0.8*nrow(d))),]

  beta <- my_gd(form,d_train,eta=0.04,iters=100000,contrasts=NULL)

  #Now evaluate the out-of-sample mean squared error
  d1 <- model.frame(form,d_test)
  X <- model.matrix(form,d1,contrasts.arg = contrasts)
  y_name <- as.character(form)[2]
  Y <- matrix(d1[,y_name],ncol=1)
  Yhat <- X %*% beta
  MSE <- mean((Y-Yhat)^2)

  return(list(beta=beta,out_of_sample_MSE=MSE))

}
