#Gradient Decent function

#' This function will fit a linear model using gradient descent.
#'
#' @param form A formula specifying the model.
#' @param d A dataframe.
#' @param eta Learning rate. Default is 0.05.
#' @param contrasts A list specifying the contrasts. The default is NULL.
#' @param iters Total iterations.
#' @return beta Fitted coefficients.
#' @examples
#' my_gd(Sepal.Length~.,iris)
#' @export

my_gd <- function(form,d,eta=0.05,iters=100000,contrasts=NULL){

  #Prepare design matrix and response vector (with contrasts if required)
  d1 <- model.frame(form,d)
  X <- model.matrix(form,d1,contrasts.arg = contrasts)
  y_name <- as.character(form)[2]
  Y <- matrix(d1[,y_name],ncol=1)

  #Default initial value at 0
  beta <- rep(0,ncol(X))

  #Check singularity
  eigenval <- eigen(t(X)%*%X)$values
  if (sum(eigenval < 1e-05)>=1){
    X1 <- X[,-which(eigenval < 1e-05)]
    eta <- 10^(-20)
  } else{
    X1 <- X
  }
  beta1 <- rep(0,ncol(X1))

  #gradient
  for (i in 1:iters){
    beta1 <- beta1-eta*(-t(X1)%*%Y + t(X1)%*%X1%*%beta1)/length(Y)
  }

  if (sum(eigenval < 1e-05)>=1){
    beta[which(eigenval < 1e-05)] <- NA
    beta[-which(eigenval < 1e-05)] <- beta1
  } else{
    beta <- beta1
  }

  return(beta)
}
