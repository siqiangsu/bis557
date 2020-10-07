#Ridge regression function

#'  Fitting a linear model using ridge regression
#'
#' @param form A formula specifying the model.
#' @param d A dataframe.
#' @param lambda The tuning parameter for the ridge penalty term
#' @param tol Tolerance for the condition number to truncate colinear variables
#' @return A list with fitted coefficients, regression data X,Y and tolerance
#' @examples
#' my_ridge(Sepal.Length~.,iris,lambda=0.01,tol=1e-8)
#' @export
my_ridge <- function(form,d,lambda,tol=1e-8){

  #Create model matrix
  d1 <- model.frame(form,d)
  X <- model.matrix(form,d1)
  y_name <- as.character(form)[2]
  Y <- matrix(d1[,y_name],ncol = 1)

  #Take colinear variables into account by checking the condition number
  svd_x <- svd(X)
  cond_num <- svd_x$d / svd_x$d[1]
  trunc <- max(which(tol<cond_num))
  svd_x$d <- svd_x$d[seq_len(trunc)]
  svd_x$u <- svd_x$u[,seq_len(trunc)]
  svd_x$v <- svd_x$v[,seq_len(trunc)]

  #Use truncated version of X to do ridge regression
  Sigma <- diag(svd_x$d,nrow = length(svd_x$d),ncol = length(svd_x$d))
  Lambda <- diag(rep(lambda,length(svd_x$d)),nrow = length(svd_x$d),ncol = length(svd_x$d))
  beta <- (svd_x$v) %*% solve(Sigma^2 + Lambda) %*% Sigma %*% t(svd_x$u) %*% Y

  #Make the output similar to lm()
  beta_names <- rownames(beta)
  beta <- as.numeric(beta)
  names(beta) <- beta_names
  ret <- list(coefficients = beta, tol=tol,form=form)
  class(ret) <- "my_ridge"
  return(ret)

}
