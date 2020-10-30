# Multinomial Logistic Regression

#' Logistic regression for K classes
#'
#' @param X A matrix of independent variables
#' @param y A vector of response variable
#' @param maxit Maximum number of iterations
#' @return A list of fitted probabilities, classes and misclassification rate.
#' @examples
#' \dontrun{multi_logistic(X=iris[,-5],y=iris[,5],maxit=20)}
#' @export
multi_logistic <- function(X,y,maxit=100){
  classes <- length(unique(y))
  beta <- matrix(0,nrow = classes,ncol = ncol(X))
  b_old <-  matrix(0,nrow = classes,ncol = ncol(X))

  for(i in seq_len(maxit))
  {
    for (j in 1:classes){

      b_old[j,] <- beta[j,]
      p <- 1 / (1 + exp(- X %*% beta[j,]))
      W <- as.numeric(p * (1 - p))
      XtX <- crossprod(X, diag(W) %*% X)
      score <- t(X) %*% (1*(y==unique(y)[j]) - p)
      delta <- solve(XtX, score)
      beta[j,] <- beta[j,] + delta

    }


  }

  #We compute fitted probabilities
  p_new <- matrix(0,nrow = nrow(X),ncol = classes)
  sum <- 0
  for (k in 1:classes) {
    sum <- sum + exp(X%*%beta[k,])

  }
  for (m in 1:classes){
    p_new[,m] <- exp(X%*%beta[m,])/sum
  }

  #We compute fitted classification
  ind <- apply(p_new, 1, which.max)
  fitted.y <- unique(y)[ind]

  #We compute mis-classification rate
  mis <- mean(y != fitted.y)

  return(list(fitted.p=p_new,fitted.y=fitted.y,y=y,misclassification = mis))
}
