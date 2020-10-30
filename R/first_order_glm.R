#First-Order GLM Maximum Likelihood Solution

#' First-Order GLM Maximum Likelihood Solution
#'
#' @param X A matrix of independent variables
#' @param y A vector of response variable
#' @param mu_fun The function specifying the mean function in exponential family
#' @param tol Tolerance for the convergence
#' @param size A number specifying the constant step size in gradient descent
#' @param maxit Maximum number of iterations
#' @param adapt A logical value. If TRUE, we use momentum gradient descent. If FALSE, we use constant step size
#' @param gamma A parameter controlling the momentum update
#' @return A vector of fitted regression coefficients.
#' @examples
#' \dontrun{first_order_glm(X, y,mu_fun = function(eta) exp(eta), size = 0.00001,adapt = FALSE,maxit = 1000)}
#' @export
first_order_glm <- function(X, y, mu_fun, maxit=100, tol=1e-10,size,adapt=FALSE,gamma=0.9)
  {
    beta <- rep(0,ncol(X))
    for(j in seq_len(maxit))
    {
      b_old <- beta
      eta <- X %*% beta
      mu <- mu_fun(eta)
      score <- t(X) %*% (y - mu)

      if (adapt==FALSE){

        #Use constant step size
        beta <- beta + size* score
      }

      if (adapt==TRUE){

        #Use adapted step size 'momentum'
        grad <- gamma*beta + (1-gamma)*score
        beta <- beta + size*grad
      }

      if(sqrt(crossprod(beta - b_old)) < tol){
        break
      }
    }
    beta
  }
