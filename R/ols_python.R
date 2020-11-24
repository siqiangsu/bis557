
#'  Fitting a linear model using python in R
#' @param X A design matrix.
#' @param Y A response vector.
#' @examples
#' \dontrun{ols_python(X,Y)}
#' @export

ols_python <- function(X,Y){

  qr <- r_to_py(np$linalg$qr(X))
  q <- qr[[0]]
  r <- qr[[1]]
  return(r_to_py(np$linalg$inv(r))$dot(q$T)$dot(Y))

}
