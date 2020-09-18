#Linear-Model function

#' This function will fit a linear model.
#'
#' @param form A formula specifying the model.
#' @param d A dataframe.
#' @param contrasts A list specifying the contrasts. The default is NULL.
#' @return A list with fitted coefficients.
#' @examples
#' linear_model(Sepal.Length~.,iris)
#' @export
linear_model <- function(form,d,contrasts=NULL){

  #Prepare design matrix and response vector (with contrasts if required)
  d1 <- model.frame(form,d)
  X <- model.matrix(form,d1,contrasts.arg = contrasts)
  y_name <- as.character(form)[2]
  Y <- matrix(d1[,y_name],ncol=1)

  #Gradiant Decent
  #beta <- my_gd(X,Y)

  #Use linear algebra to solve the coefficients
  beta <- qr.coef(qr(X),Y)

  #Make the output more alike lm()
  beta_names <- rownames(beta)
  beta <- as.numeric(beta)
  names(beta) <- beta_names

  return(list(coefficients=beta))

}
