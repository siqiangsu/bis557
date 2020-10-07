#Dispatch predict to ridge

predict.my_ridge <- function(object,...){
  dots <- list(...)
  d <- dots[[1]]
  if(!inherits(d,"data.frame")){
    stop("Second argument must be a dataframe")
  }
  m <- model.matrix(object$form,d)
  m %*% object$coefficients

}
