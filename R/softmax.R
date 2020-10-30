#Softmax function
softmax <- function(x){
  val <- exp(x)/(sum(exp(x)))
  return(val)
}
