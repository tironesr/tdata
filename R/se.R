#' A Function to Calculate the SE of a Vector
#'
#' This function allows you to quickly calculate the standard error of a vector.
#' @param x a continuous variable
#' @export
#' @examples
#' se(iris$Sepal.Width)

se <- function(x){
  sd(x)/sqrt(length(x))
}
