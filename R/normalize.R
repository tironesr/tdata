#' A Function to normalze data
#'
#' This function allows you to quickly normalize a vector of data.
#' @param x a continuous variable
#' @export
#' @examples
#' normalize(iris$Sepal.Width)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)}
