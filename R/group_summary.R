#' A Function For Quick Access to SE & Means for Groups
#'
#' This function allows you to quickly find the mean & se for any variable sorted by groups. It also has an option to visualize the mean and the se quickly and easily.
#' @param var a continuous variable
#' @param df a dataframe containing x & y
#' @param groupvar a grouping variable
#' @param autoplot Would you like a quick visual of the output? Default value is FALSE.
#' @export
#' @examples
#' group_summary(Sepal.Width, iris, groupvar = Species)
#' group_summary(Sepal.Width, iris, groupvar = Species, autoplot = TRUE)



group_summary <- function(var, df, groupvar, autoplot = FALSE){
  group1 <- df[,deparse(substitute(groupvar))]
  plotcheck <-  deparse(substitute(autoplot))
  varlabel <- deparse(substitute(var))
  var <- df[,deparse(substitute(var))]
  grouplevels <- levels(group1)
  means <- vector(mode = "numeric", length = 0)
  for (i in 1:length(grouplevels)){
    means[i] <- mean(var[group1 == grouplevels[i]])
  }
  se <- vector(mode = "numeric", length = 0)
  for (i in 1:length(grouplevels)){
    sd <- sd(var[group1 == grouplevels[i]])
    denom <- sqrt(length(var[group1 == grouplevels[i]]))
    se[i] <- sd/denom
  }
  output <- as.data.frame(rbind(means,se))
  names(output) <- grouplevels
  if(autoplot != "FALSE"){
    mids <- barplot(as.matrix(output[1,]), ylim = c(0,max(as.matrix(output[1,]))*1.1), col = "lightblue1", ylab = varlabel);mids
    full <- as.matrix(rbind(output,mids))
    arrows(x0=full[3,], y0=full[1,]-full[2,], x1=full[3,], y1=full[1,]+full[2,], code=3, angle=90, length=0.25, col = "black")
  }
  return(output)
}

