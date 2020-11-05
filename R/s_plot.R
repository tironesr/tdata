#' A Plotting Function With Slopes & Interactions
#'
#' This function allows you to quickly visualize the slopes and confidence intervals of your data in informative ways. It is designed for use with two continuous variables and optionally one categorical moderator.
#'
#' NOTE: Please make sure your data has been cleaned of NA's and is a data frame (not a tibble or matrix)
#'
#' @param x a continuous variable for the x axis
#' @param y a continuous variable for the y axis
#' @param df a dataframe containing x & y
#' @param groupvar a grouping variable
#' @param interaction Would you like to see the slopes of each group? Default value is FALSE.
#' @param intplot Would you like to see each group plotted separately with slope and confidence intervals? Default value is FALSE.
#' @param lloc location of legend. Default value is "topleft". Check the ?legend documentation for other location options.
#' @export
#' @examples
#' s_plot(Sepal.Width, Sepal.Length, iris)
#' s_plot(Sepal.Width, Sepal.Length, iris, groupvar = Species)
#' s_plot(Sepal.Width, Sepal.Length, iris, groupvar = Species, interaction = TRUE)
#' s_plot(Sepal.Width, Sepal.Length, iris, groupvar = Species, interaction = TRUE, intplot = TRUE)


  s_plot <- function(x,y,df,groupvar=FALSE, interaction = FALSE, intplot = FALSE, lloc = "topleft"){
    xlab <- deparse(substitute(x))
    ylab <- deparse(substitute(y))
    x <- df[,deparse(substitute(x))]
    y <- df[,deparse(substitute(y))]
    groupcheck <- deparse(substitute(groupvar))
    intcheck <- deparse(substitute(interaction))
    intconfcheck <- deparse(substitute(intplot))
    lm_slope <- lm(y~x,data = df)
    plot <- plot(y~x, xlab = xlab, ylab = ylab)
    legend(lloc, legend = paste0("Beta = ",round(lm_slope$coef[2],3)), bg = "white")
    plotcoldims <- c(1,2,3,4)
    plotrowdims <- c(1,2,3,4)
    if(groupcheck != "FALSE"){
      groupingvar <- df[,deparse(substitute(groupvar))]
      grouplevels <- levels(groupingvar)
      cols <- sample(2:8, length(grouplevels))
      plot <- plot(y~x, xlab = xlab, ylab = ylab, pch = 21, bg = cols[groupingvar])
      legend(lloc, pch = 16, col = cols, legend = grouplevels, bg = "white")
      if(intcheck != "FALSE"){
        int_lm <- list()
        for (i in 1:length(grouplevels)){
          selector <- which(groupingvar == grouplevels[i])
          int_lm[[i]] <- lm(y[selector]~x[selector])
        }
        for (i in 1:length(int_lm)){
          abline(int_lm[[i]], col = cols[i])
        }
        if(intconfcheck != "FALSE"){
          levelnum <- length(grouplevels)
          par(mfrow = c(ceiling(sqrt(levelnum)),ceiling(sqrt(levelnum))))
          for (i in 1:length(grouplevels)){
            intselector <- which(groupingvar == grouplevels[i])
            intdf <- df[intselector,]
            intx <- intdf[,xlab]
            inty <- intdf[,ylab]
            plot(inty~intx, xlab = xlab, ylab=ylab, ylim=c(min(y),max(y)), xlim=c(min(x),max(x)), main =       grouplevels[i])
            int_lm2 <- lm(inty~intx)
            abline(int_lm2)
            xvals <- seq(from = min(x)-10, to = max(x)+10, by = 0.01)
            pred_df <- data.frame(intx = xvals)
            CI <- predict(int_lm2, newdata = pred_df, interval = "confidence")
            CI <- as.data.frame(CI)
            polygon(c(xvals, rev(xvals)),
                    c(CI$lwr, rev(CI$upr)), border = cols[i])
            title(sub = paste0("Beta = ",round(int_lm2$coef[2],3)))

          }
        }
        par(mfrow=c(1,1))
      }}
    if(intcheck %in% "FALSE"){
      abline(lm_slope, col = "red")
      xvals <- seq(from = min(x)-10, to = max(x)+10, by = 0.01)
      pred_df <- data.frame(x = xvals)
      CI <- predict(lm_slope, newdata = pred_df, interval = "confidence")
      CI <- as.data.frame(CI)
      polygon(c(xvals, rev(xvals)),
              c(CI$lwr, rev(CI$upr)))
    }
  }

