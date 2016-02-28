#' This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel
#'
#' @param df is dataset with numeric columns
#' @return plot described above
#' @author Grag2015
#' @description This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel. This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel.
#' @export
#' @import graphics
#' @examples
#' df <- data.frame(1:10, 11:20)
#' predict.flatvalue(x)

predict.flatvalue <- function(x) {
    library(caret)
    library(rpart)
    training <- dget("training")
    fit2 <- dget("fit2")
    predict(fit2, newdata = x)
}
