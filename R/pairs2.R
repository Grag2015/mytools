#' This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel
#'
#' @param "df" is dataset with numeric columns
#' @return plot described above
#' @author Grag2015
#' @description This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel. This function plots table for paired correlations with coefficients in upper
#' panel and scatterplots in lower panel.
#' @export
#' @import graphics
#' @examples
#' df <- data.frame(1:10, 11:20, (1:10)^2)
#' pairs2(df)

pairs2 <- function(df){
    # пользовательские функции для график попарных корреляций
    panel.density <- function(x, ...) {
        n.groups <-  1
        adjust <-  1
        groups = NULL
        if (n.groups > 1) {
            levs <- levels(groups)
            for (i in 1:n.groups) {
                xx <- x[levs[i] == groups]
                dens.x <- try(density(xx, adjust = adjust, na.rm = TRUE),
                              silent = TRUE)
                if (!inherits(dens.x, "try-error")) {
                    lines(dens.x$x, min(x, na.rm = TRUE) + dens.x$y *
                              diff(range(x, na.rm = TRUE))/diff(range(dens.x$y,
                                                                      na.rm = TRUE)), col = col[i])
                }
                else warning("cannot estimate density for group ",
                             levs[i], "\n", dens.x, "\n")
                rug(xx, col = col[i])
            }
        }
        else {
            dens.x <- density(x, adjust = adjust, na.rm = TRUE)
            lines(dens.x$x, min(x, na.rm = TRUE) + dens.x$y *
                      diff(range(x, na.rm = TRUE))/diff(range(dens.x$y,
                                                              na.rm = TRUE)))
            rug(x)
        }
        #         if (do.legend)
        #             legendPlot(position = if (is.null(legend.pos))
        #                 "topright"
        #             else legend.pos)
        #         do.legend <<- FALSE
    }
    panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x[!is.na(x*y)], y[!is.na(x*y)])
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * max(abs(r), 0.25))
    }
    panel.smooth2 <- function(x, y){
        panel.smooth(x, y,iter = 1)
    }
    pairs(df, diag.panel=panel.density, upper.panel=panel.cor, lower.panel=panel.smooth2)
}

#' This function calculates pairs coeff.correlation and print pairs which have correlation
#' greater than level
#' @param "df" is dataset with numeric columns
#' @return list of pairs
#' @author Grag2015
#' @description This function calculates pairs coeff.correlation and print pairs which have correlation
#' greater than level
#' @export
#' @import graphics
#' @examples
#' df <- data.frame(1:10, 11:20)
#' printcor(df)

printcor <- function(df, level=0.5){
    # на входе дф с числовыми столбцами
    for (i in 1:(ncol(df)-1)){
        for (j in (i+1):ncol(df)){
            x <- df[,i]
            y <- df[,j]
            t <-cor(x[!is.na(x*y)], y[!is.na(x*y)])
            if (abs(t)>=level) {
                print(paste0(names(df)[i]," * ", names(df)[j], " = ", round(t,2)))
            }
        }
    }
}
