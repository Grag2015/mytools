#' converts text string in cyrillic into latin text string (letter by letters)
#'
#' @param "str" is string in cyrillic
#' @param "space" is string to replace space, "_" - by default
#' @return latin text string
#' @author Grag2015
#' @description converts text string in cyrillic into latin text string (letter by letters)
#' digits and latin symbols stay at place, space is replaced on "space", other symbols are deleted
#' @export stringr
#' @import
#' @examples
#' str <- "��� �����"
#' translit(str)
#' "moya zhizn"

translit <- function(str, space="_"){
    tolat <- function(s){
        if (grepl('([a-z]|\\d{1})', s)) {
            res <- s
        } else{
            res <- switch (s,
                           �="a",
                           �="b",
                           �="v",
                           �="g",
                           �="d",
                           �="e",
                           �="e",
                           �="zh",
                           �="z",
                           �="i",
                           �="y",
                           �="k",
                           �="l",
                           �="m",
                           �="n",
                           �="o",
                           �="p",
                           �="r",
                           �="s",
                           �="t",
                           �="u",
                           �="f",
                           �="h",
                           �="ts",
                           �="ch",
                           �="sh",
                           �="sch",
                           �="ii",
                           �="",
                           �="",
                           �="e",
                           �="yu",
                           �="ya",
                           ' '=space
            )
        }
    }
    n <- nchar(str)
    str_t <- str_trim(str)
    str_t <- tolower(str)
    str_res <- ""
    for (i in 1:n) {
        s <- substr(str_t,i,i)
        str_res <- paste0(str_res,tolat(s))
    }
    str_res
}



