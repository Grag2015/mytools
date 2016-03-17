#' converts text string in cyrillic into latin text string (letter by letters)
#'
#' @param "str" is string or vector of strings in cyrillic
#' @param "space" is string to replace space, "_" - by default
#' @return latin text string
#' @author Grag2015
#' @description converts text string in cyrillic into latin text string (letter by letters)
#' digits and latin symbols stay at place, space is replaced on "space", other symbols are deleted
#' @export
#' @import stringr
#' @examples
#' str <- "моя жизнь"
#' translit(str)
#' "moya zhizn"

translit <- function(str, space="_"){
    tolat <- function(s){
        if (grepl('([a-z]|\\d{1})', s)) {
            res <- s
        } else{
            res <- switch (s,
                           а="a",
                           б="b",
                           в="v",
                           г="g",
                           д="d",
                           е="e",
                           ё="e",
                           ж="zh",
                           з="z",
                           и="i",
                           й="y",
                           к="k",
                           л="l",
                           м="m",
                           н="n",
                           о="o",
                           п="p",
                           р="r",
                           с="s",
                           т="t",
                           у="u",
                           ф="f",
                           х="h",
                           ц="ts",
                           ч="ch",
                           ш="sh",
                           щ="sch",
                           ы="ii",
                           ь="",
                           ъ="",
                           э="e",
                           ю="yu",
                           я="ya",
                           ' '=space
            )
        }
    }
    str <- as.character(str)
    res <- data.frame(res="",stringsAsFactors = F)
    for (j in 1:length(str)) {
        n <- nchar(str[j])
        str_t <- str_trim(str[j])
        str_t <- tolower(str[j])
        str_res <- ""
        for (i in 1:n) {
            s <- substr(str_t,i,i)
            str_res <- paste0(str_res,tolat(s))
        }
        res[j,1] <- str_res
    }
    res
}

