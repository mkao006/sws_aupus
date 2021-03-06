##' This function trends the data once
##'
##' @param Num The value column of the element
##' @param Symb The symbol column of the element
##' @param applyIndex Which value should be trended
##' @param transfer Should the symbol be converted by the function
##' transferSymb.
##' @export
##' 

trendOnce = function(Num, Symb, applyIndex = 1:length(Num),
                     transfer = FALSE){
    value = c(NA, Num)
    symb = c(NA, Symb)
    newTrendIndex = intersect(applyIndex + 1,
        which(is.na(value) & symb %in% c("T", "C")))
    value[newTrendIndex] = value[newTrendIndex - 1]
    if(transfer){
        symb[newTrendIndex] = transferSymb(symb[newTrendIndex - 1])
    } else {
        symb[newTrendIndex] = "T"
    }
    trendedOnceValue = value[-1]
    trendOnceSymb = symb[-1]
    list(trendedOnceValue, trendOnceSymb)
}
