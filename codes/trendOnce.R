trendOnce = function(Num, Symb, applyIndex = 1:length(Num),
                     transfer = FALSE){
    value = c(NA, Num)
    symb = c(NA, Symb)
    newTrendIndex = intersect(applyIndex + 1, which(is.na(value)))
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
