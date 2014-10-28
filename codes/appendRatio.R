appendRatio = function(aupus, ratio, verbose = FALSE){
    base = merge(aupus, ratio[[1]], all.x = TRUE)
    ## Fill in wild card
    for(i in 2:length(ratio)){
        lapply(grep("RATIO", colnames(ratio[[i]]), value = TRUE),
               FUN = function(x) wildCardFill(base, ratio[[i]], x,
                                              verbose = verbose))
    }
    base
}
