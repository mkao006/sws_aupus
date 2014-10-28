appendBalanceElement = function(aupus, balanceElement, verbose = FALSE){
    base = merge(aupus, balanceElement[[1]], all.x = TRUE)
    ## Fill in wild card
    for(i in 1:length(balanceElement)){
        wildCardFill(base, balanceElement[[i]], "balanceElement", verbose)
    }
    base
}
