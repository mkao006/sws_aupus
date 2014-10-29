mergeShare = function(share, aupus){
    uniquePath =
        unique.data.frame(Reduce(rbind,
                                 lapply(share, FUN = function(x)
                                     x[, list(itemCode, itemChildCode)]))
                          )
    uniqueYear = unique(c(share[[1]]$Year, unique(aupus$Year)))
    uniqueArea = unique(c(share[[1]]$areaCode, unique(aupus$areaCode)))
    tmp = lapply(uniquePath, rep, times = length(uniqueYear))
    tmp$Year = rep(uniqueYear, each = NROW(uniquePath))
    tmp$areaCode = uniqueArea
    tmp$SHARE = as.numeric(NA)
    finalBase = as.data.table(tmp)
    setkeyv(finalBase,
            c("areaCode", "itemCode", "itemChildCode", "Year"))
    for(i in 1:length(share)){
        wildCardFill(finalBase, share[[i]], "SHARE", TRUE)
    }
    finalBase[SHARE != 0, ]
    ## finalBase
}
