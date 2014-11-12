##' Function to merge the share data to the aupus data
##'
##' @param shareData The shares data obtained from the function
##' getShare.
##' @param shares The column correspond to shares.
##' @param aupus The aupus data
##' @param verbos Whether the output should be printed.
##' @export
##' 

mergeShare = function(shareData, aupus, shares, verbose = FALSE){
    uniquePath =
        unique.data.frame(Reduce(rbind,
                                 lapply(shareData, FUN = function(x)
                                     x[, list(itemCode, itemChildCode)]))
                          )
    uniqueYear = unique(c(shareData[[1]][[key(aupus)[3]]],
        unique(aupus[[key(aupus)[3]]])))
    uniqueArea = unique(c(shareData[[1]][[key(aupus)[1]]],
        unique(aupus[[key(aupus)[1]]])))
    tmp = lapply(uniquePath, rep, times = length(uniqueYear))
    tmp[[key(aupus)[3]]] = rep(uniqueYear, each = NROW(uniquePath))
    tmp[[key(aupus)[1]]] = uniqueArea
    tmp[[shares]] = as.numeric(NA)
    finalBase = as.data.table(tmp)
    setkeyv(finalBase,
            c("areaCode", "itemCode", "itemChildCode", "Year"))
    for(i in 1:length(shareData)){
        wildCardFill(finalBase, shareData[[i]], shares, verbose)
    }
    finalBase[finalBase[[shares]] != 0, ]
}
