##' Function to collapse the share data
##'
##' @param shareData The shares data obtained from the function
##' getShare.
##' @param shares The column correspond to shares.
##' @param verbos Whether the output should be printed.
##' @export
##' 

collapseShare = function (shareData, shares, aupusParam, verbose = FALSE){
    uniquePath = unique.data.frame(Reduce(rbind, lapply(shareData, 
        FUN = function(x){
            x[, c(aupusParam$keyNames$itemParentName, 
                  aupusParam$keyNames$itemChildName),
              with = FALSE]
        })))
    uniqueYear = as.numeric(aupusParam$year)
    uniqueArea = aupusParam$areaCode
    tmp = lapply(uniquePath, rep, times = length(uniqueYear))
    tmp[[aupusParam$keyNames$yearName]] = rep(uniqueYear, each = NROW(uniquePath))
    tmp[[aupusParam$keyNames$areaName]] = uniqueArea
    tmp[[shares]] = as.numeric(NA)
    finalBase = as.data.table(tmp)
    setkeyv(finalBase, key(shareData[[1]]))
    for(i in 1:length(shareData)) {
        wildCardFill(finalBase, shareData[[i]], shares, verbose)
    }
    setkeyv(finalBase, key(shareData[[1]]))
    finalBase[, `:=`(timePointYearsSP, as.numeric(timePointYearsSP))]
    finalBase[finalBase[[shares]] != 0, ]
}
