##' Function to merge shares and extraction rate and build the
##' relation graph.
##'
##' @param aupusData The aupus data from the function getAupus
##' @param extractionRate The column name corresponding to extaction rate
##' @param shareData The share data from the function getShare
##' @param param The parameter from the function getAupusParem
##'
##' @export


## NOTE (Michael): The extraction rate should only apply to process
##                 commodity.
buildEdgeRelation = function(aupusData, extractionRate, shareData,  param){
    extractionRateData = aupusData[, c(key(aupusData), extractionRate),
        with = FALSE]
    setnames(extractionRateData,
             old = param$keyNames$itemName,
             new = param$keyNames$itemChildName)
    edgeData =
        merge(shareData, extractionRateData,
              by = intersect(colnames(shareData), colnames(extractionRateData)),
              all = FALSE)
    setnames(edgeData, extractionRate, "Value_extraction")
    setkeyv(edgeData, key(shareData))
    edgeData
}
