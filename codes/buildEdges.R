##' Function to merge shares and extraction rate and build the
##' relation graph.
##'
##' @param aupusData The aupus data from the function getAupus
##' @param extractionRate The column name corresponding to extaction rate
##' @param shareData The share data from the function getShare
##' @param inputData The input data returned by the function getInputData
##'
##' @export
##' 


## NOTE (Michael): The extraction rate should only apply to process
##                 commodity.

buildEdges = function(extractionRateData, shareData, inputData){
    ## extractionRateData = aupusData[, c(key(aupusData), extractionRate),
    ##     with = FALSE]
    ## setnames(extractionRateData,
    ##          old = param$keyNames$itemName,
    ##          new = param$keyNames$itemChildName)
    
    ## TODO (Michael): Need to check this, if there are no extraction
    ##                 rates then probably the defaults are filled
    ##                 in. Also need to check how the input from
    ##                 processing data base is built.
    edgeData =
        Reduce(function(x, y){
            merge(x, y, all = FALSE, by = intersect(colnames(x), colnames(y)))
          }, x = list(shareData, extractionRateData, inputData))
    ## edgeData =
    ##     merge(shareData, extractionRateData,
    ##           by = intersect(colnames(shareData), colnames(extractionRateData)),
    ##           all = FALSE)
    ## setnames(edgeData, extractionRate, "Value_extraction")
    setkeyv(edgeData, key(inputData))
    edgeData
}
