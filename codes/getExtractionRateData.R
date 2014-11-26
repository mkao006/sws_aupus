##' This function extracts the extraction rate data from the aupus data
##'
##'
##' @param aupusData The data returned from the function getAupusData
##' @param element41Num The column name corresponding to element 41 in
##' the aupus data.
##' @export
##' 


getExtractionRateData = function(aupusData, element41Num){
    ## This is to get the extraction rate data for the edge
    extractionRateData =
        aupusData[, c(key(aupusData), element41Num), with = FALSE]
    setnames(x = extractionRateData,
             old = c("measuredItemFS", element41Num),
             new = c("measuredItemChildFS", "Value_extraction"))
    setkeyv(extractionRateData,
            cols = c("geographicAreaFS", "measuredItemChildFS", "timePointYearsSP"))
    extractionRateData
}
