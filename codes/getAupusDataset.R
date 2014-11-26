##' Function to obtain all required data for the AUPUS module
##'
##' @param assignGlobal Whether the data should be assigned globally
##' or returned as a list. Default to TRUE, and assigned globally.
##' @export

getAupusDataset = function(assignGlobal = TRUE){
    ## Get aupus data
    ## ----------------------------------------------------------------
    aupusData = getAupusData(database = "new")


    ## Get input from processing data
    ## ----------------------------------------------------------------

    inputData = getInputFromProcessData(database = "new")

    ## Get ratio data
    ## ----------------------------------------------------------------

    ratioData = getRatioData(database = "new")

    ## Get share data
    ## ----------------------------------------------------------------

    shareData =
        collapseShare(
            shareData = getShareData(database = "new"),
            shares = "Value_share", verbose = FALSE)
    

    ## Get balancing item
    ##
    ## Note (Michael): This is just the flag of the 'ratio' table
    ## ----------------------------------------------------------------

    balanceElementData  =
        getBalanceElementData(database = "new")

    ## Get item information table
    ## ---------------------------------------------------------------
    itemInfoData = getItemInfoData()

    ## Get population data
    populationData = getPopulationData(database = "new")

    ## Get extraction rate
    extractionRateData =
        getExtractionRateData(aupusData = aupusData,
                              element41Num = "Value_measuredElementFS_41")
        

    ## Return the data
    dataList =
        list(aupusData = aupusData, inputData = inputData,
             ratioData = ratioData, shareData = shareData,
             balanceElementData = balanceElementData,
             itemInfoData = itemInfoData, populationData = populationData,
             extractionRateData = extractionRateData)
    
    if(assignGlobal){
        lapply(names(dataList), FUN = function(x)
            assign(x, dataList[[x]], envir = .GlobalEnv))
        invisible(dataList)
    } else {
        return(dataList)
    }
}


