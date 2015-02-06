##' Function to obtain all required data for the AUPUS module
##'
##' @param assignGlobal Whether the data should be assigned globally
##' or returned as a list. Default to TRUE, and assigned globally.
##' @export

getAupusDataset = function(assignGlobal = TRUE, aupusParam){
    ## Get aupus data
    ## ----------------------------------------------------------------
    aupusData = getAupusData(database = "new", aupusParam = aupusParam)


    ## Get input from processing data
    ## ----------------------------------------------------------------

    inputData = getInputFromProcessData(database = "new", aupusParam = aupusParam)

    ## Get ratio data
    ## ----------------------------------------------------------------

    ratioData = getRatioData(database = "new", aupusParam = aupusParam)

    ## Get share data
    ## ----------------------------------------------------------------

    shareData =
        collapseShare(
            shareData = getShareData(database = "new", aupusParam = aupusParam),
            shares = "Value_share", verbose = FALSE)
    

    ## Get balancing item
    ##
    ## Note (Michael): This is just the flag of the 'ratio' table
    ## ----------------------------------------------------------------

    balanceElementData  =
        getBalanceElementData(database = "new", aupusParam = aupusParam)

    ## Get item information table
    ## ---------------------------------------------------------------
    ## NOTE (Michael): This function only works for the new data base
    itemInfoData = getItemInfoData()

    ## Get population data
    populationData = getPopulationData(database = "new", aupusParam = aupusParam)

    ## Get extraction rate
    ## ---------------------------------------------------------------
    ##
    ## NOTE (Michael): This function only works for the new data base
    ##                 as well, since there is restructing of the
    ##                 data.
    extractionRateData =
        getExtractionRateData(database = "new", aupusParam = aupusParam)
        

    ## Return the data
    dataset =
        list(aupusData = aupusData, inputData = inputData,
             ratioData = ratioData, shareData = shareData,
             balanceElementData = balanceElementData,
             itemInfoData = itemInfoData, populationData = populationData,
             extractionRateData = extractionRateData)
    
    if(assignGlobal){
        lapply(names(dataset), FUN = function(x)
            assign(x, dataset[[x]], envir = .GlobalEnv))
        invisible(dataset)
    } else {
        return(dataset)
    }
}


