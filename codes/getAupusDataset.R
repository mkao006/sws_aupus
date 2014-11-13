##' Function to obtain all required data for the AUPUS module
##'
##' @param param The parameter obtained from the getAupusParam
##' function.
##' @param assignGlobal Whether the data should be assigned globally
##' or returned as a list. Default to TRUE, and assigned globally.
##' @export

getAupusDataset = function(param, assignGlobal = TRUE){
    ## Get aupus data
    ## ----------------------------------------------------------------
    aupusData = getAupusData(param = param, database = "new")


    ## Get input from processing data
    ## ----------------------------------------------------------------

    inputData = getInputFromProcessData(database = "new", param = param)

    ## Get ratio data
    ## ----------------------------------------------------------------

    ratioData = getRatioData(database = "new", param = param)

    ## Get share data
    ## ----------------------------------------------------------------

    shareData =
        collapseShare(
            shareData = getShareData(database = "new", param = param),
            shares = "Value_share", param = param, verbose = FALSE)
            

    ## Get balancing item
    ##
    ## Note (Michael): This is just the flag of the 'ratio' table
    ## ----------------------------------------------------------------

    balanceElementData  =
        getBalanceElementData(database = "new", param = param)

    ## Get item information table
    ## ---------------------------------------------------------------
    itemInfoData = getItemInfoData()
        

    ## Return the data
    dataList = list(aupusData = aupusData, inputData = inputData,
        ratioData = ratioData, shareData = shareData,
        balanceElementData = balanceElementData,
        itemInfoData = itemInfoData)
    
    if(assignGlobal){
        lapply(names(dataList), FUN = function(x)
            assign(x, dataList[[x]], envir = .GlobalEnv))
        invisible(dataList)
    } else {
        return(dataList)
    }
}


