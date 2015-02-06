##' This function gets all the aupusParameter in order to query the data
##'
##' @param areaCode The country code of the country of interest
##' @param assignGlobal logical, default to FALSE, where a list is
##' returned. If TRUE, then the result will be assigned globally.
##' @export
##' 

getAupusParameter = function(areaCode, assignGlobal = TRUE){

    ## Get all item Code
    ## --------------------------------------------------------------
    itemCodeList =
        GetCodeList(domain = "faostat_one",
                    dataset = "FS1_SUA",
                    dimension = "measuredItemFS")
    allItemCodes = itemCodeList[type != 0, code]

    ## Get all year
    ## --------------------------------------------------------------
    ## NOTE (Michael): This is temporary just to speed up the computation
    allYearCodes = swsContext.datasets[[1]]@dimensions$timePointYears@keys
    ## allYearCodes = as.character(2005:2013)
    ## yearCodeList = GetCodeList(domain = "faostat_one",
    ##     dataset = "FS1_SUA",
    ##     dimension = "timePointYears")
    ## allYearCodes = yearCodeList[code != 0, code]


    ## Get all aupus element
    ## --------------------------------------------------------------
    ## TODO (Michael): Need to add back element 541 and 546, when Nick
    ##                 import then into the data base.
    allElementCodes =
        as.character(c(11, 21, 31, 41, 51, 58, 61, 62, 63, 66, 71, 91,
                       92, 93, 95, 96, 101, 111, 121, 131, 141, 144,
                       151, 161, 171, 174, 181, 191, 261, 264, 271,
                       274, 281, 284))

    ## Set key names
    ## --------------------------------------------------------------
    keyNames =
        list(areaName = "geographicAreaFS",
             itemName = "measuredItemFS",
             itemParentName = "measuredItemParentFS",
             itemChildName = "measuredItemChildFS",
             itemTypeName = "measuredItemTypeFS",
             elementName = "measuredElementFS",
             extractionRateName = "Value_extractionRate",
             balanceElementName = "Value_balanceElement",
             inputName = "Value_input",
             shareName = "Value_share",
             yearName = "timePointYearsSP",
             valuePrefix = "Value_",
             flagPrefix = "flagFaostat_",
             ratioPrefix = "Ratio_")

        
    aupusParameters =
        list(areaCode = areaCode,
             itemCode = allItemCodes, elementCode = allElementCodes,
             year = allYearCodes, keyNames = keyNames)
    
    if(assignGlobal){
        lapply(names(aupusParameters), FUN = function(x)
            assign(x, aupusParameters[[x]], envir = .GlobalEnv))
        invisible(aupusParameters)
    } else {
        return(aupusParameters)
    }
}
