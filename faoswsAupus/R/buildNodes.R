##' Function to merge aupus, ratio, balance element, item info and
##' whether to use AUPUS data.
##'
##' @param aupusData The aupus data from the function getAupus
##' @param ratioData The ratio data from the function getRatio
##' @param balanceElementData The balance element data from the
##' function getBalanceElement.
##' @param itemInfoData The item information data from the function
##' getItemInfo.
##' @param balanceElementNum The column name which corresponds to
##' balance element.
##'
##' @export
##' 

buildNodes = function(aupusData, ratioData, balanceElementData, itemInfoData,
    balanceElementNum, populationData, verbose = FALSE){
    aupusWithInfo = mergeItemInfo(aupusData, itemInfoData)
    aupusWithInfoRatio = mergeRatio(aupusData = aupusWithInfo, 
        ratioData = ratioData, verbose = varbose)
    setkeyv(aupusWithInfoRatio, key(aupusData))
    aupusFinal = mergeBalanceElement(aupus = aupusWithInfoRatio, 
        balanceElementData = balanceElementData,
        balanceElementNum = balanceElementNum, 
        verbose = verbose)

    okey = key(aupusFinal)
    setkeyv(aupusFinal, key(populationData))
    aupusFinal[populationData,
                     `:=`(c("Value_population_11", "Value_population_21"),
                          list(i.Value_population_11,
                               i.Value_population_21))]
    setkeyv(aupusFinal, key(aupusData))
    aupusFinal  
}
