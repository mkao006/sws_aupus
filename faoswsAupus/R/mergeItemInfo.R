##' Function to merge item info to the data
##'
##' @param aupusData Data returned from the function getAupusData
##' @param itemInfoData Data returned from the function
##' getItemInfoData.
##' @export
##' 

mergeItemInfo = function(aupusData, itemInfoData){
    aupusItemInfo = merge(aupusData, itemInfoData, all.x = TRUE)
    setkeyv(aupusItemInfo, key(aupusData))
    aupusItemInfo
}
