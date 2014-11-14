##' Function to merge all the data to obtain the final data for
##' processing aupus
##'
##' @param aupusData The data obtained from the function getAupusData
##' @param itemInfoData The data obtained from the function
##' getItemInfoData.
##' @param balanceElementData The data obtained from the
##' getBalanceElemetData.
##' @param shareData The data obtained from the getShareData
##' @param inputData the data obtained from getInputData.
##' @param param The param set by getAupusParam
##' @param inputNum The column corresponding to input value in the
##' inputData.
##' @param balanceNum The column corresponding to balance element in
##' the balanceElementData.
##' @param share The column corresponding to shares in the shareData.
##' @param element131Num The column correspond to element 131 in the
##' aupusData.
##' @export
##' 

mergeAll = function(aupusData, itemInfoData, balanceElementData,
    shareData, inputData, param, inputNum = "Value_input",
    balanceElementNum = "balanceElement", shares = "Value_share",
    element131Num){
    ## updatedInput =
    ##     updateInputFromProcess(aupusData = aupusData,
    ##                            shareData = shareData,
    ##                            inputData = inputData,
    ##                            sharesNum = "Value_share",
    ##                            inputNum = "Value_input",
    ##                            inputSymb = "flagFaostat_input",
    ##                            element131Num = element131Num)
    ## aggregatedInput = calculateTotalInput(updatedInput)
    aggregatedInput = calculateTotalInput(inputData = inputData,
        inputNum = inputNum, param = param)
    aupusWithInfo = mergeItemInfo(aupusData, itemInfoData)
    aupusWithInfoRatio =
        mergeRatio(aupusData = aupusWithInfo,
        ratioData = ratioData, verbose = varbose)
    setkeyv(aupusWithInfoRatio, key(aupusData))
    aupusWithInfoRatioBalance =
        mergeBalanceElement(aupus = aupusWithInfoRatio,
                            balanceElementData = balanceElementData,
                            balanceElementNum = balanceElementNum,
                            verbose = FALSE)
    aupusFinal = merge(aupusWithInfoRatioBalance, aggregatedInput,
        all.x = TRUE)
    setkeyv(aupusFinal, key(aupusData))
    aupusFinal
}
