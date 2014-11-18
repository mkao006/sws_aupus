##' The function re-calculates the input from processing
##'
##' @param aupusData The aupus data from the function getAupus
##' @param shareData The shares data from the function getShares
##' @param inputData The input data from the function
##' getInputFromProcessing.
##' @param sharesNum The column corresponding to shares value.
##' @param inputNum The column corresponding to input values.
##' @param inputSymb The column corresponding to input symbol.
##' @param element131Num The column corresponding to the value of
##' element 131.
##' @export
##' 

updateInputFromProcess = function(aupusData, shareData, inputData,
    sharesNum, inputNum, inputSymb, element131Num){
    available = aupusData[, c(key(aupusData), element131Num),
        with = FALSE]
    setnames(available, old = element131Num, new = "element131Num")
    ## inputShare = Reduce(f = function(x, y){
    ##     merge(x, y, all = TRUE, allow.cartesian = TRUE,
    ##           by = intersect(colnames(x), colnames(y)))
    ## }, x = share, init = available)
    inputShare = merge(available, shareData, all = TRUE,
        allow.cartesian = TRUE, by = key(shareData))
    

    
    setkeyv(x = inputShare,
            cols = c("areaCode", "itemParentCode", "itemCode", "Year"))
    availableForInput = inputShare[!is.na(itemCode) & !is.na(Year), ]
    ## print(str(availableForInput))
    ## print(str(input))
    newInput = merge(availableForInput, inputData, all = TRUE)
    setnames(newInput,
             old = c(sharesNum, inputNum, inputSymb),
             new = c("sharesNum", "inputNum", "inputSymb"))
    newInput[replaceable(inputSymb),
             `:=`(c("inputNum", "inputSymb"),
                  appendSymbol(element131Num * sharesNum/100, "C"))]
    newInput[, element131Num := NULL]
    setnames(newInput,
             new = c(sharesNum, inputNum, inputSymb),
             old = c("sharesNum", "inputNum","inputSymb"))
    newInput
}
