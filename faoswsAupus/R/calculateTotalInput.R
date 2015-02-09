##' This function calculates total input
##'
##' @param inputData The data containing the input extracted from the
##' function getInputFromProcess after running the
##' updateInputFromProcess function.
##' @export
##' 

calculateTotalInput = function(inputData, inputNum, aupusParam){
    newKey = with(aupusParam$keyNames,
        c(countryName, itemChildName, yearName))
    evalString = paste0("list(", inputNum, " = sum(", inputNum, "))")
    aggregatedInput =
        inputData[, eval(parse(text = evalString)),
                  by = newKey]
    setkeyv(aggregatedInput, newKey)
    setnames(aggregatedInput,
             old = aupusParam$keyNames$itemChildName,
             new = aupusParam$keyNames$itemName)
    aggregatedInput
}
