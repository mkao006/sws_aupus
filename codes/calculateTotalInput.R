##' This function calculates total input
##'
##' @param inputData The data containing the input extracted from the
##' function getInputFromProcess after running the
##' updateInputFromProcess function.
##' @export
##' 

calculateTotalInput = function(inputData, inputNum, param){
    newKey = with(param$keyNames,
        c(countryName, itemChildName, yearName))
    evalString = paste0("list(", inputNum, " = sum(", inputNum, "))")
    aggregatedInput =
        inputData[, eval(parse(text = evalString)),
                  by = newKey]
    setkeyv(aggregatedInput, newKey)
    setnames(aggregatedInput,
             old = param$keyNames$itemChildName,
             new = param$keyNames$itemName)
    aggregatedInput
}
