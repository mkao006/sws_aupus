##' This function calculates total input
##'
##' @param inputData The data containing the input extracted from the
##' function getInputFromProcess after running the
##' updateInputFromProcess function.
##' @export
##' 

calculateTotalInput = function(inputData){
    newKey = c("areaCode", "itemCode", "Year")
    aggregatedInput =
        inputData[, list(NUM_TOTAL_INPUT = sum(NUM_INPUT)),
              by = newKey]
    setkeyv(aggregatedInput, newKey)
    aggregatedInput
}
