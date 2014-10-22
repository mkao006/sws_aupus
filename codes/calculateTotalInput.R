calculateTotalInput = function(input){
    newKey = c("areaCode", "itemCode", "Year")
    aggregatedInput =
        input[, list(NUM_TOTAL_INPUT = sum(NUM_INPUT)),
              by = newKey]
    setkeyv(aggregatedInput, newKey)
    aggregatedInput
}
