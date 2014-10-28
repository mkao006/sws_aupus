updateInputFromProcess = function(aupus, share, input, element131Num){
    available = aupus[, c(key(aupus), element131Num), with = FALSE]
    setnames(available, old = element131Num, new = "element131Num")
    ## inputShare = Reduce(f = function(x, y){
    ##     merge(x, y, all = TRUE, allow.cartesian = TRUE,
    ##           by = intersect(colnames(x), colnames(y)))
    ## }, x = share, init = available)
    inputShare = merge(available, share, all = TRUE,
        allow.cartesian = TRUE,
        by = intersect(colnames(available), colnames(share)))
    setnames(inputShare,
             old = c("itemCode", "itemChildCode"),
             new = c("itemParentCode", "itemCode"))
    setkeyv(x = inputShare,
            cols = c("areaCode", "itemParentCode", "itemCode", "Year"))
    availableForInput = inputShare[!is.na(itemCode) & !is.na(Year), ]
    ## print(str(availableForInput))
    ## print(str(input))
    newInput = merge(availableForInput, input, all = TRUE)
    newInput[is.na(NUM_INPUT),
                     NUM_INPUT := element131Num * SHARE/100]
    newInput[!is.na(NUM_INPUT) & SYMB_INPUT == "M", SYMB_INPUT := "C"]
    newInput[, c("SHARE", "element131Num") := NULL]
    newInput
}
