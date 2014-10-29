calculateBalance = function(supply, utilization, element161Num,
    element171Num, element181Num, element181Symb, balanceElement, data){
    setnames(data,
             old = c(supply, utilization, element161Num, element171Num,
                     element181Num, element181Symb, balanceElement),
             new = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "element181Symb",
                 "balanceElement"))
    data[!itemType %in% c(04, 15, 16, 20, 21, 25, 32, 33, 37, 49,
                          50, 55, 56, 57),
         BALANCE := supply - utilization]
    data[itemType == 57 & !is.na(element161Num) & !is.na(element171Num),
         BALANCE := element161Num/element171Num * 1000]
    data[itemType == 57 & (is.na(element161Num) | is.na(element171Num)),
         BALANCE := 0]

    data[balanceElement == 71, BALANCE := -BALANCE]
    
    fillBalance = function(balanceValue, balanceNum, balanceSymb,
        element181Num, element181Symb, data){
        ## Replace if the balancing element is calculated or missing
        ## and calculated balancing value is greater than zero
        if(balanceNum == "NUM_181"){
            balanceNum = element181Num
            balanceSymb = element181Symb
        }
        
        toReplace =
            unlist(data[, balanceSymb, with = FALSE]) %in% c("M", "C") &
            unlist(data[, balanceValue, with = FALSE]) > 0
        toReplace[is.na(toReplace)] = FALSE
        originalValue = unlist(data[, balanceNum, with = FALSE])
        originalValue[toReplace] =
            unlist(data[toReplace, balanceValue, with = FALSE])
        originalSymb = unlist(data[, balanceSymb, with = FALSE]) 
        originalSymb[toReplace] = "C"
        discrepancyValue = unlist(data[, element181Num, with = FALSE])
        discrepancyValue[toReplace] = 0
        discrepancySymb = unlist(data[, element181Symb, with = FALSE])
        discrepancySymb[toReplace] = "M"

        ## If neither is satisfied, then set the balancing element to
        ## 0 and the calculated balancing value to statistical
        ## discrepancy.
        originalValue[!toReplace] = 0
        originalSymb[!toReplace] = "M"
        discrepancyValue[!toReplace] =
            unlist(data[!toReplace, balanceValue, with = FALSE])
        discrepancySymb[!toReplace] = "C"
        list(originalValue, originalSymb, discrepancyValue,
             discrepancySymb)
    }
    data[!is.na(balanceElement),
         `:=`(c(paste0("NUM_", .BY[[1]]), paste0("SYMB_", .BY[[1]]),
                "element181Num", "element181Symb"),
              fillBalance("BALANCE", paste0("NUM_", .BY[[1]]),
                          paste0("SYMB_", .BY[[1]]),
                          "element181Num", "element181Symb", .SD)),
         by = "balanceElement"]
    setnames(data,
             new = c(supply, utilization, element161Num, element171Num,
                     element181Num, element181Symb, balanceElement),
             old = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "element181Symb",
                 "balanceElement"))    

}
