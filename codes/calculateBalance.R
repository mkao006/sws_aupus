calculateBalance = function(supply, utilization, element161Num,
    element171Num, element181Num, balanceElement, data){
    setnames(data,
             old = c(supply, utilization, element161Num, element171Num,
                     element181Num, balanceElement),
             new = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "balanceElement"))
    data[!itemType %in% c(04, 15, 16, 20, 21, 25, 32, 33, 37, 49,
                          50, 55, 56, 57),
         BALANCE := supply - utilization]
    data[itemType == 57 & !is.na(element161Num) & !is.na(element171Num),
         BALANCE := element161Num/element171Num * 1000]
    data[itemType == 57 & (is.na(element161Num) | is.na(element171Num)),
         BALANCE := 0]

    data[balanceElement == 71, BALANCE := -BALANCE]
    data[BALANCE < 0, `:=`(c("BALANCE", "element181Num"),
                           list(0, BALANCE))]
    ## TODO (Michael): Rewrite this
    balanceText = paste0("NUM_", balanceElement, " := BALANCE")
    data[!is.na(balanceElement),
         eval(parse(text = balanceText)), by = "balanceElement"]
    setnames(data,
             new = c(supply, utilization, element161Num, element171Num,
                     element181Num, balanceElement),
             old = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "balanceElement"))    

}
