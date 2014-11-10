calculateEle31 = function(element31Num, element31Symb, inputNum,
    itemTypeCol, data){
    setnames(data,
             new = c("element31Num", "element31Symb", "inputNum"),
             old = c(element31Num, element31Symb, inputNum))
    ## NOTE (Michael): These are assumed from the names in Annex 3
    replaceIndex =
        which(data[[itemTypeCol]] %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                         28, 3, 32, 33, 5, 6, 7, 8, 9) &
              !is.na(inputNum) & 
              replaceable(data$element31Symb))
    print(replaceIndex)
    data[replaceIndex, 
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(inputNum, "C"))]
    replaceIndex = 
        which(data[[itemTypeCol]] %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                         28, 3, 32, 33, 5, 6, 7, 8, 9) &
              is.na(inputNum) & 
              replaceable(data$element31Symb))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element31Num", "element31Symb"), list(NA, "M"))]
    setnames(data,
             old = c("element31Num", "element31Symb", "inputNum"),
             new = c(element31Num, element31Symb, inputNum))    
}
