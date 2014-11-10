calculateEle58 = function(element58Num, element58Symb, itemTypeCol,
    data){
    setnames(data,
             old = c(element58Num, element58Symb),
             new = c("element58Num", "element58Symb"))
    calcValue =
        unlist(data[data[[key(data)[2]]] == 3158, element58Num]) +
        unlist(data[data[[key(data)[2]]] == 3159, element58Num])
    if(length(calcValue) != 0){
        replaceIndex = which(data[[itemTypeCol]] == 57 &
                                 replaceable(data$element58Symb))
        print(replaceIndex)
        data[replaceIndex,
             `:=`(c("element58Num", "element58Symb"),
                  appendSymbol(calcValue, "C"))]
    }
    setnames(data,
             new = c(element58Num, element58Symb),
             old = c("element58Num", "element58Symb"))
}

