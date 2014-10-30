calculateEle58 = function(element58Num, element58Symb, data){
    setnames(data,
             old = c(element58Num, element58Symb),
             new = c("element58Num", "element58Symb"))
    calcValue = unlist(data[itemCode == 3158, element58Num]) +
        unlist(data[itemCode == 3159, element58Num])
    if(length(calcValue) != 0)
        data[itemType == 57,
             `:=`(c("element58Num", "element58Symb"),
                  list(calcValue, "C"))]
    setnames(data,
             new = c(element58Num, element58Symb),
             old = c("element58Num", "element58Symb"))
}

