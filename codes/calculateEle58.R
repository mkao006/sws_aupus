calculateEle58 = function(element58Num, element58Symb, data){
    setnames(data,
             old = c(element58Num, element58Symb),
             new = c("element58Num", "element58Symb"))
    calcValue = data[itemCode == 3158, element58Num] +
        data[itemCode == 3158, element58Num]
    if(length(calcValue) != 0){
        data[itemType == 57, element58Num := calcValue]
        data[itemType == 57 & element58Symb != "M",
             elementSymb := "C"]
    }
    setnames(data,
             new = c(element58Num, element58Symb),
             old = c("element58Num", "element58Symb"))
}

