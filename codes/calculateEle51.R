calculateEle51 = function(element51Num, element51Symb, element58Num,
    itemTypeCol, data){
    setnames(data,
             old = c(element51Num, element51Symb, element58Num),
             new = c("element51Num", "element51Symb", "element58Num"))
             
    data[data[[itemTypeCol]] %in% c(55, 56) & !is.na(element58Num),
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element58Num, "C"))]
    data[data[[key(data)[2]]] == 3183,
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(sum(data[data[[key(data)[2]]] %in%
                                    c(3158, 3159), element51Num]), "C"))]
    setnames(data,
             new = c(element51Num, element51Symb, element58Num),
             old = c("element51Num", "element51Symb", "element58Num"))
}
