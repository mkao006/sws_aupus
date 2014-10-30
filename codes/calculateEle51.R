calculateEle51 = function(element51Num, element51Symb, element58Num,
    data){
    setnames(data,
             old = c(element51Num, element51Symb, element58Num),
             new = c("element51Num", "element51Symb", "element58Num"))
             
    data[itemType %in% c(55, 56) & !is.na(element58Num),
         `:=`(c("element51Num", "element51Symb"),
              list(element58Num, "C"))]
    data[itemCode == 3183,
         `:=`(c("element51Num", "element51Symb"),
              list(sum(data[itemCode %in% c(3158, 3159),
                            element51Num]), "C"))]
    setnames(data,
             new = c(element51Num, element51Symb, element58Num),
             old = c("element51Num", "element51Symb", "element58Num"))
}
