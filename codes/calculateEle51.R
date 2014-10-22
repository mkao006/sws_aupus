calculateEle51 = function(element51Num, element51Symb, element58Num,
    data){
    setnames(data,
             old = c(element51Num, element51Symb, element58Num),
             new = c("element51Num", "element51Symb", "element58Num"))
             
    data[itemType %in% c(55, 56) & !is.na(element58Num),
         element51Num := element58Num]
    data[itemCode == 3183,
         element51Num :=
             sum(data[itemCode %in% c(3158, 3159), element51Num])]
    setnames(data,
             new = c(element51Num, element51Symb, element58Num),
             old = c("element51Num", "element51Symb", "element58Num"))
}
