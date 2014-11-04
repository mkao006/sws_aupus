calculateEle21 = function(element21Num, element21Symb, element11Num,
    element111Num, ratio171Num, itemTypeCol, data){
    setnames(data,
             old = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             new = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))
    ## If the item was population then copy from element 11
    data[data[[key(data)[2]]] == 1,
         `:=`(c("element21Num", "element21Symb"),
              list(element11Num, "/"))]
    
    ## NOTE (Michael): This formula is derived from the formula of
    ##                 element 111 which has the reverse calculation.
    data[data[[itemTypeCol]] %in% c(2, 3, 9, 29, 30),
         `:=`(c("element21Num", "element21Symb"),
              appendSymbol(element111Num * 1000/ratio171Num, "C"))]
    setnames(data,
             new = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             old = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))    
}
